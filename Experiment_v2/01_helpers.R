## ══════════════════════════════════════════════════════════════════════════
## All reusable helper functions: small summary helpers, folder-name parsing,
## CSV reading, zone-bout extraction, zone -> zone latency, paired stats and
## the shared paired bar-plot / across-days plot engines, and the Parquet
## store: per-file processing, chow.csv handling, ingest, check_store(),
## store_query() and build_rf_features().
## ══════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════
# SMALL SUMMARY HELPERS
# ══════════════════════════════════════════════════════════════════════════

# sum / mean that return NA (not 0 / NaN) when there is nothing to summarise,
# so a column that doesn't exist in a file shows up as NA instead of a fake 0.
sum_or_na  <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
sem <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) NA_real_ else sd(x) / sqrt(length(x))
}

# Seconds spent in a 0/1 zone column: each sample is weighted by the time until
# the next sample (dt), so this is correct whatever the sampling rate is.
zone_seconds <- function(x, dt) {
  if (all(is.na(x))) return(NA_real_)
  sum((x != 0) * dt, na.rm = TRUE)
}

# ══════════════════════════════════════════════════════════════════════════
# FOLDER / FILE NAME PARSING
# ══════════════════════════════════════════════════════════════════════════

get_csvs <- function(folder) {
  list.files(folder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
}

# Folder name -> type / light / effort / diet / replicate / folder_day.
#
#   "Baseline"                          -> type "baseline"
#   "training" / "Training day 2"       -> type "training"
#   "low_effort_no_light_sucrose"       -> forage, effort low, light none, diet sucrose
#   "medium effort white light hfd 2"   -> forage, medium, white, hfd, replicate 2
#
# Forage names are parsed by keyword, so the order of the pieces doesn't matter:
#   effort = word before "effort", light = word before "light",
#   diet   = whatever is left over.
parse_condition <- function(folder_name) {

  nm <- str_to_lower(str_squish(str_replace_all(folder_name, "_", " ")))

  day_match  <- str_match(nm, "\\bday\\s*(\\d+)\\s*$")
  folder_day <- if (is.na(day_match[1, 2])) NA_integer_ else as.integer(day_match[1, 2])
  nm <- str_squish(str_remove(nm, "\\bday\\s*\\d+\\s*$"))

  if (str_detect(nm, "^baseline")) {
    return(tibble(type = "baseline", light = NA_character_, effort = NA_character_,
                  diet = NA_character_, replicate = 1L, folder_day = folder_day))
  }
  if (str_detect(nm, "^training")) {
    return(tibble(type = "training", light = NA_character_, effort = NA_character_,
                  diet = NA_character_, replicate = 1L, folder_day = folder_day))
  }

  replicate <- str_extract(nm, "\\d+$")
  replicate <- if (is.na(replicate)) 1L else as.integer(replicate)
  nm <- str_squish(str_remove(nm, "\\s*\\d+$"))

  effort    <- str_match(nm, "\\b(low|medium|high)\\s+effort\\b")[1, 2]
  light_raw <- str_match(nm, "\\b([a-z]+)\\s+light\\b")[1, 2]
  light     <- if (is.na(light_raw)) NA_character_ else
    unname(ifelse(light_raw %in% names(LIGHT_ALIASES), LIGHT_ALIASES[light_raw], light_raw))

  diet <- nm %>%
    str_remove(regex("\\b(low|medium|high)\\s+effort\\b")) %>%
    str_remove(regex("\\b[a-z]+\\s+light\\b")) %>%
    str_squish()
  diet <- if (diet == "") NA_character_ else str_replace_all(diet, " ", "_")

  tibble(type = "forage", light = light, effort = effort, diet = diet,
         replicate = replicate, folder_day = folder_day)
}

# "day N" / "dN" in the file name or in sub-folders below the condition folder
parse_file_day <- function(path, folder) {
  p <- str_replace_all(path,   "\\\\", "/")
  f <- str_replace_all(folder, "\\\\", "/")
  rel <- tools::file_path_sans_ext(str_remove(p, fixed(paste0(f, "/"))))
  hits <- str_match_all(rel, regex("(?<![a-z])(?:day|d)[ _-]?(\\d+)(?![0-9])", ignore_case = TRUE))
  map_int(hits, function(m) if (nrow(m) == 0) NA_integer_ else as.integer(tail(m[, 2], 1)))
}

arena_id <- function(filepath) {
  fname <- tools::file_path_sans_ext(basename(filepath))
  hit <- str_extract(fname, regex("arena[ _-]*\\d+[a-z]?(?=[^0-9a-z]|$)", ignore_case = TRUE))
  out <- str_to_upper(str_remove_all(hit, "[^A-Za-z0-9]"))
  # if the file name has no "arena N", fall back to the cleaned file name
  if_else(is.na(out), str_to_upper(str_remove_all(fname, "[^A-Za-z0-9]")), out)
}

drop_incomplete_day <- function(df) {
  if (n_distinct(df$day) > 1) filter(df, day < max(day)) else df
}

# ══════════════════════════════════════════════════════════════════════════
# CSV READING
# ══════════════════════════════════════════════════════════════════════════

# Number of lines to skip so that the column-name row is the first row read.
# Uses the "Number of header lines" entry on line 1 when present (column names
# are the second-to-last header line, units the last); otherwise looks for the
# row that starts with "Trial time"; otherwise falls back to HEADER_SKIP_FALLBACK.
find_header_skip <- function(filepath) {
  lines <- readLines(filepath, n = 300, warn = FALSE, skipNul = TRUE)
  lines <- iconv(lines, "UTF-8", "UTF-8", sub = "")
  is_names <- function(i) i >= 1 && i <= length(lines) &&
    str_detect(lines[i], regex("^[\\s\"\\x{FEFF}]*Trial time", ignore_case = TRUE))

  n_hdr <- suppressWarnings(as.integer(str_match(lines[1], "(?i)number of header lines\\D*(\\d+)")[1, 2]))
  if (!is.na(n_hdr) && is_names(n_hdr - 1)) return(n_hdr - 2L)

  idx <- which(map_lgl(seq_along(lines), is_names))
  if (length(idx) > 0) return(idx[1] - 1L)

  warning("Couldn't locate the 'Trial time' header row in ", basename(filepath),
          "; using HEADER_SKIP_FALLBACK = ", HEADER_SKIP_FALLBACK)
  HEADER_SKIP_FALLBACK
}

read_arena <- function(filepath, zt_offset) {

  raw <- read_csv(filepath, skip = find_header_skip(filepath),
                  col_types = cols(.default = col_character()),
                  show_col_types = FALSE, progress = FALSE)
  names(raw) <- str_squish(names(raw))

  # Drop the units row (first data row) if it isn't numeric
  if (nrow(raw) > 0 && "Trial time" %in% names(raw) &&
      is.na(suppressWarnings(as.numeric(raw[["Trial time"]][1])))) {
    raw <- raw[-1, ]
  }

  hit <- match(str_to_lower(names(COLUMN_MAP)), str_to_lower(names(raw)))
  if (is.na(hit[1])) stop("No 'Trial time' column in ", basename(filepath))

  missing_cols <- names(COLUMN_MAP)[is.na(hit)]
  if (length(missing_cols) > 0) {
    warning("Missing expected column(s) in ", basename(filepath), ": ",
            paste(missing_cols, collapse = ", "), " (filled with NA)", call. = FALSE)
  }

  out <- raw[, hit[!is.na(hit)], drop = FALSE]
  names(out) <- unname(COLUMN_MAP)[!is.na(hit)]
  for (nm in setdiff(unname(COLUMN_MAP), names(out))) out[[nm]] <- NA_character_

  out %>%
    mutate(across(everything(), ~ suppressWarnings(as.numeric(.x)))) %>%
    mutate(
      time_hr = trial_time / 3600,
      zt      = (time_hr + zt_offset) %% 24,
      zt_c    = ifelse(zt > 12, zt - 24, zt),
      day     = floor((time_hr + zt_offset) / 24),
      bin     = floor(trial_time / BIN_SIZE_SEC),
      # seconds until the next sample (last sample reuses the median step)
      dt      = { d <- c(diff(trial_time), NA_real_); d[is.na(d)] <- median(d, na.rm = TRUE); d }
    )
}

# ══════════════════════════════════════════════════════════════════════════
# ZONE BOUTS AND ZONE -> ZONE LATENCY
# ══════════════════════════════════════════════════════════════════════════

# Turns a 0/1 "in zone" column into one row per continuous bout, with the
# trial_time (seconds) at which the bout started and ended. A bout still open
# at the last row of the file is closed at the file's final trial_time.
extract_zone_bouts <- function(df, zone_col) {
  empty <- tibble(onset_time = numeric(0), offset_time = numeric(0))
  if (!zone_col %in% names(df) || nrow(df) == 0) return(empty)

  x <- df[[zone_col]]
  x <- ifelse(is.na(x), 0, x)
  x <- as.numeric(x != 0)

  prev <- dplyr::lag(x, default = 0)
  onset_idx  <- which(x == 1 & prev == 0)
  offset_idx <- which(x == 0 & prev == 1)
  if (length(onset_idx) > length(offset_idx)) offset_idx <- c(offset_idx, nrow(df))
  if (length(onset_idx) == 0) return(empty)

  tibble(onset_time = df$trial_time[onset_idx], offset_time = df$trial_time[offset_idx])
}

# Latency (s) from LEAVING from_col to the next ENTRY into to_col, same file.
compute_zone_latency <- function(df, from_col, to_col, mode = "all_exits") {
  from_b <- extract_zone_bouts(df, from_col)
  to_b   <- extract_zone_bouts(df, to_col)
  if (nrow(from_b) == 0 || nrow(to_b) == 0) return(tibble())

  to_onsets <- sort(to_b$onset_time)

  ev <- purrr::map_dfr(from_b$offset_time, function(exit_time) {
    nxt <- to_onsets[to_onsets > exit_time]
    if (length(nxt) == 0) return(NULL)
    tibble(from_exit_time = exit_time, to_entry_time = nxt[1], latency_sec = nxt[1] - exit_time)
  })
  if (nrow(ev) == 0) return(tibble())

  if (identical(mode, "last_exit")) {
    ev <- ev %>%
      group_by(to_entry_time) %>%
      slice_max(from_exit_time, n = 1, with_ties = FALSE) %>%
      ungroup()
  }
  ev
}

# Runs every pair in ZONE_LATENCY_PAIRS on one file's data. `day` = day within
# the recording at which the from-zone exit happened.
compute_zone_latencies <- function(df, zt_offset) {
  purrr::map_dfr(seq_len(nrow(ZONE_LATENCY_PAIRS)), function(i) {
    p  <- ZONE_LATENCY_PAIRS[i, ]
    ev <- compute_zone_latency(df, p$from_zone, p$to_zone, p$mode)
    if (nrow(ev) == 0) return(NULL)
    ev %>% mutate(pair = p$pair, day = floor((from_exit_time / 3600 + zt_offset) / 24))
  })
}

# ══════════════════════════════════════════════════════════════════════════
# PAIRED STATS + SHARED PLOT ENGINES
# ══════════════════════════════════════════════════════════════════════════

# Paired pairwise t-tests, Holm-adjusted (repeated measures)
paired_pairwise_t <- function(df, group_col, value_col, id_col) {
  wide <- df %>%
    select(all_of(c(id_col, group_col, value_col))) %>%
    pivot_wider(names_from = all_of(group_col), values_from = all_of(value_col))

  grps  <- setdiff(names(wide), id_col)
  pairs <- combn(grps, 2, simplify = FALSE)

  results <- map_dfr(pairs, function(p) {
    sub <- wide %>% select(all_of(c(id_col, p))) %>% drop_na()
    if (nrow(sub) < 2) {
      return(tibble(group1 = p[1], group2 = p[2], n_paired = nrow(sub), p.value = NA_real_))
    }
    tt <- t.test(sub[[p[1]]], sub[[p[2]]], paired = TRUE)
    tibble(group1 = p[1], group2 = p[2], n_paired = nrow(sub), p.value = tt$p.value)
  })

  results %>% mutate(p.adj = p.adjust(p.value, method = "holm"))
}

# One repeated-measures bar plot + stats, used for every group comparison.
#   stats_df  : long data, one row per id x group (duplicates are averaged)
#   group_col : column holding the groups being compared (make it a factor to
#               control the order)
#   id_col    : the repeated-measures unit (animal_id, ...)
# Prints: group summary, lmer (random intercept per id), paired Holm t-tests.
# Returns: the ggplot (only ids present in EVERY group are drawn).
paired_bar_plot <- function(stats_df, group_col, id_col, y_col, y_label,
                            title = NULL, x_label = NULL, fill_values = NULL) {

  stats_df <- stats_df %>%
    filter(is.finite(.data[[y_col]]), !is.na(.data[[group_col]])) %>%
    mutate(group = droplevels(factor(.data[[group_col]]))) %>%
    group_by(across(all_of(c(id_col, "group")))) %>%
    summarise(value = mean(.data[[y_col]]), .groups = "drop")
  names(stats_df)[names(stats_df) == "value"] <- y_col

  if (nrow(stats_df) == 0 || n_distinct(stats_df$group) < 2) {
    message("Skipping ", y_label, if (!is.null(title)) paste0(" (", title, ")"),
            ": fewer than 2 groups with data.")
    return(invisible(NULL))
  }

  cat("\n=============================\n")
  cat(y_label, if (!is.null(title)) paste0("- ", title), "\n")
  print(
    stats_df %>%
      group_by(group) %>%
      summarise(n_ids = n_distinct(.data[[id_col]]), mean = mean(.data[[y_col]]),
                sd = sd(.data[[y_col]]), .groups = "drop")
  )

  cat("\nRepeated-measures model (lmer,", id_col, "random intercept)\n")
  model <- tryCatch(
    lmer(reformulate(c("group", sprintf("(1 | %s)", id_col)), response = y_col), data = stats_df),
    error = function(e) { message("  lmer failed: ", conditionMessage(e)); NULL }
  )
  if (!is.null(model)) {
    print(anova(model))
    if (isSingular(model, tol = 1e-5)) cat("  (singular fit: a random-effect variance is ~0)\n")
  }

  cat("\nPaired, Holm-adjusted pairwise t tests (matched by", id_col, ")\n")
  print(paired_pairwise_t(stats_df, "group", y_col, id_col))

  complete_ids <- stats_df %>%
    count(across(all_of(c(id_col, "group")))) %>%
    count(across(all_of(id_col))) %>%
    filter(n == n_distinct(stats_df$group)) %>%
    pull(1)

  plot_df <- stats_df %>%
    filter(.data[[id_col]] %in% complete_ids) %>%
    arrange(.data[[id_col]], group)

  if (nrow(plot_df) == 0) {
    message("  No ", id_col, " has data in every group - nothing to plot for ", y_label)
    return(invisible(NULL))
  }

  comparisons <- if (length(complete_ids) >= 2) combn(levels(plot_df$group), 2, simplify = FALSE) else list()

  p <- ggplot(plot_df, aes(x = group, y = .data[[y_col]], fill = group)) +
    stat_summary(fun = mean, geom = "bar", width = .65, alpha = .7) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .15) +
    geom_line(aes(group = .data[[id_col]]), color = "grey60", alpha = .4) +
    geom_jitter(width = .08, size = 2, alpha = .75)

  if (length(comparisons) > 0) {
    p <- p + stat_compare_means(
      comparisons = comparisons, method = "t.test", paired = TRUE,
      p.adjust.method = "holm", label = "p.signif", step.increase = .10, size = 5
    )
  }
  if (!is.null(fill_values)) p <- p + scale_fill_manual(values = fill_values)

  p +
    labs(x = x_label, y = y_label, title = title) +
    theme_classic(base_size = 14) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 12),
          axis.title.y = element_text(size = 14))
}

# One line per animal across days + mean +/- SEM (red). Used for the training
# and baseline day-by-day plots.
plot_across_days <- function(df, y_col, y_label, id_col = "animal_id",
                             title = NULL, log_y = FALSE, x_label = "Day") {
  df <- df %>% filter(is.finite(.data[[y_col]]), is.finite(day_num))
  if (nrow(df) == 0) {
    message("Skipping ", y_label, ": no finite values.")
    return(invisible(NULL))
  }
  p <- ggplot(df, aes(x = day_num, y = .data[[y_col]])) +
    geom_line(aes(group = .data[[id_col]]), alpha = 0.3) +
    geom_point(alpha = 0.5, size = 1.8) +
    stat_summary(fun = mean, geom = "line", color = "firebrick", linewidth = 1.1) +
    stat_summary(fun.data = mean_se, geom = "errorbar", color = "firebrick", width = 0.15) +
    stat_summary(fun = mean, geom = "point", color = "firebrick", size = 2.8) +
    scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]))) +
    labs(x = x_label, y = y_label, title = title) +
    theme_classic(base_size = 12) +
    theme(panel.border = element_rect(fill = NA, color = "black"))
  if (log_y) p <- p + scale_y_log10()
  p
}

# Does the behaviour change across days? lmer(y ~ day + (1 | id)), prints anova.
day_effect_test <- function(df, y_col, id_col = "animal_id", label = y_col) {
  d <- df %>% filter(is.finite(.data[[y_col]]), is.finite(day_num)) %>%
    mutate(day_f = factor(day_num))
  if (n_distinct(d$day_f) < 2 || n_distinct(d[[id_col]]) < 2) return(invisible(NULL))
  cat("\n--- Effect of day on", label, "(lmer,", id_col, "random intercept) ---\n")
  m <- tryCatch(
    lmer(reformulate(c("day_f", sprintf("(1 | %s)", id_col)), response = y_col), data = d),
    error = function(e) { message("  lmer failed: ", conditionMessage(e)); NULL }
  )
  if (!is.null(m)) print(anova(m))
  invisible(m)
}


# ══════════════════════════════════════════════════════════════════════════
# READ ONE FILE -> DAILY ROWS + LATENCY EVENTS
# ══════════════════════════════════════════════════════════════════════════

# "Start time" value from the EthoVision header, or NA if the header has none.
# Stored for reference only until a real export confirms the field (see
# DATA_DECISIONS.md).
read_header_start <- function(filepath) {
  n <- suppressWarnings(find_header_skip(filepath))
  if (n <= 0) return(NA_character_)
  lines <- iconv(readLines(filepath, n = n, warn = FALSE, skipNul = TRUE), "UTF-8", "UTF-8", sub = "")
  hit <- str_subset(lines, regex("^[\\s\"\\x{FEFF}]*start time", ignore_case = TRUE))
  if (length(hit) == 0) return(NA_character_)
  val <- str_remove(hit[1], regex("^[\\s\"\\x{FEFF}]*start time\"?\\s*[,;:]?", ignore_case = TRUE))
  str_squish(str_remove_all(val, "\""))
}

# meta = one row of the file index (with id columns). Returns the daily rows
# (incl. peak 1-min wheel turns) and latency events for that file's complete days.
process_file <- function(meta) {
  zt_offset <- get_zt_offset(meta$path, meta$type, meta$light, meta$effort,
                             meta$diet, meta$file_day, meta$arena)
  df_full <- read_arena(meta$path, zt_offset)
  df_days <- drop_incomplete_day(df_full)

  # one row per day
  daily <- df_days %>%
    group_by(day) %>%
    summarise(
      wheel                = sum_or_na(wheel_cycles),
      pellets              = sum_or_na(pellets_earned),
      nose_pokes           = sum_or_na(nose_pokes),
      licks                = sum_or_na(licks),
      distance_moved       = sum_or_na(distance_moved),
      in_zone_wheel_time   = zone_seconds(in_zone_wheel,   dt),
      in_zone_pellet_time  = zone_seconds(in_zone_pellet,  dt),
      in_zone_cup_time     = zone_seconds(in_zone_cup,     dt),
      in_zone_hopper_time  = zone_seconds(in_zone_hopper,  dt),
      in_zone_shelter_time = zone_seconds(in_zone_shelter, dt),
      .groups = "drop"
    )

  # peak wheel turns in any 1-min bin, per day
  max_bin <- df_days %>%
    group_by(day, bin) %>%
    summarise(turns_in_bin = sum_or_na(wheel_cycles), .groups = "drop") %>%
    group_by(day) %>%
    summarise(max_turns_1min = if (all(is.na(turns_in_bin))) NA_real_ else max(turns_in_bin, na.rm = TRUE),
              .groups = "drop")

  daily <- daily %>%
    left_join(max_bin, by = "day") %>%
    mutate(day_num = meta$file_day + as.integer(day)) %>%
    select(-day)

  # zone -> zone latency events (only for the complete days kept above)
  lat <- compute_zone_latencies(df_full, zt_offset)
  if (nrow(lat) > 0) {
    lat <- lat %>%
      filter(day %in% unique(df_days$day)) %>%
      mutate(day_num = meta$file_day + as.integer(day)) %>%
      select(-day)
  }

  list(daily = daily, latency = lat, rows_read = nrow(df_full),
       total_pellets = sum_or_na(df_full$pellets_earned))
}

# ══════════════════════════════════════════════════════════════════════════
# INDEXING INCOMING FILES
# ══════════════════════════════════════════════════════════════════════════

CHOW_FILE <- "chow.csv"

# One row per EthoVision CSV in the condition folders under src_dir.
index_folder_files <- function(src_dir) {
  map_dfr(list.dirs(src_dir, recursive = FALSE), function(folder) {
    files <- get_csvs(folder)
    files <- files[basename(files) != CHOW_FILE]
    if (length(files) == 0) return(NULL)
    cond <- parse_condition(basename(folder))
    tibble(
      path      = files,
      folder    = basename(folder),
      arena     = arena_id(files),
      type      = cond$type,
      light     = cond$light,
      effort    = cond$effort,
      diet      = cond$diet,
      replicate = cond$replicate,
      file_day  = coalesce(parse_file_day(files, folder), cond$folder_day)
    )
  })
}

# Everything that would make a file's IDs wrong or ambiguous. Empty = OK.
validate_index <- function(idx) {
  c(
    idx %>% filter(!str_detect(folder, regex(FOLDER_REGEX, ignore_case = TRUE))) %>%
      distinct(folder) %>%
      transmute(p = paste0("Folder name not recognised: '", folder, "'")) %>% pull(p),
    idx %>% filter(is.na(file_day)) %>%
      transmute(p = paste0("No 'day N' in folder or file name: ", folder, "/", basename(path))) %>% pull(p),
    idx %>% filter(type == "forage",
                   is.na(effort) | !light %in% names(LIGHT_LABELS) | is.na(diet)) %>%
      distinct(folder) %>%
      transmute(p = paste0("Couldn't parse effort / light / diet from folder: '", folder, "'")) %>% pull(p),
    idx %>% count(folder, arena, file_day) %>% filter(n > 1) %>%
      transmute(p = paste0("More than one file for ", arena, " day ", file_day, " in '", folder, "'")) %>% pull(p)
  )
}

# Same id columns the pipeline has always used, plus cond_id for mouse_day_id.
add_id_cols <- function(idx) {
  idx %>%
    mutate(
      cond_id   = case_when(
        type != "forage" ~ type,
        replicate > 1    ~ paste(light, effort, diet, paste0("rep", replicate), sep = "_"),
        TRUE             ~ paste(light, effort, diet, sep = "_")
      ),
      # mouse_id = one animal in one condition (days of that condition share it)
      mouse_id  = paste(cond_id, arena, sep = "_"),
      # Assumes arena number = same physical mouse across conditions, for a given
      # diet. This is true FOR NOW.
      animal_id = if_else(is.na(diet), arena, paste(diet, arena, sep = "_"))
    )
}

make_mouse_day_id <- function(arena, cond_id, day_num) paste0(arena, "_", cond_id, "_d", day_num)

stop_with <- function(header, problems) {
  stop(header, "\n", paste0("  - ", problems, collapse = "\n"), "\nNothing was written to the store.",
       call. = FALSE)
}

# ══════════════════════════════════════════════════════════════════════════
# CHOW WEIGHTS (chow.csv in each incoming folder)
# ══════════════════════════════════════════════════════════════════════════

write_chow_template <- function(folder_path, ids) {
  ids %>%
    transmute(mouse_day_id, arena, source_file,
              standard_chow_start = NA_real_, standard_chow_end = NA_real_) %>%
    arrange(mouse_day_id) %>%
    write_csv(file.path(folder_path, CHOW_FILE), na = "")
}

# ids = expected mouse_day_id / arena / source_file for this folder.
# Returns list(chow = validated weights, problems = character()).
read_chow <- function(folder_path, ids) {
  f <- file.path(folder_path, CHOW_FILE)
  chow <- read_csv(f, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    mutate(across(everything(), str_trim))
  where <- paste0(basename(folder_path), "/", CHOW_FILE)

  need <- c("mouse_day_id", "arena", "source_file", "standard_chow_start", "standard_chow_end")
  if (length(setdiff(need, names(chow))) > 0) {
    return(list(chow = NULL, problems = paste0(where, ": missing column(s) ",
                                               paste(setdiff(need, names(chow)), collapse = ", "))))
  }

  chow <- chow %>%
    mutate(start = suppressWarnings(as.numeric(standard_chow_start)),
           end   = suppressWarnings(as.numeric(standard_chow_end)))
  matched <- inner_join(chow, ids, by = "mouse_day_id", suffix = c("", ".expected"))

  problems <- c(
    chow %>% count(mouse_day_id) %>% filter(n > 1) %>%
      transmute(p = paste0(where, ": ", mouse_day_id, " appears ", n, " times")) %>% pull(p),
    ids %>% filter(!mouse_day_id %in% chow$mouse_day_id) %>%
      transmute(p = paste0(where, ": no row for ", mouse_day_id)) %>% pull(p),
    chow %>% filter(!mouse_day_id %in% ids$mouse_day_id) %>%
      transmute(p = paste0(where, ": ", mouse_day_id, " doesn't match any file in the folder")) %>% pull(p),
    matched %>% filter(arena != arena.expected | source_file != source_file.expected) %>%
      transmute(p = paste0(where, ": arena / source_file edited for ", mouse_day_id)) %>% pull(p),
    chow %>% filter(is.na(start) | is.na(end)) %>%
      transmute(p = paste0(where, ": blank or non-numeric weight for ", mouse_day_id)) %>% pull(p),
    chow %>% filter(start < 0 | end < 0) %>%
      transmute(p = paste0(where, ": negative weight for ", mouse_day_id)) %>% pull(p),
    chow %>% filter(end > start) %>%
      transmute(p = paste0(where, ": end (", end, " g) > start (", start, " g) for ", mouse_day_id)) %>% pull(p)
  )

  list(chow = chow %>% transmute(mouse_day_id, standard_chow_start = start, standard_chow_end = end),
       problems = problems)
}

# ══════════════════════════════════════════════════════════════════════════
# PARQUET STORE (DuckDB)
# Three files that grow with every ingest. Each update writes a complete new
# version to *.tmp.parquet, checks it, keeps the old version as
# *.prev.parquet, then renames the new one into place. The ingest_log is
# swapped last: a source file counts as loaded only once it is in the log.
# ══════════════════════════════════════════════════════════════════════════

# Columns that must be unique in each file
STORE_KEYS <- list(daily   = "mouse_day_id",
                   latency = c("mouse_day_id", "pair", "from_exit_time"),
                   log     = "source_hash")

store_tmp  <- function(name) sub("\\.parquet$", ".tmp.parquet",  STORE_FILES[[name]])
store_prev <- function(name) sub("\\.parquet$", ".prev.parquet", STORE_FILES[[name]])

sql_path <- function(path) {
  paste0("'", gsub("'", "''", normalizePath(path, winslash = "/", mustWork = FALSE)), "'")
}

store_connect <- function() dbConnect(duckdb::duckdb())

# Run a query against the store. Refer to the files as {daily}, {latency},
# {log}, e.g. store_query("SELECT count(*) FROM {daily}").
store_query <- function(sql) {
  for (nm in names(STORE_FILES)) {
    sql <- gsub(paste0("{", nm, "}"), paste0("read_parquet(", sql_path(STORE_FILES[[nm]]), ")"),
                sql, fixed = TRUE)
  }
  con <- store_connect()
  on.exit(dbDisconnect(con, shutdown = TRUE))
  as_tibble(dbGetQuery(con, sql))
}

# Whole table, or an empty one (key columns only) if nothing has been ingested yet.
read_store <- function(name) {
  if (!file.exists(STORE_FILES[[name]])) {
    return(as_tibble(map(set_names(c(STORE_KEYS[[name]], "source_hash") %>% unique()), ~ character())))
  }
  store_query(paste0("SELECT * FROM {", name, "}"))
}

# Rows whose key appears more than once in the parquet file at `path`.
key_duplicates <- function(con, path, keys) {
  dbGetQuery(con, sprintf("SELECT %s, count(*) AS n FROM read_parquet(%s) GROUP BY ALL HAVING count(*) > 1",
                          paste(keys, collapse = ", "), sql_path(path)))
}

# Writes old rows (minus drop_hashes) + new_rows to the tmp file and verifies
# it. Returns TRUE if a tmp file was written.
write_store_tmp <- function(con, name, new_rows, drop_hashes = character()) {
  path <- STORE_FILES[[name]]
  tmp  <- store_tmp(name)
  has_old <- file.exists(path)
  has_new <- nrow(new_rows) > 0
  if (!has_old && !has_new) return(FALSE)

  duckdb_register(con, "drop_hashes", tibble(h = as.character(drop_hashes)), overwrite = TRUE)
  old_sql <- sprintf("SELECT * FROM read_parquet(%s) WHERE source_hash NOT IN (SELECT h FROM drop_hashes)",
                     sql_path(path))
  n_old <- if (has_old) dbGetQuery(con, sprintf("SELECT count(*) AS n FROM (%s)", old_sql))$n else 0

  if (has_new) duckdb_register(con, "new_rows", new_rows, overwrite = TRUE)
  body <- case_when(
    has_old && has_new ~ paste(old_sql, "UNION ALL BY NAME SELECT * FROM new_rows"),
    has_old            ~ old_sql,
    TRUE               ~ "SELECT * FROM new_rows"
  )
  dbExecute(con, sprintf("COPY (%s) TO %s (FORMAT parquet)", body, sql_path(tmp)))

  n_tmp <- dbGetQuery(con, sprintf("SELECT count(*) AS n FROM read_parquet(%s)", sql_path(tmp)))$n
  dups  <- key_duplicates(con, tmp, STORE_KEYS[[name]])
  if (n_tmp != n_old + nrow(new_rows) || nrow(dups) > 0) {
    unlink(tmp)
    stop("Check failed while building ", basename(path), ": expected ", n_old + nrow(new_rows),
         " rows, got ", n_tmp, "; duplicate keys: ", nrow(dups), ". Nothing was written to the store.",
         call. = FALSE)
  }
  TRUE
}

swap_in <- function(name) {
  path <- STORE_FILES[[name]]
  if (file.exists(path)) file.rename(path, store_prev(name))
  file.rename(store_tmp(name), path)
}

# Undo a run that was interrupted between swapping the data files and the
# log: their source_hash values would be missing from the log.
recover_store <- function() {
  unlink(map_chr(names(STORE_FILES), store_tmp))
  log_hashes <- read_store("log")$source_hash
  restored <- character()
  for (nm in c("daily", "latency")) {
    path <- STORE_FILES[[nm]]
    orphan <- (!file.exists(path) && file.exists(store_prev(nm))) ||
      (file.exists(path) && length(setdiff(read_store(nm)$source_hash, log_hashes)) > 0)
    if (orphan && file.exists(store_prev(nm))) {
      file.copy(store_prev(nm), path, overwrite = TRUE)
      restored <- c(restored, basename(path))
    }
  }
  if (length(restored) > 0) {
    message("An earlier ingest was interrupted. Restored the previous version of: ",
            paste(restored, collapse = ", "))
  }
  check_store()
}

# Fails loudly if any key is duplicated or a data row has no ingest_log entry.
check_store <- function(verbose = FALSE) {
  con <- store_connect()
  on.exit(dbDisconnect(con, shutdown = TRUE))
  problems <- character()
  for (nm in names(STORE_FILES)) {
    path <- STORE_FILES[[nm]]
    if (!file.exists(path)) next
    dups <- key_duplicates(con, path, STORE_KEYS[[nm]])
    if (nrow(dups) > 0) problems <- c(problems, paste0(basename(path), ": ", nrow(dups), " duplicated key(s)"))
  }
  log_hashes <- read_store("log")$source_hash
  for (nm in c("daily", "latency")) {
    orphan <- setdiff(read_store(nm)$source_hash, log_hashes)
    if (length(orphan) > 0) problems <- c(problems, paste0(basename(STORE_FILES[[nm]]), ": rows from ",
                                                           length(orphan), " file(s) missing from ingest_log"))
  }
  if (length(problems) > 0) {
    stop("STORE CHECK FAILED - do not analyse this data until fixed:\n",
         paste0("  - ", problems, collapse = "\n"), call. = FALSE)
  }
  if (verbose) cat("Store check passed: no duplicates.\n")
  invisible(TRUE)
}

# ══════════════════════════════════════════════════════════════════════════
# INGEST
# ══════════════════════════════════════════════════════════════════════════

# Moves a folder's files into dest_root/<folder>/. A file already there with
# identical contents is just removed from the source. A different one stops,
# unless its name is in `replaced`: then the old copy is kept as
# <name>.replaced_<time> (no .csv ending, so a rebuild ignores it). For
# chow.csv the archived copy becomes old rows + the new rows, so raw/ always
# holds a complete chow.csv for 02c_rebuild_store.R.
archive_folder <- function(folder_path, dest_root, replaced = character()) {
  dest <- file.path(dest_root, basename(folder_path))
  files <- list.files(folder_path, recursive = TRUE, full.names = TRUE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  for (f in files) {
    target <- file.path(dest, str_remove(f, fixed(paste0(folder_path, "/"))))
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(target)) {
      if (unname(tools::md5sum(f)) == unname(tools::md5sum(target))) {
        unlink(f)
        next
      }
      if (!basename(f) %in% replaced) {
        stop("Can't archive ", f, ": a different file already exists at ", target, call. = FALSE)
      }
      old_copy <- paste0(target, ".replaced_", stamp)
      file.rename(target, old_copy)
      if (basename(f) == CHOW_FILE) {
        read_chr <- function(p) read_csv(p, col_types = cols(.default = col_character()), show_col_types = FALSE)
        new_rows <- read_chr(f)
        bind_rows(new_rows, filter(read_chr(old_copy), !mouse_day_id %in% new_rows$mouse_day_id)) %>%
          arrange(mouse_day_id) %>%
          write_csv(target, na = "")
        unlink(f)
        next
      }
    }
    file.rename(f, target)
  }
  unlink(folder_path, recursive = TRUE)
}

# Already-loaded files: delete the incoming copy if identical bytes are already
# archived under any name (so raw/ never holds two copies of one recording);
# otherwise they are archived normally.
discard_already_archived <- function(files, dest_root) {
  if (length(files) == 0) return(invisible())
  archived <- unname(tools::md5sum(get_csvs(dest_root)))
  unlink(files[unname(tools::md5sum(files)) %in% archived])
}

# Files in archive targets that already exist with different contents
# (ignoring names in `replaced`).
archive_conflicts <- function(folder_paths, dest_root, replaced = character()) {
  unlist(map(folder_paths, function(fp) {
    files <- list.files(fp, recursive = TRUE, full.names = TRUE)
    targets <- file.path(dest_root, basename(fp), str_remove(files, fixed(paste0(fp, "/"))))
    clash <- file.exists(targets) & !basename(files) %in% replaced
    clash[clash] <- unname(tools::md5sum(files[clash])) != unname(tools::md5sum(targets[clash]))
    targets[clash]
  }))
}

# Loads every new CSV under src_dir into the store. Safe to run any number of
# times: files already in the ingest_log are skipped, and nothing is written
# unless every new file reads, every ID is new (or listed in replace_files),
# and every chow.csv is complete.
ingest <- function(src_dir = INCOMING_DIR, archive = TRUE, replace_files = REPLACE_FILES) {
  dir.create(STORE_DIR, recursive = TRUE, showWarnings = FALSE)
  recover_store()

  idx <- index_folder_files(src_dir)
  if (is.null(idx) || nrow(idx) == 0) {
    cat("Nothing to ingest: no CSV files in", src_dir, "\n")
    return(invisible(NULL))
  }
  problems <- validate_index(idx)
  if (length(problems) > 0) stop_with("Fix these before ingesting:", problems)

  idx <- idx %>% mutate(source_hash = unname(tools::md5sum(path)),
                        source_file = basename(path),
                        folder_path = file.path(src_dir, folder))
  log_old <- read_store("log")
  idx <- idx %>% mutate(already = source_hash %in% log_old$source_hash)

  same_bytes <- idx %>% filter(!already) %>% group_by(source_hash) %>% filter(n() > 1) %>% ungroup()
  if (nrow(same_bytes) > 0) {
    stop_with("These files have identical contents (copied twice?):",
              paste0(same_bytes$folder, "/", same_bytes$source_file))
  }

  skipped <- idx %>% filter(already)
  new     <- idx %>% filter(!already) %>% add_id_cols()
  cat(nrow(idx), "CSV file(s) found:", nrow(new), "new,", nrow(skipped), "already loaded (skipped)\n")

  if (nrow(new) == 0) {
    if (archive) {
      discard_already_archived(skipped$path, RAW_DIR)
      walk(unique(skipped$folder_path), archive_folder, dest_root = RAW_DIR)
    }
    cat("Nothing new to add.\n")
    return(invisible(NULL))
  }

  # ── Read every new file (all must succeed) ──
  results <- vector("list", nrow(new))
  failed  <- character()
  for (i in seq_len(nrow(new))) {
    meta <- new[i, ]
    cat(sprintf("[%d/%d] %s / %s\n", i, nrow(new), meta$folder, meta$source_file))
    results[[i]] <- tryCatch(process_file(meta), error = function(e) {
      failed <<- c(failed, paste0(meta$folder, "/", meta$source_file, ": ", conditionMessage(e)))
      NULL
    })
  }
  if (length(failed) > 0) stop_with("These files couldn't be read:", failed)

  id_cols <- c("mouse_id", "animal_id", "arena", "type", "light", "effort", "diet", "replicate")
  file_cols <- function(i) new[i, ] %>% select(all_of(id_cols), cond_id, source_hash, source_file, folder)

  daily_new <- map_dfr(seq_len(nrow(new)), function(i) {
    if (nrow(results[[i]]$daily) == 0) return(NULL)
    bind_cols(results[[i]]$daily, file_cols(i))
  }) %>%
    mutate(mouse_day_id = make_mouse_day_id(arena, cond_id, day_num))

  latency_new <- map_dfr(seq_len(nrow(new)), function(i) {
    lat <- results[[i]]$latency
    if (nrow(lat) == 0) return(NULL)
    bind_cols(lat, file_cols(i))
  })
  if (nrow(latency_new) > 0) {
    latency_new <- latency_new %>% mutate(mouse_day_id = make_mouse_day_id(arena, cond_id, day_num))
  }

  # ── Duplicate / conflict checks ──
  in_batch <- daily_new %>% count(mouse_day_id) %>% filter(n > 1)
  if (nrow(in_batch) > 0) {
    stop_with("These mouse-days come from more than one new file:", in_batch$mouse_day_id)
  }
  daily_old <- read_store("daily")
  conflicts <- daily_new %>% filter(mouse_day_id %in% daily_old$mouse_day_id)
  not_allowed <- conflicts %>% filter(!source_file %in% replace_files)
  if (nrow(not_allowed) > 0) {
    stop_with(paste0("Already loaded from a DIFFERENT file (to replace, add the new file name to ",
                     "REPLACE_FILES in 00_config.R):"),
              paste0(not_allowed$mouse_day_id, "  <- ", not_allowed$folder, "/", not_allowed$source_file))
  }
  drop_hashes <- daily_old %>% filter(mouse_day_id %in% conflicts$mouse_day_id) %>%
    distinct(source_hash) %>% pull(source_hash)
  if (length(drop_hashes) > 0) {
    removed <- daily_old %>% filter(source_hash %in% drop_hashes)
    cat("Replacing", nrow(removed), "mouse-day(s):", paste(removed$mouse_day_id, collapse = ", "), "\n")
  }

  # ── Chow weights ──
  ids_by_folder <- daily_new %>% distinct(folder, mouse_day_id, arena, source_file)
  folders <- new %>% distinct(folder, folder_path)
  no_chow <- folders %>% filter(!file.exists(file.path(folder_path, CHOW_FILE)))
  if (nrow(no_chow) > 0) {
    for (i in seq_len(nrow(no_chow))) {
      write_chow_template(no_chow$folder_path[i], filter(ids_by_folder, folder == no_chow$folder[i]))
    }
    cat("\nChow weights needed. Fill in standard_chow_start and standard_chow_end (grams) in:\n",
        paste0("  ", file.path(no_chow$folder_path, CHOW_FILE), "\n"),
        "then run this script again. Nothing was written to the store.\n")
    return(invisible(NULL))
  }
  chow <- map(seq_len(nrow(folders)), function(i) {
    read_chow(folders$folder_path[i], filter(ids_by_folder, folder == folders$folder[i]))
  })
  chow_problems <- unlist(map(chow, "problems"))
  if (length(chow_problems) > 0) stop_with("Problems in chow.csv:", chow_problems)
  daily_new <- daily_new %>% left_join(bind_rows(map(chow, "chow")), by = "mouse_day_id")

  # replaced files, and the chow.csv of their folder, may overwrite archived copies
  replaced <- if (nrow(conflicts) > 0) c(unique(conflicts$source_file), CHOW_FILE) else character()
  if (archive) {
    clash <- archive_conflicts(unique(idx$folder_path), RAW_DIR, replaced)
    if (length(clash) > 0) stop_with("A different file with the same name is already archived:", clash)
  }

  # ── Rows to add ──
  recording_start <- map_chr(new$path, read_header_start)
  log_new <- new %>%
    mutate(rows_read      = map_int(results, "rows_read"),
           total_pellets  = map_dbl(results, "total_pellets"),
           days_produced  = map_int(results, ~ nrow(.x$daily)),
           recording_start = recording_start,
           ingested_at    = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) %>%
    select(source_hash, source_file, folder, arena, type, light, effort, diet, replicate,
           file_day, recording_start, rows_read, days_produced, total_pellets, ingested_at)
  daily_new <- daily_new %>%
    left_join(select(log_new, source_hash, recording_start), by = "source_hash") %>%
    select(mouse_day_id, all_of(id_cols), day_num, all_of(BEHAVIOR_VARS), max_turns_1min,
           standard_chow_start, standard_chow_end, recording_start, source_file, source_hash)
  if (nrow(latency_new) > 0) {
    latency_new <- latency_new %>%
      select(mouse_day_id, all_of(id_cols), day_num, pair, from_exit_time, to_entry_time,
             latency_sec, source_file, source_hash)
  }

  # ── Write: build + check all three, then swap (log last) ──
  con <- store_connect()
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  written <- c(
    daily   = write_store_tmp(con, "daily",   daily_new,   drop_hashes),
    latency = write_store_tmp(con, "latency", latency_new, drop_hashes),
    log     = write_store_tmp(con, "log",     log_new,     drop_hashes)
  )
  for (nm in c("daily", "latency", "log")) if (written[[nm]]) swap_in(nm)
  check_store()

  if (archive) {
    discard_already_archived(skipped$path, RAW_DIR)
    walk(unique(idx$folder_path), archive_folder, dest_root = RAW_DIR, replaced = replaced)
  }

  cat("\nAdded", nrow(daily_new), "mouse-day(s) and", nrow(latency_new), "latency event(s) from",
      nrow(new), "file(s).\n")
  print(count(daily_new, type, light, effort, diet, day_num), n = Inf)
  invisible(daily_new)
}

# ══════════════════════════════════════════════════════════════════════════
# RANDOM FOREST FEATURE TABLE
# ══════════════════════════════════════════════════════════════════════════

# One row per mouse x day: daily summary + chow eaten + per-day latency stats
# (<pair>_n, _median_sec, _mean_sec, _iqr_sec). RF_FEATURES picks columns from it.
build_rf_features <- function() {
  if (!file.exists(STORE_FILES[["daily"]])) return(tibble())
  daily <- store_query("SELECT *, standard_chow_start - standard_chow_end AS standard_chow_eaten FROM {daily}")
  if (nrow(daily) == 0) return(daily)

  lat <- if (file.exists(STORE_FILES[["latency"]])) store_query("
    SELECT mouse_day_id, pair,
           count(*)                    AS n,
           median(latency_sec)         AS median_sec,
           avg(latency_sec)            AS mean_sec,
           quantile_cont(latency_sec, 0.75) - quantile_cont(latency_sec, 0.25) AS iqr_sec
    FROM {latency} GROUP BY ALL") else tibble()

  if (nrow(lat) > 0) {
    lat_wide <- lat %>%
      mutate(n = as.numeric(n)) %>%
      pivot_wider(names_from = pair, values_from = c(n, median_sec, mean_sec, iqr_sec),
                  names_glue = "{pair}_{.value}")
    daily <- left_join(daily, lat_wide, by = "mouse_day_id")
  }
  for (f in setdiff(LATENCY_FEATURES, names(daily))) daily[[f]] <- NA_real_
  # no events that day = 0 trips, not unknown
  daily %>%
    mutate(across(all_of(paste0(ZONE_LATENCY_PAIRS$pair, "_n")), ~ coalesce(.x, 0)),
           light_group = factor(unname(LIGHT_LABELS[light]), levels = unname(LIGHT_LABELS)),
           effort      = factor(effort, levels = EFFORT_LEVELS))
}
