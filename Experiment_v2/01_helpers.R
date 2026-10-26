## ══════════════════════════════════════════════════════════════════════════
## All reusable helper functions: small summary helpers, folder-name parsing,
## CSV reading, zone-bout extraction, zone -> zone latency, paired stats and
## the shared paired bar-plot / across-days plot engines.
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

