## ══════════════════════════════════════════════════════════════════════════
## All reusable helper functions: folder-name parsing, CSV reading, zone-bout
## extraction, wheel->cup latency, and paired stats.
## ══════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════
# HELPERS
# ══════════════════════════════════════════════════════════════════════════

get_csvs <- function(folder) {
  list.files(folder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
}

# Folder name -> type / light / effort / diet / day_num / has_explicit_day.
#
# Two branches:
#   1) "Training day N"                    -> type = "training"
#   2) everything else ("LD low effort sucrose", "... day N", "... 2", etc.)
#                                           -> type = "forage"
# We actually have NO training recordings yet!

parse_condition <- function(folder_name) {

  if (str_detect(folder_name, regex("^training", ignore_case = TRUE))) {
    day_num <- str_extract(folder_name, "\\d+$")
    day_num <- if (is.na(day_num)) 1L else as.integer(day_num)
    return(tibble(
      type = "training",
      light = NA_character_, effort = NA_character_, diet = NA_character_,
      day_num = day_num, has_explicit_day = TRUE
    ))
  }

  day_match <- str_match(folder_name, regex("day\\s*(\\d+)\\s*$", ignore_case = TRUE))
  if (!is.na(day_match[1, 2])) {
    day_num <- as.integer(day_match[1, 2])
    has_explicit_day <- TRUE
    base_name <- str_trim(str_remove(folder_name, regex("\\s*day\\s*\\d+\\s*$", ignore_case = TRUE)))
  } else {
    trailing_digit <- str_extract(folder_name, "\\d+$")
    day_num <- if (is.na(trailing_digit)) 1L else as.integer(trailing_digit)
    has_explicit_day <- FALSE
    base_name <- str_trim(str_remove(folder_name, "\\s*\\d+$"))
  }

  effort <- str_extract(base_name, regex("(low|medium|high)(?=\\s+effort)", ignore_case = TRUE))
  diet   <- str_extract(base_name, regex("(sucrose|hfd|sd)\\s*$", ignore_case = TRUE))

  light <- base_name
  if (!is.na(effort)) light <- str_remove(light, regex(paste0("\\s*", effort, "\\s+effort\\s*"), ignore_case = TRUE))
  if (!is.na(diet))   light <- str_remove(light, regex(paste0("\\s*", diet, "\\s*$"),           ignore_case = TRUE))
  light <- str_trim(light)

  tibble(
    type       = "forage",
    light      = light,
    effort     = str_to_lower(effort),
    diet       = str_to_lower(str_trim(diet)),
    day_num    = day_num,
    has_explicit_day = has_explicit_day
  )
}

arena_id <- function(filepath) {
  fname <- tools::file_path_sans_ext(basename(filepath))
  str_extract(fname, regex("arena_?\\d+[a-z]?$", ignore_case = TRUE)) %>%
    str_remove("_") %>%
    str_to_upper()
}

drop_incomplete_day <- function(df) {
  if (n_distinct(df$day) > 1) filter(df, day < max(day)) else df
}

read_arena <- function(filepath, zt_offset) {
  raw <- read_csv(filepath, skip = 34, show_col_types = FALSE) %>%
    slice(-1)

  missing_cols <- setdiff(names(COLUMN_MAP), names(raw))
  if (length(missing_cols) > 0) {
    warning("Missing expected column(s) in ", basename(filepath), ": ",
            paste(missing_cols, collapse = ", "))
  }

  raw <- raw %>% select(any_of(names(COLUMN_MAP)))
  names(raw) <- unname(COLUMN_MAP[names(raw)])

  raw %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(
      time_hr = trial_time / 3600,
      zt      = (time_hr + zt_offset) %% 24,
      zt_c    = ifelse(zt > 12, zt - 24, zt),
      day     = floor((time_hr + zt_offset) / 24),
      bin     = floor(trial_time / BIN_SIZE_SEC)
    )
}

# Bout extraction for binary zone-occupancy columns
# Turns a 0/1 "in zone" column into one row per continuous bout, with the
# trial_time (seconds) at which the bout started and ended. Used for both
# the wheel<->cup latency analysis and the zone timeline plot.
#
# If the mouse is still "in zone" at the very last row of the file, that
# bout is closed off at the file's final trial_time.
extract_zone_bouts <- function(df, zone_col) {
  if (!zone_col %in% names(df) || nrow(df) == 0) {
    return(tibble(onset_time = numeric(0), offset_time = numeric(0)))
  }
  x <- df[[zone_col]]
  x <- ifelse(is.na(x), 0, x)
  x <- as.numeric(x != 0)  # coerce to strict 0/1 in case of stray values

  prev <- dplyr::lag(x, default = 0)
  onset_idx  <- which(x == 1 & prev == 0)
  offset_idx <- which(x == 0 & prev == 1)

  # still in-zone when the file ends -> close the bout at the last sample
  if (length(onset_idx) > length(offset_idx)) {
    offset_idx <- c(offset_idx, nrow(df))
  }

  if (length(onset_idx) == 0) {
    return(tibble(onset_time = numeric(0), offset_time = numeric(0)))
  }

  tibble(
    onset_time  = df$trial_time[onset_idx],
    offset_time = df$trial_time[offset_idx]
  )
}

# Wheel -> cup travel latency
# For every bout of being in the wheel zone, finds the very next cup-zone
# entry that happens afterward in the same file, and returns the gap between
# them (seconds). This is "how long after leaving the wheel did the mouse
# reach the cup." 
compute_wheel_to_cup_latency <- function(path, zt_offset) {
  df <- read_arena(path, zt_offset)

  if (!all(c("in_zone", "in_zone_cup") %in% names(df))) {
    return(tibble())
  }

  wheel_bouts <- extract_zone_bouts(df, "in_zone")
  cup_bouts   <- extract_zone_bouts(df, "in_zone_cup")

  if (nrow(wheel_bouts) == 0 || nrow(cup_bouts) == 0) {
    return(tibble())
  }

  cup_onsets <- sort(cup_bouts$onset_time)

  purrr::map_dfr(wheel_bouts$offset_time, function(wheel_exit) {
    candidates <- cup_onsets[cup_onsets > wheel_exit]
    if (length(candidates) == 0) return(NULL)
    cup_entry <- candidates[1]
    tibble(
      wheel_exit_time = wheel_exit,
      cup_entry_time  = cup_entry,
      latency_sec     = cup_entry - wheel_exit
    )
  })
}

# Paired pairwise t-tests, Holm-adjusted
# Repeated-measures
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
