## ══════════════════════════════════════════════════════════════════════════
## Libraries, file paths, constants, and color palettes.
## This must be run first.
## ══════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(ggplot2)
library(readr)
library(patchwork)
library(plotly)
library(writexl)
library(ggpubr)
library(lme4)
library(lmerTest)
library(colorspace)
library(duckdb)

# ══════════════════════════════════════════════════════════════════════════
# CONFIG
# ══════════════════════════════════════════════════════════════════════════

# ── Data folders ──────────────────────────────────────────────────────────
# Everything lives in Experiment_v2/data/ (git-ignored), so the paths are the
# same for anyone with the repo. Works whether R runs from the repo root or
# from Experiment_v2/. See DATA_DECISIONS.md for the design.
#   incoming/  drop each day's EthoVision CSVs here, in a condition folder
#   raw/       ingest moves loaded CSVs (and chow.csv) here
#   store/     the Parquet files: daily_summary, latency_events, ingest_log
# (DATA_DECISIONS.md only exists in Experiment_v2/; the repo root has its own 00_config.R)
EXP_DIR <- if (file.exists("DATA_DECISIONS.md")) "." else "Experiment_v2"
if (!file.exists(file.path(EXP_DIR, "DATA_DECISIONS.md"))) {
  stop("Run R from the repo root or from Experiment_v2/ (current: ", getwd(), ")")
}
DATA_DIR     <- file.path(EXP_DIR, "data")
INCOMING_DIR <- file.path(DATA_DIR, "incoming")
RAW_DIR      <- file.path(DATA_DIR, "raw")
STORE_DIR    <- file.path(DATA_DIR, "store")
STORE_FILES  <- c(
  daily   = file.path(STORE_DIR, "daily_summary.parquet"),
  latency = file.path(STORE_DIR, "latency_events.parquet"),
  log     = file.path(STORE_DIR, "ingest_log.parquet")
)

# ── Incoming folder names ─────────────────────────────────────────────────
# Each folder in incoming/ is one day for one condition. Three families of
# folder names are recognised, and each MUST end in "day N":
#
#   BASELINE : "Baseline day 1"
#   TRAINING : "training day 3"
#   FORAGE   : "<effort>_effort_<light>_light_<diet> day N"
#              e.g. "low_effort_no_light_sucrose day 2",
#                   "high_effort_yellow_light_sucrose day 1"
#              (underscores or spaces both work; a number right before
#               "day N" = repeat, e.g. "low_effort_no_light_sucrose 2 day 1")
#
# Ingest stops if a folder name doesn't match.
FOLDER_REGEX <- "^(baseline|training|(low|medium|high)[ _]+effort)"

# ── Day numbering ─────────────────────────────────────────────────────────
# Day number comes from "day N" / "dN" in the file name, or else "day N" at the
# end of the incoming folder name. A file with neither is refused: numbering
# days automatically would let a re-export of an already-loaded day slip in as
# a new day (see DATA_DECISIONS.md).
# A single long recording that spans several 24 h days is split into days
# automatically (day_num = labelled day + day within the recording).

# Replacing an already-loaded mouse-day: list the new file names here, then run
# 02a. The old rows for those mouse-days are removed and the new ones added.
REPLACE_FILES <- c()

# ── Zeitgeber time ────────────────────────────────────────────────────────
# All recordings are assumed to start 1 hour after lights-on (ZT 1) and run ~23
# hours, so every file is missing the same ~1 hour window: ZT 0 -> ZT 1.
DEFAULT_ZT_OFFSET <- 1

# If a particular file's recording didn't start at ZT 1, either add its file
# name here:   FILE_ZT_OVERRIDES <- c("Trial 12 Arena 3.csv" = 1.25)
FILE_ZT_OVERRIDES <- c()

# ...or add a rule. NA in a column = "matches anything". `light` is a regex
# against the parsed light label ("none", "white", "yellow"), day_num is the
# file's day number. Example (uncomment and edit):
#   tibble(type = "forage", light = "white", effort = "low", diet = "sucrose",
#          day_num = 1L, arena_pattern = NA_character_, offset_hr = 1 + 15/60,
#          note = "white low sucrose day 1 started 15 min late")
ZT_OFFSET_RULES <- tibble(
  type = character(), light = character(), effort = character(), diet = character(),
  day_num = integer(), arena_pattern = character(), offset_hr = numeric(), note = character()
)

get_zt_offset <- function(path, type, light, effort, diet, day_num, arena) {

  # 1. Check for a specific file override first
  if (length(FILE_ZT_OVERRIDES) > 0 && basename(path) %in% names(FILE_ZT_OVERRIDES)) {
    return(unname(FILE_ZT_OVERRIDES[basename(path)]))
  }

  # 2. Check rule-based overrides, if any rules exist
  if (nrow(ZT_OFFSET_RULES) > 0) {
    for (i in seq_len(nrow(ZT_OFFSET_RULES))) {
      r <- ZT_OFFSET_RULES[i, ]

      type_ok   <- is.na(r$type)   || identical(type, r$type)
      light_ok  <- is.na(r$light)  || (!is.na(light) && str_detect(light, regex(r$light, ignore_case = TRUE)))
      effort_ok <- is.na(r$effort) || identical(effort, r$effort)
      diet_ok   <- is.na(r$diet)   || identical(diet, r$diet)
      day_ok    <- is.na(r$day_num) || identical(as.integer(day_num), as.integer(r$day_num))
      arena_ok  <- is.na(r$arena_pattern) || str_detect(arena, regex(r$arena_pattern, ignore_case = TRUE))

      if (type_ok && light_ok && effort_ok && diet_ok && day_ok && arena_ok) return(r$offset_hr)
    }
  }

  # 3. If no override/rule applies, use the default
  DEFAULT_ZT_OFFSET
}

BIN_SIZE_SEC <- 60   # seconds per actogram / peak-running bin

# ── Raw CSV columns ───────────────────────────────────────────────────────
# Left = header in the raw export (matched case-insensitively), right = name
# used in the code. Columns that are missing from a file are filled with NA
# (with a warning) so older/newer exports can be mixed. "Recording time" and
# "Result 1" are not loaded.
COLUMN_MAP <- c(
  "Trial time"             = "trial_time",
  "X center"               = "x_center",
  "Y center"               = "y_center",
  "X nose"                 = "x_nose",
  "Y nose"                 = "y_nose",
  "X tail"                 = "x_tail",
  "Y tail"                 = "y_tail",
  "Area"                   = "area",
  "Areachange"             = "area_change",
  "Elongation"             = "elongation",
  "Direction"              = "direction",
  "Distance moved"         = "distance_moved",
  "pellet earned"          = "pellets_earned",
  "licks"                  = "licks",
  "wheel"                  = "wheel_cycles",
  "nose pokes"             = "nose_pokes",
  "In zone wheel"          = "in_zone_wheel",
  "In zone cup"            = "in_zone_cup",
  "In zone shelter"        = "in_zone_shelter",
  "In zone food hopper"    = "in_zone_hopper",
  "In zone to earn pellet" = "in_zone_pellet"
)

# If the header line count can't be detected automatically, skip this many lines.
HEADER_SKIP_FALLBACK <- 34

# Which zone column is "the pellet zone". Assumed to be "In zone to earn
# pellet"; change to "in_zone_hopper" if you mean the food hopper zone.
PELLET_ZONE_VAR <- "in_zone_pellet"

# Zone -> zone latencies. latency = time from LEAVING `from_zone` to the next
# ENTRY into `to_zone`.
#   mode "all_exits" : every exit from the from-zone is paired with the next
#                      to-zone entry (the original wheel -> cup definition)
#   mode "last_exit" : only the final exit before each to-zone entry counts
ZONE_LATENCY_PAIRS <- tribble(
  ~pair,           ~label,                       ~from_zone,      ~to_zone,      ~mode,
  "wheel_to_cup",  "Wheel zone \u2192 cup zone", "in_zone_wheel", "in_zone_cup", "all_exits",
  "pellet_to_cup", "Pellet zone \u2192 cup zone", PELLET_ZONE_VAR, "in_zone_cup", "all_exits"
)

# ── Behaviours summarised per day ─────────────────────────────────────────
# name = column in daily_all, value = axis label. Add a variable here and it
# flows into the summaries, trend plots, bar plots and drift plots.
BEHAVIOR_LABELS <- c(
  pellets              = "Pellets earned",
  wheel                = "Wheel turns",
  nose_pokes           = "Nose pokes",
  licks                = "Licks",
  distance_moved       = "Distance moved",
  in_zone_wheel_time   = "Time in wheel zone (s)",
  in_zone_pellet_time  = "Time in pellet zone (s)",
  in_zone_cup_time     = "Time in cup zone (s)",
  in_zone_hopper_time  = "Time in food hopper zone (s)",
  in_zone_shelter_time = "Time in shelter (s)"
)
BEHAVIOR_VARS <- names(BEHAVIOR_LABELS)

BEHAVIOR_METRICS <- tibble(y_col = BEHAVIOR_VARS, y_label = unname(BEHAVIOR_LABELS))

# ── Forage-phase analysis options ─────────────────────────────────────────
# Which day numbers of each forage condition go into the effort / light
# comparisons. NULL = all days (averaged per animal). e.g. c(2) = day 2 only.
FORAGE_DAYS_TO_USE <- NULL

# Random forest (07): features, and what gets held out in cross-validation.
# Features are columns of build_rf_features() (one row per mouse x day):
# the daily behaviours, peak wheel rate, chow eaten, and per-day latency
# stats for every pair in ZONE_LATENCY_PAIRS (n, median, mean, IQR).
#   "mouse_id"  = hold out one animal-in-one-condition (original behaviour)
#   "animal_id" = hold out the whole physical animal across ALL conditions
LATENCY_FEATURES <- as.vector(outer(ZONE_LATENCY_PAIRS$pair,
                                    c("n", "median_sec", "mean_sec", "iqr_sec"), paste, sep = "_"))
RF_FEATURES   <- c(BEHAVIOR_VARS, "max_turns_1min", "standard_chow_eaten", LATENCY_FEATURES)
RF_HOLDOUT_ID <- "mouse_id"

# ── Conditions and colours ────────────────────────────────────────────────
EFFORT_LEVELS <- c("low", "medium", "high")

# Parsed light value -> display label. Order here = order in plots.
LIGHT_LABELS  <- c(none = "No light", white = "White light", yellow = "Yellow light")
# Words that can appear before "light" in a folder name -> parsed light value
LIGHT_ALIASES <- c(no = "none", none = "none", dark = "none", off = "none",
                   white = "white", yellow = "yellow")

COND_COLOURS <- c(
  low    = "#2CBBAB",
  medium = "#ED1A61",
  high   = "#3F000F"
)

# Base colour per light condition - the "high effort" shade IS this colour,
# lower efforts are progressively lighter tints of it.
CONDITION_BASE_COLOURS <- setNames(c("grey20", "#5B8DEF", "#E6A700"), unname(LIGHT_LABELS))

make_effort_shades <- function(base_colour, effort_levels = EFFORT_LEVELS) {
  n <- length(effort_levels)
  amounts <- seq(0.75, 0, length.out = n)
  shades <- sapply(amounts, function(a) lighten(base_colour, amount = a))
  setNames(shades, effort_levels)
}

CONDITION_COLOURS <- imap(CONDITION_BASE_COLOURS, function(base_colour, cond_name) {
  shades <- make_effort_shades(base_colour)
  setNames(shades, paste(cond_name, names(shades), sep = "_"))
}) %>% unname() %>% unlist()

# Keeps legend entries in the right order (each light: low -> high)
CONDITION_COLOUR_ORDER <- names(CONDITION_COLOURS)

