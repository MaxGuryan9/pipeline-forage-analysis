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

# ══════════════════════════════════════════════════════════════════════════
# CONFIG
# ══════════════════════════════════════════════════════════════════════════

# Parent folder that CONTAINS the data folders (Baseline, training, ...).
BASE_DIR <- "C:/Users/irs3th/Desktop/forage left corner"

# Every subfolder of BASE_DIR whose name matches FOLDER_REGEX is one "session
# batch". Three families of folder names are recognised:
#
#   BASELINE : "Baseline"                       (2 days)
#   TRAINING : "training"                       (several trial days)
#   FORAGE   : "<effort>_effort_<light>_light_<diet>"
#              e.g. "low_effort_no_light_sucrose", "medium_effort_white_light_sucrose",
#                   "high_effort_yellow_light_sucrose"
#              (underscores or spaces both work; a trailing number = repeat,
#               a trailing "day N" = which day that folder holds)
#
# Folders that don't match (e.g. old LD/LL pilot folders) are ignored, and
# folders that don't exist yet are simply absent.
FOLDER_REGEX <- "^(baseline|training|(low|medium|high)[ _]+effort)"

ALL_DIRS    <- list.dirs(BASE_DIR, recursive = FALSE)
.is_data    <- str_detect(basename(ALL_DIRS), regex(FOLDER_REGEX, ignore_case = TRUE))
CONDITION_FOLDERS <- ALL_DIRS[.is_data]
if (any(!.is_data)) {
  message("Ignoring folders that don't look like Baseline / training / <effort>_effort_...: ",
          paste(basename(ALL_DIRS[!.is_data]), collapse = ", "))
}

# ── Day numbering ─────────────────────────────────────────────────────────
# Day number comes from (in order of priority):
#   1. "day N" / "dN" in the file name or a subfolder name (e.g. training/Day 2/...)
#   2. "day N" at the end of the condition folder name
#   3. otherwise: files are numbered 1, 2, ... in alphabetical order when an
#      arena has several files in one folder; a single file = day 1
# On top of that, a single long recording that spans several 24 h days is split
# into days automatically (day_num = file day + day within the recording).

# ── Zeitgeber time ────────────────────────────────────────────────────────
# All recordings are assumed to start 1 hour after lights-on (ZT 1) and run ~23
# hours, so every file is missing the same ~1 hour window: ZT 0 -> ZT 1.
DEFAULT_ZT_OFFSET <- 1

# If a particular file's recording didn't start at ZT 1, either add the exact
# path here:   FILE_ZT_OVERRIDES <- c("C:/.../file.csv" = 1.25)
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
  if (length(FILE_ZT_OVERRIDES) > 0 && path %in% names(FILE_ZT_OVERRIDES)) {
    return(unname(FILE_ZT_OVERRIDES[path]))
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
#   "mouse_id"  = hold out one animal-in-one-condition (original behaviour)
#   "animal_id" = hold out the whole physical animal across ALL conditions
RF_FEATURES   <- BEHAVIOR_VARS
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

