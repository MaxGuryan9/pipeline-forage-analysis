## ══════════════════════════════════════════════════════════════════════════
## Libraries, file paths, constants, and color palettes.
## This must be run first.
## ══════════════════════════════════════════════════════════════════════════

needed_pkgs <- c("randomForest", "caret", "dplyr", "readxl")
missing_pkgs <- setdiff(needed_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(tidyverse)
library(ggplot2)
library(readr)
library(patchwork)
library(plotly)
library(writexl)
library(readxl)
library(ggpubr)
library(lme4)
library(lmerTest)
library(colorspace)

# ══════════════════════════════════════════════════════════════════════════
# CONFIG
# ══════════════════════════════════════════════════════════════════════════

# Relative to this project's working directory, so the same path works on
# any machine: open "Pipeline forage analysis.Rproj" in RStudio (which sets
# the working directory automatically), or run scripts with this project
# folder as the working directory. Only this line needs to change between
# computers/datasets - everything downstream is derived from it.
BASE_DIR <- "data"

# Every subfolder of BASE_DIR is treated as one "session batch". There are
# two families of folder names:
#
#   FORAGE folders   : e.g. "LD low effort sucrose", "LL (low) low effort sucrose 2"
#   TRAINING folders : e.g. "Training day 1", "Training day 2"
#
# Folders that don't exist yet are simply absent from list.dirs() 
# below hardcodes which conditions/days must be present
CONDITION_FOLDERS <- list.dirs(BASE_DIR, recursive = FALSE)

# All recordings start 1 hour after lights-on (ZT 1) and run ~23 hours,
# so every file is missing the same ~1 hour window: ZT 0 -> ZT 1.
DEFAULT_ZT_OFFSET <- 1

# BASELINE_DAY_CUTOFF are the baseline block and anything after that is the 
# later retest block. Change this one number if the baseline window length changes.
BASELINE_DAY_CUTOFF <- 3

# If a particular file's recording didn't actually start at ZT 1 (we started 
# late one day), add it here.
FILE_ZT_OVERRIDES <- c(

)

ZT_OFFSET_RULES <- tribble(
  ~type,      ~light, ~effort, ~diet,     ~day_num, ~arena_pattern, ~offset_hr, ~note,
  "forage",   "LD",   "low",   "sucrose", 1,    NA, 1 + 15/60,  "LD low sucrose day 1 started 15 min late",
  "forage",   "LD",   "low",   "sucrose", 2,    NA, 1 + 15/60,  "LD low effort sucrose day 2 started 15 min late",
  "forage",   "LD",   "low",   "sucrose", 3,    NA, 1 + 10/60,  "LD low effort sucrose day 3 started 10 min late",
  "forage",   "LL.*low",   "low",   "sucrose", 1,    NA, 1 + 5/60,  "LL (low) low effort sucrose started 5 min late",
   "forage", "LL.*high", "low", "sucrose", 1, NA, 1 + 4/60, "LL (high) low effort sucrose started 4 min late",
  "forage",   "LD",   "medium",   "sucrose", 1,    NA, 1 + 12/60,  "LD medium effort sucrose started 12 min late",
  "forage",   "LL.*low",   "medium",   "sucrose", 1,    NA, 1 + 5/60,  "LL (low) medium effort sucrose started 5 min late",
 #"forage",     "LD",     "low",    "sucrose",   "3", NA, , "LD low effort sucrose day 1 started"

  )

get_zt_offset <- function(path, type, light, effort, diet, day_num, arena) {

  # 1. Check for a specific file override first
  if (length(FILE_ZT_OVERRIDES) > 0 && path %in% names(FILE_ZT_OVERRIDES)) {
    return(unname(FILE_ZT_OVERRIDES[path]))
  }

  # 2. Check rule-based overrides, if any rules exist
  if (exists("ZT_OFFSET_RULES") && nrow(ZT_OFFSET_RULES) > 0) {

    for (i in seq_len(nrow(ZT_OFFSET_RULES))) {

      r <- ZT_OFFSET_RULES[i, ]

      type_ok   <- is.na(r$type) || identical(type, r$type)

      light_ok  <- is.na(r$light) ||
        (!is.na(light) &&
           str_detect(light, regex(r$light, ignore_case = TRUE)))

      effort_ok <- is.na(r$effort) || identical(effort, r$effort)

      diet_ok   <- is.na(r$diet) || identical(diet, r$diet)

      day_ok    <- is.na(r$day_num) ||
        identical(as.integer(day_num), as.integer(r$day_num))

      arena_ok  <- is.na(r$arena_pattern) ||
        str_detect(arena, regex(r$arena_pattern, ignore_case = TRUE))

      if (type_ok && light_ok && effort_ok && diet_ok && day_ok && arena_ok) {
        return(r$offset_hr)
      }
    }
  }

  # 3. If no override/rule applies, use the default
  DEFAULT_ZT_OFFSET
}

BIN_SIZE_SEC <- 60   # seconds per actogram bin

# In the next run of this experiment we will have shelter time too
COLUMN_MAP <- c(
  "Trial time"    = "trial_time",
  "pellet earned" = "pellets_earned",
  "licks"         = "licks",
  "wheel"         = "wheel_cycles",
  "nose pokes"    = "nose_pokes",
  "In zone"       = "in_zone",
  "In zone cup"   = "in_zone_cup"   
)

EFFORT_LEVELS <- c("low", "medium", "high")

COND_COLOURS <- c(
  low    = "#2CBBAB",
  medium = "#ED1A61",
  high   = "#3F000F"
)

LIGHT_COLOURS <- c(LD = "#4C6EF5", LL = "#F5A623")

# Shared palette for light comparisons: LD / LL Low / LL High 
# Base color per condition family - the "high effort" shade IS this color,
# lower efforts are progressively lighter tints of it.
CONDITION_BASE_COLOURS <- c(
  "LD"      = "grey20",
  "LL Low"  = "#1b9e77",
  "LL High" = "#d95f02"
)


make_effort_shades <- function(base_colour, effort_levels = EFFORT_LEVELS) {
  n <- length(effort_levels)
  # spread lightening amounts evenly from most lightened (low) to 0 (high = base color)
  amounts <- seq(0.75, 0, length.out = n)
  shades <- sapply(amounts, function(a) lighten(base_colour, amount = a))
  setNames(shades, effort_levels)
}

CONDITION_COLOURS <- imap(CONDITION_BASE_COLOURS, function(base_colour, cond_name) {
  shades <- make_effort_shades(base_colour)
  setNames(shades, paste(cond_name, names(shades), sep = "_"))
}) %>% unname() %>% unlist()

# Keeps legend entries in the right order (LD low->high, LL Low low->high, LL High low->high)
CONDITION_COLOUR_ORDER <- names(CONDITION_COLOURS)
