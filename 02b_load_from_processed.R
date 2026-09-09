## ══════════════════════════════════════════════════════════════════════════
## Alternative to 02_load_and_index.R: loads forage_only and mouse_summary
## directly from pre-computed summary files in processed/, instead of
## rebuilding them from raw arena CSVs in data/.
##
## Run 00_config.R and 01_helpers.R first - 01_helpers.R isn't used by this
## script directly, but 03/05/06 downstream need functions defined there
## (e.g. paired_pairwise_t()).
## Use this INSTEAD OF 02_load_and_index.R, not in addition to it.
##
## LIMITATION: forage_only/mouse_summary only have daily-level totals, not
## raw per-timestamp data. Anything downstream that needs file_index or the
## raw arena CSVs directly - the peak-running-rate and wheel->cup latency
## sections of 06_efficiency_demand_analysis.R, and all of
## 04_training_summary.R (which needs daily_all, not built here) - will
## still fail when loading this way. Once real raw arena CSVs are available
## under data/, switch back to running 02_load_and_index.R instead of this
## script, and stop skipping those sections.
## ══════════════════════════════════════════════════════════════════════════

forage_only <- read_excel("processed/forage_only.xlsx") %>%
  mutate(effort = factor(effort, levels = EFFORT_LEVELS))

mouse_summary <- read_excel("processed/mouse_summary.xlsx") %>%
  mutate(effort = factor(effort, levels = EFFORT_LEVELS))

cat("Loaded forage_only (", nrow(forage_only), "rows) and mouse_summary (",
    nrow(mouse_summary), "rows) from processed/\n")

# 02_load_and_index.R defines these two as part of building mouse_summary
# from scratch; since this script bypasses that, 03/06 need them defined
# here too.
BEHAVIOR_VARS <- c("wheel", "pellets", "nose_pokes", "licks", "in_zone_time", "in_zone_cup_time")

summarise_behaviors <- function(df, group_vars) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(
      all_of(BEHAVIOR_VARS),
      list(mean = ~ mean(.x, na.rm = TRUE),
           sem  = ~ sd(.x,   na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
      .names = "{.col}_{.fn}"
    ), .groups = "drop")
}
