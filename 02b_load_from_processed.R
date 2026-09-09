## ══════════════════════════════════════════════════════════════════════════
## Alternative to 02_load_and_index.R: loads forage_only and mouse_summary
## directly from pre-computed summary files in processed/, instead of
## rebuilding them from raw arena CSVs in data/.
##
## Run 00_config.R first (needed for library(readxl) and EFFORT_LEVELS).
## Use this INSTEAD OF 02_load_and_index.R, not in addition to it.
## ══════════════════════════════════════════════════════════════════════════

forage_only <- read_excel("processed/forage_only.xlsx") %>%
  mutate(effort = factor(effort, levels = EFFORT_LEVELS))

mouse_summary <- read_excel("processed/mouse_summary.xlsx") %>%
  mutate(effort = factor(effort, levels = EFFORT_LEVELS))

cat("Loaded forage_only (", nrow(forage_only), "rows) and mouse_summary (",
    nrow(mouse_summary), "rows) from processed/\n")
