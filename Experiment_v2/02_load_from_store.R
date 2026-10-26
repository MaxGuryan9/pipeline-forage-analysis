## ══════════════════════════════════════════════════════════════════════════
## Loads the Parquet store (built by 02a_ingest_new_data.R) into the tables
## the rest of the pipeline uses:
##   daily_all       one row per mouse x day (all behaviours + chow)
##   max_bin_all     peak wheel turns in a 1-min bin, per mouse x day
##   latency_all     every zone -> zone latency event (wheel->cup, pellet->cup)
##   pellets_total   total pellets per source file
## ══════════════════════════════════════════════════════════════════════════

check_store()

daily_all <- read_store("daily")
if (nrow(daily_all) == 0) {
  stop("The store is empty - put CSVs in ", INCOMING_DIR, " and run 02a_ingest_new_data.R first.")
}

add_light_group <- function(df) {
  df %>% mutate(light_group = factor(unname(LIGHT_LABELS[light]), levels = unname(LIGHT_LABELS)))
}

daily_all <- daily_all %>%
  add_light_group() %>%
  mutate(standard_chow_eaten = standard_chow_start - standard_chow_end)

max_bin_all <- daily_all %>%
  select(mouse_day_id, mouse_id, animal_id, type, light, light_group, effort, diet, replicate,
         day_num, max_turns_1min)

latency_all <- read_store("latency")
if (nrow(latency_all) > 0) latency_all <- add_light_group(latency_all)

pellets_total <- read_store("log") %>%
  add_id_cols() %>%
  add_light_group() %>%
  select(mouse_id, animal_id, type, light, light_group, effort, diet, replicate, file_day,
         total_pellets, source_file)

cat("Loaded", nrow(daily_all), "mouse-days from", nrow(pellets_total), "files\n")
print(count(daily_all, type, light, effort, diet, replicate, day_num), n = Inf)

# Calculating additional features
daily_all <- daily_all %>%
  mutate(
    effort = factor(effort, levels = EFFORT_LEVELS),
    wheel_per_zone_sec       = wheel / in_zone_wheel_time,
    pellets_per_zone_sec     = pellets / in_zone_wheel_time,
    pellets_per_wheel        = pellets / wheel,
    pellets_per_zone_cup_sec = pellets / in_zone_cup_time
  )
max_bin_all <- max_bin_all %>% mutate(max_turns_1min = if_else(is.finite(max_turns_1min), max_turns_1min, NA_real_),
                                      effort = factor(effort, levels = EFFORT_LEVELS))
if (nrow(latency_all) > 0) latency_all <- latency_all %>% mutate(effort = factor(effort, levels = EFFORT_LEVELS))

# ── Save total pellets per file as excel ──────────────────────────────────
pellets_total_ordered <- pellets_total %>%
  mutate(arena_num = suppressWarnings(as.integer(str_extract(mouse_id, "(?<=ARENA)\\d+")))) %>%
  arrange(type, light_group, effort, diet, replicate, file_day, arena_num, mouse_id) %>%
  select(-arena_num)

write_xlsx(pellets_total_ordered, file.path(DATA_DIR, "pellets_new_data.xlsx"))

# ══════════════════════════════════════════════════════════════════════════
# SPLIT BY EXPERIMENT PHASE
# ══════════════════════════════════════════════════════════════════════════

baseline_only <- daily_all %>% filter(type == "baseline")
training_only <- daily_all %>% filter(type == "training")

# Everything effort / light-specific (03, 05, 06, 07) runs on forage rows.
forage_only <- daily_all %>% filter(type == "forage")
forage_used <- forage_only %>% filter(is.null(FORAGE_DAYS_TO_USE) | day_num %in% FORAGE_DAYS_TO_USE)

HAS_FORAGE_DATA <- nrow(forage_used) > 0
if (!HAS_FORAGE_DATA) {
  message("No forage-condition data yet (only: ", paste(unique(daily_all$type), collapse = ", "),
          ") - scripts 03, 05, 06 and 07 will skip themselves.")
}

## ══════════════════════════════════════════════════════════════════════════
## Mouse-level and condition-level summaries used across the forage plots.
## ══════════════════════════════════════════════════════════════════════════

summarise_behaviors <- function(df, group_vars) {
  df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(across(
      all_of(BEHAVIOR_VARS),
      list(mean = mean_or_na, sem = sem),
      .names = "{.col}_{.fn}"
    ), .groups = "drop")
}

# One row per mouse (one animal in one condition) per day
mouse_summary <- forage_used %>%
  group_by(mouse_id, animal_id, light, light_group, effort, diet, replicate, day_num) %>%
  summarise(across(all_of(BEHAVIOR_VARS), mean_or_na), .groups = "drop")

# One row per physical animal per light x effort x diet (averaged over days
# and repeats). This is what the repeated-measures comparisons use.
animal_summary <- mouse_summary %>%
  group_by(animal_id, light, light_group, effort, diet) %>%
  summarise(across(all_of(BEHAVIOR_VARS), mean_or_na), .groups = "drop")

condition_summary <- summarise_behaviors(animal_summary, c("light_group", "effort", "diet"))
