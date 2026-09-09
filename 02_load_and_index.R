## ══════════════════════════════════════════════════════════════════════════
## Builds file_index (one row per CSV, with parsed condition metadata),
## then produces the per-file/per-day summary tables (daily_all, forage_only,
## pellets_total) that almost everything downstream is built from.
## ══════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════
# LOAD EVERYTHING
# ══════════════════════════════════════════════════════════════════════════

file_index <- map_dfr(CONDITION_FOLDERS, function(folder) {
  cond  <- parse_condition(basename(folder))
  files <- get_csvs(folder)
  if (length(files) == 0) return(NULL)
  tibble(
    path             = files,
    folder           = basename(folder),
    arena            = arena_id(files),
    type             = cond$type,
    light            = cond$light,
    effort           = cond$effort,
    diet             = cond$diet,
    day_num          = cond$day_num,
    has_explicit_day = cond$has_explicit_day
  )
})

cat("Found", nrow(file_index), "files across", length(unique(file_index$folder)), "condition folders\n")
print(count(file_index, type, light, effort, diet, day_num))

file_index <- file_index %>%
  mutate(
    baseline_phase = case_when(
      type == "forage" & has_explicit_day & day_num <= BASELINE_DAY_CUTOFF ~ "baseline",
      type == "forage" & has_explicit_day & day_num >  BASELINE_DAY_CUTOFF ~ "retest",
      TRUE ~ NA_character_
    ),
    mouse_id = case_when(
      type == "training" ~ paste("training", arena, sep = "_"),
      has_explicit_day   ~ paste(light, effort, diet, arena, sep = "_"),
      TRUE ~ paste(light, effort, diet, "rep", day_num, arena, sep = "_")
    ),
    # Assumes arena number = same physical mouse regardless
    # of light/effort/day, for a given diet. This is true FOR NOW.
    animal_id = paste(diet, arena, sep = "_")
  )

# Sanity check: confirm the ZT offset rules are on the correct files.
file_index %>%
  mutate(zt_offset_hr = pmap_dbl(list(path, type, light, effort, diet, day_num, arena), get_zt_offset)) %>%
  filter(zt_offset_hr != DEFAULT_ZT_OFFSET) %>%
  select(folder, arena, day_num, path, zt_offset_hr) %>%
  print(n = Inf)

# ══════════════════════════════════════════════════════════════════════════
# PER-FILE / PER-DAY SUMMARIES 
# This portion of the code may take a few minutes to run.
#
# There will be a warning message for a few files because we didn't have a 
# cup zone varaible at the start of experiment.
# ══════════════════════════════════════════════════════════════════════════

daily_all <- pmap_dfr(file_index, function(path, folder, arena, type, light, effort, diet,
                                           day_num, has_explicit_day, baseline_phase, mouse_id,
                                           animal_id, ...) {
  zt_offset <- get_zt_offset(path, type, light, effort, diet, day_num, arena)
  df <- read_arena(path, zt_offset)

  df %>%
    drop_incomplete_day() %>%
    group_by(day) %>%
    summarise(
      wheel          = sum(wheel_cycles,   na.rm = TRUE),
      pellets        = sum(pellets_earned, na.rm = TRUE),
      nose_pokes     = sum(nose_pokes,     na.rm = TRUE),
      licks          = sum(licks,          na.rm = TRUE),
      in_zone_time   = sum(in_zone,        na.rm = TRUE),
      # ADDED: cup-zone occupancy, summed the same way as the wheel zone
      in_zone_cup_time = if ("in_zone_cup" %in% names(df)) sum(in_zone_cup, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(mouse_id = mouse_id, animal_id = animal_id, type = type, light = light,
           effort = effort, diet = diet, day_num = day_num,
           has_explicit_day = has_explicit_day, baseline_phase = baseline_phase)
})

# Calculating additional features
daily_all <- daily_all %>%
  mutate(
    effort = factor(effort, levels = EFFORT_LEVELS),
    wheel_per_zone_sec       = wheel / in_zone_time,
    pellets_per_zone_sec     = pellets / in_zone_time,
    pellets_per_wheel        = pellets / wheel,
    pellets_per_zone_cup_sec = pellets / in_zone_cup_time
  )

# Everything below that's effort/diet/light-specific runs on forage_only, so
# training rows are not used. This may also take a few minutes to run.
forage_only <- daily_all %>% filter(type == "forage")

pellets_total <- pmap_dfr(file_index, function(path, folder, arena, type, light,
                                               effort, diet, day_num,
                                               has_explicit_day, baseline_phase,
                                               mouse_id, animal_id, ...) {
  zt_offset <- get_zt_offset(path, type, light, effort, diet, day_num, arena)
  df <- read_arena(path, zt_offset)

  tibble(
    mouse_id      = mouse_id,
    animal_id     = animal_id,
    type          = type,
    light         = light,
    effort        = factor(effort, levels = EFFORT_LEVELS),
    diet          = diet,
    day_num       = day_num,
    baseline_phase = baseline_phase,
    total_pellets = sum(df$pellets_earned, na.rm = TRUE)
  )
})

# Reorder the rows based off of mouse id
pellets_total_ordered <- pellets_total %>%
  mutate(
    arena_order = stringr::str_extract(mouse_id, "(?<=ARENA)(1b|2b|3b|1|2|3|4)$"),
    arena_order = factor(arena_order,
                         levels = c("1", "2", "3", "4", "1b", "2b", "3b"))
  ) %>%
  arrange(type, light, effort, diet, day_num, arena_order) %>%
  select(-arena_order)

# Save as excel for later
write_xlsx(pellets_total_ordered,
           file.path(BASE_DIR, "pellets_new_data.xlsx"))

## ══════════════════════════════════════════════════════════════════════════
## Mouse-level and condition-level summaries used across most later plots.
## ══════════════════════════════════════════════════════════════════════════

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

# One row per mouse per condition x day_num (mean across whatever intra-file
# days that file has, normally just 1). For explicit-day baseline/retest
# conditions this still keeps each day_num as its own row.
mouse_summary <- forage_only %>%
  group_by(mouse_id, animal_id, light, effort, diet, day_num, has_explicit_day, baseline_phase) %>%
  summarise(across(all_of(BEHAVIOR_VARS), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

condition_summary <- summarise_behaviors(mouse_summary, c("light", "effort", "diet"))
