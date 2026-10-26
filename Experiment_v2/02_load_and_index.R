## ══════════════════════════════════════════════════════════════════════════
## Builds file_index (one row per CSV, with parsed condition metadata), then
## reads every file ONCE and produces all the per-file / per-day tables the
## rest of the pipeline uses:
##   daily_all       one row per mouse x day (all behaviours)
##   max_bin_all     peak wheel turns in a 1-min bin, per mouse x day
##   latency_all     every zone -> zone latency event (wheel->cup, pellet->cup)
##   pellets_total   total pellets per file
## ══════════════════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════════════════
# INDEX THE FILES
# ══════════════════════════════════════════════════════════════════════════

file_index <- map_dfr(CONDITION_FOLDERS, function(folder) {
  cond  <- parse_condition(basename(folder))
  files <- get_csvs(folder)
  if (length(files) == 0) return(NULL)
  tibble(
    path       = files,
    folder     = basename(folder),
    arena      = arena_id(files),
    type       = cond$type,
    light      = cond$light,
    effort     = cond$effort,
    diet       = cond$diet,
    replicate  = cond$replicate,
    file_day   = coalesce(parse_file_day(files, folder), cond$folder_day)
  )
})

if (is.null(file_index) || nrow(file_index) == 0) {
  stop("No CSV files found in any recognised folder under BASE_DIR:\n  ", BASE_DIR,
       "\nExpected folders like 'Baseline', 'training', 'low_effort_no_light_sucrose'.")
}

# An arena with several files in one folder and no "day N" in any name:
# number them 1, 2, ... in alphabetical order.
file_index <- file_index %>%
  group_by(folder, arena) %>%
  mutate(file_day = if (n() > 1 && all(is.na(file_day))) {
    message("No day labels for ", first(folder), " / ", first(arena),
            " - numbering its ", n(), " files by file name order.")
    as.integer(rank(path, ties.method = "first"))
  } else file_day) %>%
  ungroup() %>%
  mutate(file_day = coalesce(file_day, 1L))

file_index <- file_index %>%
  mutate(
    light_group = factor(unname(LIGHT_LABELS[light]), levels = unname(LIGHT_LABELS)),
    is_forage   = type == "forage",
    cond_key    = if_else(is_forage, paste(light, effort, diet, sep = "_"), type),
    # mouse_id = one animal in one condition (days of that condition share it)
    mouse_id    = case_when(
      !is_forage     ~ paste(type, arena, sep = "_"),
      replicate > 1  ~ paste(cond_key, paste0("rep", replicate), arena, sep = "_"),
      TRUE           ~ paste(cond_key, arena, sep = "_")
    ),
    # Assumes arena number = same physical mouse across conditions, for a given
    # diet. This is true FOR NOW.
    animal_id   = if_else(is.na(diet), arena, paste(diet, arena, sep = "_"))
  ) %>%
  select(-is_forage, -cond_key)

cat("Found", nrow(file_index), "files across", n_distinct(file_index$folder), "folders\n")
print(count(file_index, type, light, effort, diet, replicate, file_day), n = Inf)

# Parsing sanity checks
bad_forage <- file_index %>%
  filter(type == "forage", is.na(effort) | is.na(light_group)) %>%
  distinct(folder, effort, light)
if (nrow(bad_forage) > 0) {
  warning("Couldn't parse effort and/or a known light (", paste(unname(LIGHT_LABELS), collapse = "/"),
          ") from these folders - they won't appear in effort/light comparisons:\n",
          paste0("  ", bad_forage$folder, collapse = "\n"), call. = FALSE)
}

# Confirm the ZT offset rules are on the correct files
file_index %>%
  mutate(zt_offset_hr = pmap_dbl(list(path, type, light, effort, diet, file_day, arena), get_zt_offset)) %>%
  filter(zt_offset_hr != DEFAULT_ZT_OFFSET) %>%
  select(folder, arena, file_day, path, zt_offset_hr) %>%
  print(n = Inf)

# ══════════════════════════════════════════════════════════════════════════
# READ EACH FILE ONCE
# This may take a few minutes. A file that fails to read is reported and
# skipped rather than stopping the whole run.
# ══════════════════════════════════════════════════════════════════════════

process_file <- function(meta) {
  zt_offset <- get_zt_offset(meta$path, meta$type, meta$light, meta$effort,
                             meta$diet, meta$file_day, meta$arena)
  df_full <- read_arena(meta$path, zt_offset)
  df_days <- drop_incomplete_day(df_full)

  id_cols <- meta %>% select(mouse_id, animal_id, type, light, light_group, effort, diet, replicate)
  attach_meta <- function(tbl) if (nrow(tbl) == 0) NULL else bind_cols(tbl, id_cols)

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
    ) %>%
    mutate(day_num = meta$file_day + as.integer(day)) %>%
    select(-day)

  # peak wheel turns in any 1-min bin, per day
  max_bin <- df_days %>%
    group_by(day, bin) %>%
    summarise(turns_in_bin = sum_or_na(wheel_cycles), .groups = "drop") %>%
    group_by(day) %>%
    summarise(max_turns_1min = if (all(is.na(turns_in_bin))) NA_real_ else max(turns_in_bin, na.rm = TRUE),
              .groups = "drop") %>%
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

  list(
    daily         = attach_meta(daily),
    max_bin       = attach_meta(max_bin),
    latency       = attach_meta(lat),
    pellets_total = attach_meta(tibble(total_pellets = sum_or_na(df_full$pellets_earned),
                                       file_day = meta$file_day))
  )
}

results <- vector("list", nrow(file_index))
for (i in seq_len(nrow(file_index))) {
  meta <- file_index[i, ]
  cat(sprintf("[%d/%d] %s / %s\n", i, nrow(file_index), meta$folder, basename(meta$path)))
  results[[i]] <- tryCatch(
    process_file(meta),
    error = function(e) {
      warning("FAILED to process ", meta$path, ": ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

daily_all     <- map_dfr(results, "daily")
max_bin_all   <- map_dfr(results, "max_bin")
latency_all   <- map_dfr(results, "latency")
pellets_total <- map_dfr(results, "pellets_total")

if (nrow(daily_all) == 0) stop("No file could be processed - see the warnings above.")

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

write_xlsx(pellets_total_ordered, file.path(BASE_DIR, "pellets_new_data.xlsx"))

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
