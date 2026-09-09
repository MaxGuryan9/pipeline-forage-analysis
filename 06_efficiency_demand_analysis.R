## ══════════════════════════════════════════════════════════════════════════
## Efficiency & Demand (part 2) analysis.
## ══════════════════════════════════════════════════════════════════════════

# Keep LD high-effort rows 
forage_ld3_ll <- forage_only %>%
  mutate(
    light_group = case_when(
      light == "LD" & (!has_explicit_day | day_num == 3 | effort == "High") ~ "LD Day 3",
      str_detect(light, regex("LL", ignore_case = TRUE)) &
        str_detect(light, regex("low",  ignore_case = TRUE)) ~ "LL Low",
      str_detect(light, regex("LL", ignore_case = TRUE)) &
        str_detect(light, regex("high", ignore_case = TRUE)) ~ "LL High",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(light_group)) %>%
  mutate(light_group = factor(light_group, levels = c("LD Day 3", "LL Low", "LL High")))

# ══════════════════════════════════════════════════════════════════════════
# Demand curve by light: reward intake vs effort requirement 
# ══════════════════════════════════════════════════════════════════════════

demand_summary <- forage_ld3_ll %>%
  group_by(light_group, effort, diet, mouse_id) %>%
  summarise(mean_daily_pellets = mean(pellets, na.rm = TRUE), .groups = "drop") %>%
  group_by(light_group, effort, diet) %>%
  summarise(
    pellets_mean = mean(mean_daily_pellets, na.rm = TRUE),
    pellets_sem  = sd(mean_daily_pellets, na.rm = TRUE) / sqrt(sum(!is.na(mean_daily_pellets))),
    .groups = "drop"
  )

pd <- position_dodge(width = 0.15)

p_demand_all <- ggplot(demand_summary,
                       aes(x = effort, y = pellets_mean, color = diet, linetype = light_group,
                           group = interaction(diet, light_group))) +
  geom_line(linewidth = 1, position = pd) +
  geom_errorbar(aes(ymin = pellets_mean - pellets_sem, ymax = pellets_mean + pellets_sem),
                width = 0.12, position = pd) +
  geom_point(aes(shape = light_group), size = 3, position = pd) +
  labs(x = "Effort requirement", y = "Mean daily pellets earned",
       title = "Demand curve \u2014 LD Day 3 vs LL Low vs LL High", color = "Diet", linetype = "Light", shape = "Light") +
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(fill = NA, color = "black"))

print(p_demand_all)

# ══════════════════════════════════════════════════════════════════════════
# Running efficiency plots
# ══════════════════════════════════════════════════════════════════════════

p_efficiency <- forage_ld3_ll %>%
  filter(is.finite(wheel_per_zone_sec)) %>%
  ggplot(aes(x = effort, y = wheel_per_zone_sec, fill = effort)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 1.6, alpha = 0.6) +
  facet_grid(diet ~ light_group) +
  scale_fill_manual(values = COND_COLOURS) +
  labs(x = "Effort requirement", y = "Wheel turns per second in zone",
       title = "Running efficiency in zone by effort level") +
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(fill = "grey95", color = "black"),
        legend.position = "none")

print(p_efficiency)

p_zone_per_pellet <- forage_ld3_ll %>%
  mutate(zone_sec_per_pellet = in_zone_time / pellets) %>%
  filter(is.finite(zone_sec_per_pellet)) %>%
  ggplot(aes(x = effort, y = zone_sec_per_pellet, fill = effort)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 1.6, alpha = 0.6) +
  facet_grid(diet ~ light_group) +
  scale_fill_manual(values = COND_COLOURS) +
  labs(x = "Effort requirement", y = "Time in zone per pellet earned",
       title = "Zone dwell time per pellet by effort level") +
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(fill = "grey95", color = "black"),
        legend.position = "none")

print(p_zone_per_pellet)

p_zone_cup_per_pellet <- forage_ld3_ll %>%
  mutate(zone_cup_sec_per_pellet = in_zone_cup_time / pellets) %>%
  filter(is.finite(zone_cup_sec_per_pellet)) %>%
  ggplot(aes(x = effort, y = zone_cup_sec_per_pellet, fill = effort)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 1.6, alpha = 0.6) +
  facet_grid(diet ~ light_group) +
  scale_fill_manual(values = COND_COLOURS) +
  labs(x = "Effort requirement", y = "Time in cup zone per pellet earned",
       title = "Cup dwell time per pellet by effort level") +
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(fill = "grey95", color = "black"),
        legend.position = "none")

print(p_zone_cup_per_pellet)

p_wheel_per_pellet <- forage_ld3_ll %>%
  mutate(wheel_per_pellet = wheel / pellets) %>%
  filter(is.finite(wheel_per_pellet)) %>%
  ggplot(aes(x = effort, y = wheel_per_pellet, fill = effort)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 1.6, alpha = 0.6) +
  facet_grid(diet ~ light_group) +
  scale_fill_manual(values = COND_COLOURS) +
  labs(x = "Effort requirement", y = "Wheel turns per pellet earned",
       title = "Running cost per pellet by effort level") +
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(fill = "grey95", color = "black"),
        legend.position = "none")

print(p_wheel_per_pellet)

# ══════════════════════════════════════════════════════════════════════════
# Max number of wheel turns completed in a one-minute bin.
# This part may take a few minutes to run.
#
# NEEDS RAW DATA: this and the wheel->cup latency section below read
# file_index and the raw arena CSVs directly - they can't be computed from
# the daily-level forage_only/mouse_summary tables. If you loaded data via
# 02b_load_from_processed.R instead of 02_load_and_index.R, this will error
# with "object 'file_index' not found." Don't skip this section once real
# raw arena CSVs are available under data/ - switch back to running
# 02_load_and_index.R first.
# ══════════════════════════════════════════════════════════════════════════
# read_arena() already computes `bin = floor(trial_time / BIN_SIZE_SEC)`
# (60-sec bins, same as the actograms), so this reuses that column.

if (!exists("file_index")) {

  message("file_index not found - skipping peak running rate and wheel->cup ",
          "latency (both need raw arena CSVs via 02_load_and_index.R, not ",
          "02b_load_from_processed.R). Re-run this script after loading real ",
          "raw data to get these sections.")

} else {

max_bin_all <- pmap_dfr(file_index, function(path, folder, arena, type, light, effort, diet,
                                             day_num, has_explicit_day, baseline_phase, mouse_id,
                                             animal_id, ...) {
  zt_offset <- get_zt_offset(path, type, light, effort, diet, day_num, arena)
  df <- read_arena(path, zt_offset)

  df %>%
    drop_incomplete_day() %>%
    group_by(day, bin) %>%
    summarise(turns_in_bin = sum(wheel_cycles, na.rm = TRUE), .groups = "drop") %>%
    group_by(day) %>%
    summarise(max_turns_1min = max(turns_in_bin, na.rm = TRUE), .groups = "drop") %>%
    mutate(mouse_id = mouse_id, animal_id = animal_id, type = type, light = light,
           effort = effort, diet = diet, day_num = day_num,
           has_explicit_day = has_explicit_day, baseline_phase = baseline_phase)
})

max_bin_all <- max_bin_all %>%
  mutate(effort = factor(effort, levels = EFFORT_LEVELS))

# One row per mouse per condition, same collapsing as mouse_summary
max_bin_summary <- max_bin_all %>%
  filter(type == "forage") %>%
  group_by(mouse_id, animal_id, light, effort, diet, day_num, has_explicit_day) %>%
  summarise(max_turns_1min = mean(max_turns_1min, na.rm = TRUE), .groups = "drop")

# Same LD Day 3 / LL Low / LL High grouping as forage_ld3_ll
max_bin_ld3_ll <- max_bin_summary %>%
  mutate(
    light_group = case_when(
      light == "LD" & (!has_explicit_day | day_num == 3) ~ "LD Day 3",
      str_detect(light, regex("LL", ignore_case = TRUE)) &
        str_detect(light, regex("low",  ignore_case = TRUE)) ~ "LL Low",
      str_detect(light, regex("LL", ignore_case = TRUE)) &
        str_detect(light, regex("high", ignore_case = TRUE)) ~ "LL High",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(light_group)) %>%
  mutate(light_group = factor(light_group, levels = c("LD Day 3", "LL Low", "LL High")))

p_max_turns <- max_bin_ld3_ll %>%
  filter(is.finite(max_turns_1min)) %>%
  ggplot(aes(x = effort, y = max_turns_1min, fill = effort)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.15, size = 1.6, alpha = 0.6) +
  facet_grid(diet ~ light_group) +
  scale_fill_manual(values = COND_COLOURS) +
  labs(x = "Effort requirement", y = "Max wheel turns in a 1-min bin",
       title = "Peak running rate (max turns/min) by effort level") +
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(fill = "grey95", color = "black"),
        strip.text.x = element_blank(),
        legend.position = "none")

print(p_max_turns)

# ══════════════════════════════════════════════════════════════════════════
# Wheel -> cup latency
# How long after leaving the wheel zone does the mouse reach the cup zone?
# Every wheel-zone EXIT is paired with the very next cup-zone ENTRY in that
# same file.
# This will take a few minutes to run.
# ══════════════════════════════════════════════════════════════════════════

latency_all <- pmap_dfr(file_index, function(path, folder, arena, type, light, effort, diet,
                                              day_num, has_explicit_day, baseline_phase, mouse_id,
                                              animal_id, ...) {
  zt_offset <- get_zt_offset(path, type, light, effort, diet, day_num, arena)
  lat <- compute_wheel_to_cup_latency(path, zt_offset)
  if (nrow(lat) == 0) return(NULL)
  lat %>%
    mutate(mouse_id = mouse_id, animal_id = animal_id, type = type, light = light,
           effort = effort, diet = diet, day_num = day_num,
           has_explicit_day = has_explicit_day, baseline_phase = baseline_phase)
})

if (nrow(latency_all) == 0) {
  message("No wheel->cup latency events found. Check that 'in_zone' and ",
          "'in_zone_cup' are both present in your raw CSVs and that the ",
          "COLUMN_MAP key for the cup zone matches your header exactly.")
} else {

  latency_forage <- latency_all %>%
    filter(type == "forage") %>%
    mutate(effort = factor(effort, levels = EFFORT_LEVELS))

  cat("\n=============================\n")
  cat("Wheel -> cup latency\n")
  print(
    latency_forage %>%
      group_by(light, effort, diet) %>%
      summarise(
        n_events        = n(),
        mean_latency_s  = mean(latency_sec, na.rm = TRUE),
        median_latency_s = median(latency_sec, na.rm = TRUE),
        .groups = "drop"
      )
  )

  # One row per mouse per condition (mean + median latency), same shape as
  # mouse_summary.
  latency_mouse_summary <- latency_forage %>%
    group_by(mouse_id, animal_id, light, effort, diet, day_num, has_explicit_day, baseline_phase) %>%
    summarise(
      mean_latency_sec   = mean(latency_sec, na.rm = TRUE),
      median_latency_sec = median(latency_sec, na.rm = TRUE),
      n_events           = n(),
      .groups = "drop"
    )
}

p_latency_mouse <- latency_mouse_summary %>%
  filter(is.finite(median_latency_sec),
         median_latency_sec > 0) %>%
  ggplot(aes(effort, median_latency_sec, fill = effort)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.12, size = 2, alpha = 0.8) +
  facet_grid(diet ~ light) +
  scale_fill_manual(values = COND_COLOURS) +
  scale_y_log10() +
  labs(
    x = "Effort requirement",
    y = "Median wheel \u2192 cup latency (s)",
    title = "Per-mouse wheel \u2192 cup latency"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.border = element_rect(fill = NA, colour = "black"),
    strip.background = element_rect(fill = "grey95", colour = "black"),
    legend.position = "none"
  )

print(p_latency_mouse)

} # end if (exists("file_index"))

# ══════════════════════════════════════════════════════════════════════════
# Baseline drift across repeat days.
# ══════════════════════════════════════════════════════════════════════════
drift_candidates <- mouse_summary %>%
  group_by(light, effort, diet) %>%
  filter(n_distinct(day_num, na.rm = TRUE) > 1) %>%
  ungroup()

if (nrow(drift_candidates) > 0) {
  
  drift_long <- drift_candidates %>%
    pivot_longer(
      all_of(BEHAVIOR_VARS),
      names_to = "behavior",
      values_to = "value"
    ) %>%
    mutate(
      behavior = recode(
        behavior,
        pellets          = "Pellets",
        wheel            = "Wheel",
        nose_pokes       = "Nose Pokes",
        licks            = "Licks",
        in_zone_time     = "In Zone",
        in_zone_cup_time = "In Zone (Cup)"
      )
    ) %>%
    # Remove missing/non-finite behavior values before plotting
    filter(
      is.finite(value),
      is.finite(day_num)
    )
  
  # Report how many rows were removed
  n_removed <- nrow(drift_candidates) * length(BEHAVIOR_VARS) -
    nrow(drift_long)
  
  if (n_removed > 0) {
    message(
      "Drift plot: removed ",
      n_removed,
      " rows with missing/non-finite behavior values."
    )
  }
  
  if (nrow(drift_long) > 0) {
    
    p_drift <- ggplot(
      drift_long,
      aes(x = day_num, y = value)
    ) +
        geom_line(
        aes(group = mouse_id),
        alpha = 0.25
      ) +
      
      geom_point(
        alpha = 0.5,
        size = 1.8
      ) +
      
      stat_summary(
        fun = mean,
        geom = "line",
        color = "firebrick",
        linewidth = 1.1
      ) +
      
      stat_summary(
        fun = mean,
        geom = "point",
        color = "firebrick",
        size = 2.8
      ) +
      
      facet_grid(
        behavior ~ light + effort + diet,
        scales = "free_y"
      ) +
      
      scale_x_continuous(
        breaks = function(x) unique(round(x))
      ) +
      
      labs(
        x = "Day / repeat #",
        y = NULL,
        title = "Drift across repeated / multi-day baseline sessions"
      ) +
      
      theme_classic(base_size = 11) +
      
      theme(
        panel.border = element_rect(
          fill = NA,
          color = "black"
        ),
        strip.background = element_rect(
          fill = "grey95",
          color = "black"
        ),
        strip.text = element_text(size = 8)
      )
    
    print(p_drift)
    
  } else {
    
    message(
      "No finite behavior values available for the drift plot."
    )
  }
  
} else {
  
  message(
    "No repeated/multi-day conditions found yet - skipping drift plot."
  )
}
