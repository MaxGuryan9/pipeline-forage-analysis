## ══════════════════════════════════════════════════════════════════════════
## Efficiency & Demand analysis (forage conditions): demand curve across
## light, running efficiency, peak running rate, zone -> zone latencies and
## day-to-day drift. Training latencies are in 04.
## ══════════════════════════════════════════════════════════════════════════

if (!HAS_FORAGE_DATA) {
  message("06: no forage-condition data yet - skipping efficiency / demand analysis.")
} else {

  forage_lg <- forage_used %>% filter(!is.na(light_group))

  # Shared look for the "<metric> by effort, faceted diet x light" boxplots
  box_by_effort <- function(df, y_col, y_label, title, log_y = FALSE) {
    df <- df %>% filter(is.finite(.data[[y_col]]), !is.na(effort))
    if (nrow(df) == 0) { message("Skipping '", title, "': no finite values."); return(invisible(NULL)) }
    p <- ggplot(df, aes(x = effort, y = .data[[y_col]], fill = effort)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.6) +
      geom_jitter(width = 0.15, size = 1.6, alpha = 0.6) +
      facet_grid(diet ~ light_group) +
      scale_fill_manual(values = COND_COLOURS) +
      labs(x = "Effort requirement", y = y_label, title = title) +
      theme_classic(base_size = 12) +
      theme(panel.border = element_rect(fill = NA, color = "black"),
            strip.background = element_rect(fill = "grey95", color = "black"),
            legend.position = "none")
    if (log_y) p <- p + scale_y_log10()
    p
  }

  # ════════════════════════════════════════════════════════════════════════
  # Demand curve by light: reward intake vs effort requirement
  # ════════════════════════════════════════════════════════════════════════
  demand_summary <- forage_lg %>%
    group_by(light_group, effort, diet, mouse_id) %>%
    summarise(mean_daily_pellets = mean_or_na(pellets), .groups = "drop") %>%
    group_by(light_group, effort, diet) %>%
    summarise(
      pellets_mean = mean(mean_daily_pellets, na.rm = TRUE),
      pellets_sem  = sem(mean_daily_pellets),
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
         title = "Demand curve \u2014 by light condition", color = "Diet", linetype = "Light", shape = "Light") +
    theme_classic(base_size = 12) +
    theme(panel.border = element_rect(fill = NA, color = "black"))

  print(p_demand_all)

  # ════════════════════════════════════════════════════════════════════════
  # Efficiency plots
  # ════════════════════════════════════════════════════════════════════════
  eff_df <- forage_lg %>%
    mutate(
      zone_sec_per_pellet     = in_zone_wheel_time / pellets,
      pellet_zone_sec_per_pellet = in_zone_pellet_time / pellets,
      zone_cup_sec_per_pellet = in_zone_cup_time / pellets,
      wheel_per_pellet        = wheel / pellets
    )

  efficiency_plots <- list(
    p_efficiency = box_by_effort(eff_df, "wheel_per_zone_sec", "Wheel turns per second in wheel zone",
                                 "Running efficiency in zone by effort level"),
    p_zone_per_pellet = box_by_effort(eff_df, "zone_sec_per_pellet", "Time in wheel zone per pellet earned (s)",
                                      "Wheel-zone dwell time per pellet by effort level"),
    p_pellet_zone_per_pellet = box_by_effort(eff_df, "pellet_zone_sec_per_pellet", "Time in pellet zone per pellet earned (s)",
                                             "Pellet-zone dwell time per pellet by effort level"),
    p_zone_cup_per_pellet = box_by_effort(eff_df, "zone_cup_sec_per_pellet", "Time in cup zone per pellet earned (s)",
                                          "Cup dwell time per pellet by effort level"),
    p_wheel_per_pellet = box_by_effort(eff_df, "wheel_per_pellet", "Wheel turns per pellet earned",
                                       "Running cost per pellet by effort level")
  )
  for (p in efficiency_plots) if (!is.null(p)) print(p)

  # ════════════════════════════════════════════════════════════════════════
  # Max number of wheel turns completed in a one-minute bin
  # (computed per file in 02 from the BIN_SIZE_SEC bins)
  # ════════════════════════════════════════════════════════════════════════
  max_bin_summary <- max_bin_all %>%
    filter(type == "forage", !is.na(light_group),
           is.null(FORAGE_DAYS_TO_USE) | day_num %in% FORAGE_DAYS_TO_USE) %>%
    group_by(mouse_id, animal_id, light_group, effort, diet) %>%
    summarise(max_turns_1min = mean_or_na(max_turns_1min), .groups = "drop")

  p_max_turns <- box_by_effort(max_bin_summary, "max_turns_1min", "Max wheel turns in a 1-min bin",
                               "Peak running rate (max turns/min) by effort level")
  if (!is.null(p_max_turns)) print(p_max_turns)

  # ════════════════════════════════════════════════════════════════════════
  # Zone -> zone latency (wheel -> cup, pellet -> cup)
  # How long after leaving the from-zone does the mouse reach the cup zone?
  # See ZONE_LATENCY_PAIRS in 00_config.R for the exact definition.
  # ════════════════════════════════════════════════════════════════════════
  latency_forage <- latency_all %>%
    filter(type == "forage", !is.na(light_group),
           is.null(FORAGE_DAYS_TO_USE) | day_num %in% FORAGE_DAYS_TO_USE)

  if (nrow(latency_forage) == 0) {
    message("No forage zone -> zone latency events found. Check that the zone columns are ",
            "present in your raw CSVs and that COLUMN_MAP matches the headers exactly.")
  } else {

    for (pr in unique(latency_forage$pair)) {
      lab <- ZONE_LATENCY_PAIRS$label[ZONE_LATENCY_PAIRS$pair == pr]
      sub <- latency_forage %>% filter(pair == pr)

      cat("\n=============================\n", lab, "latency\n")
      print(
        sub %>%
          group_by(light_group, effort, diet) %>%
          summarise(n_events = n(),
                    mean_latency_s = mean(latency_sec, na.rm = TRUE),
                    median_latency_s = median(latency_sec, na.rm = TRUE),
                    .groups = "drop")
      )

      # One row per mouse per condition (mean + median latency)
      latency_mouse_summary <- sub %>%
        group_by(mouse_id, animal_id, light_group, effort, diet) %>%
        summarise(mean_latency_sec   = mean(latency_sec, na.rm = TRUE),
                  median_latency_sec = median(latency_sec, na.rm = TRUE),
                  n_events           = n(), .groups = "drop") %>%
        filter(is.finite(median_latency_sec), median_latency_sec > 0)

      p_lat <- box_by_effort(latency_mouse_summary, "median_latency_sec",
                             paste0("Median ", lab, " latency (s)"),
                             paste0("Per-mouse ", lab, " latency"), log_y = TRUE)
      if (!is.null(p_lat)) print(p_lat)
    }
  }

  # ════════════════════════════════════════════════════════════════════════
  # Drift across days within each condition.
  # Only conditions with more than one day are shown.
  # ════════════════════════════════════════════════════════════════════════
  drift_candidates <- mouse_summary %>%
    filter(!is.na(light_group)) %>%
    group_by(light_group, effort, diet) %>%
    filter(n_distinct(day_num, na.rm = TRUE) > 1) %>%
    ungroup()

  if (nrow(drift_candidates) == 0) {
    message("No multi-day forage conditions found yet - skipping drift plot.")
  } else {

    drift_long <- drift_candidates %>%
      pivot_longer(all_of(BEHAVIOR_VARS), names_to = "behavior", values_to = "value") %>%
      mutate(behavior = factor(BEHAVIOR_LABELS[behavior], levels = unname(BEHAVIOR_LABELS))) %>%
      filter(is.finite(value), is.finite(day_num))

    if (nrow(drift_long) == 0) {
      message("No finite behavior values available for the drift plot.")
    } else {
      p_drift <- ggplot(drift_long, aes(x = day_num, y = value)) +
        geom_line(aes(group = mouse_id), alpha = 0.25) +
        geom_point(alpha = 0.5, size = 1.8) +
        stat_summary(fun = mean, geom = "line", color = "firebrick", linewidth = 1.1) +
        stat_summary(fun = mean, geom = "point", color = "firebrick", size = 2.8) +
        facet_grid(behavior ~ light_group + effort + diet, scales = "free_y") +
        scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]))) +
        labs(x = "Day", y = NULL, title = "Drift across days within each condition") +
        theme_classic(base_size = 11) +
        theme(panel.border = element_rect(fill = NA, color = "black"),
              strip.background = element_rect(fill = "grey95", color = "black"),
              strip.text = element_text(size = 8))
      print(p_drift)
    }
  }
}
