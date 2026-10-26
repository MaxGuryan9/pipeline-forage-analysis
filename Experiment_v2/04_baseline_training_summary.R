# ══════════════════════════════════════════════════════════════════════════
# BASELINE
# ══════════════════════════════════════════════════════════════════════════
## ══════════════════════════════════════════════════════════════════════════
## Baseline (2 days) and training (trial days) summaries.
##   - Baseline : day-vs-day paired comparison of every behaviour
##   - Training : every behaviour across training days
##   - Pellet-zone time, cup-zone time, and latency are plotted across
##     baseline + training to show learning across the task
## ══════════════════════════════════════════════════════════════════════════
# ════════════════════════════════════════════════════════════════════════
# BASELINE + TRAINING: ZONE TIME AND LATENCY
# ════════════════════════════════════════════════════════════════════════

# Combine baseline and training data so learning can be visualized
# continuously from baseline through training.
baseline_training <- bind_rows(
  baseline_only %>%
    filter(animal_id != "4b", day_num != 2) %>% # removing day 2 for now
    mutate(
      phase = "Baseline",
      phase_day = day_num
    ),
  training_only %>%
    filter(animal_id != "4b") %>%
    mutate(
      phase = "Training",
      phase_day = day_num
    )
)

# Explicit session order:
# Baseline 1 -> Baseline 2 -> Training 1 -> Training 2 ...
baseline_days_order <- sort(unique(baseline_only$day_num))
training_days_order <- sort(unique(training_only$day_num))

baseline_days_order <- sort(unique(
  baseline_only %>%
    filter(day_num != 2) %>%
    pull(day_num)
))

training_days_order <- sort(unique(training_only$day_num))

session_levels <- c(
  paste0("Baseline ", baseline_days_order),
  paste0("Training ", training_days_order)
)

baseline_training <- baseline_training %>%
  mutate(
    session_label = paste0(phase, " ", phase_day),
    session = match(session_label, session_levels)
  )

# ── Helper for baseline + training plots ───────────────────────────────
plot_baseline_training <- function(data, value_var, y_label, title,
                                   log_y = FALSE) {
  
  plot_data <- data %>%
    filter(is.finite(.data[[value_var]]))
  
  if (nrow(plot_data) == 0) return(NULL)
  
  p <- ggplot(plot_data,
              aes(x = session, y = .data[[value_var]])) +
    geom_line(aes(group = animal_id), alpha = 0.3) +
    geom_point(alpha = 0.5, size = 1.8) +
    stat_summary(fun = mean, geom = "line",
                 color = "firebrick", linewidth = 1.1,
                 aes(group = 1)) +
    stat_summary(fun = mean, geom = "point",
                 color = "firebrick", size = 2.8) +
    stat_summary(fun.data = mean_se, geom = "errorbar",
                 color = "firebrick", width = 0.15) +
    scale_x_continuous(
      breaks = seq_along(session_levels),
      labels = session_levels
    ) +
    labs(
      x = "Session",
      y = y_label,
      title = title
    ) +
    theme_classic(base_size = 11) +
    theme(
      panel.border = element_rect(fill = NA, color = "black"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  if (log_y) {
    p <- p + scale_y_log10()
  }
  
  p
}

# ── Time spent in the pellet zone: baseline + training ─────────────────
cat("\nTime in pellet zone (s), baseline + training:\n")
print(
  baseline_training %>%
    group_by(session_label, session) %>%
    summarise(
      n_animals = n_distinct(animal_id),
      mean_s = mean_or_na(in_zone_pellet_time),
      sem_s = sem(in_zone_pellet_time),
      .groups = "drop"
    ) %>%
    arrange(session)
)

p_pellet_zone_time <- plot_baseline_training(
  baseline_training,
  "in_zone_pellet_time",
  BEHAVIOR_LABELS[["in_zone_pellet_time"]],
  "Time in pellet zone: baseline through training"
)

if (!is.null(p_pellet_zone_time)) print(p_pellet_zone_time)

# ── Time spent in the cup zone: baseline + training ────────────────────
if ("in_zone_cup_time" %in% names(baseline_training)) {
  
  cat("\nTime in cup zone (s), baseline + training:\n")
  print(
    baseline_training %>%
      group_by(session_label, session) %>%
      summarise(
        n_animals = n_distinct(animal_id),
        mean_s = mean_or_na(in_zone_cup_time),
        sem_s = sem(in_zone_cup_time),
        .groups = "drop"
      ) %>%
      arrange(session)
  )
  
  p_cup_zone_time <- plot_baseline_training(
    baseline_training,
    "in_zone_cup_time",
    BEHAVIOR_LABELS[["in_zone_cup_time"]],
    "Time in cup zone: baseline through training"
  )
  
  if (!is.null(p_cup_zone_time)) print(p_cup_zone_time)
}

# ════════════════════════════════════════════════════════════════════════
# LATENCY: BASELINE + TRAINING
# Pellet zone -> cup zone only
# ════════════════════════════════════════════════════════════════════════

baseline_latency <- latency_all %>%
  filter(type == "baseline", animal_id != "4b", day_num != 2)

training_latency <- latency_all %>%
  filter(type == "training", animal_id != "4b")

all_latency <- bind_rows(
  baseline_latency %>% mutate(phase = "Baseline"),
  training_latency %>% mutate(phase = "Training")
)

if (nrow(all_latency) == 0) {
  message("04: no zone -> zone latency events in the baseline or training files.")
} else {
  
  # One row per mouse per day: median (robust to the long tail) + mean + n
  all_latency_summary <- all_latency %>%
    group_by(pair, mouse_id, animal_id, type, phase, day_num) %>%
    summarise(
      median_latency_sec = median(latency_sec),
      mean_latency_sec = mean(latency_sec),
      n_events = n(),
      .groups = "drop"
    ) %>%
    mutate(
      session_label = paste0(phase, " ", day_num),
      session = match(session_label, session_levels)
    )
  
  # Only plot pellet -> cup latency.
  # Wheel latency and other latency pairs are removed.
  pellet_cup_pair <- all_latency_summary %>%
    filter(
      grepl("pellet", tolower(pair)) &
        grepl("cup", tolower(pair))
    )
  
  if (nrow(pellet_cup_pair) > 0) {
    
    lab <- ZONE_LATENCY_PAIRS$label[
      ZONE_LATENCY_PAIRS$pair %in% unique(pellet_cup_pair$pair)
    ]
    
    if (length(lab) == 0) {
      lab <- "Pellet zone -> cup zone"
    } else {
      lab <- lab[1]
    }
    
    cat("\n", lab, " (baseline + training), median latency per session:\n",
        sep = "")
    
    print(
      pellet_cup_pair %>%
        group_by(session_label, session) %>%
        summarise(
          n_animals = n_distinct(animal_id),
          mean_of_median_s = mean(median_latency_sec),
          sem_s = sem(median_latency_sec),
          events = sum(n_events),
          .groups = "drop"
        ) %>%
        arrange(session)
    )
    
    # Plot directly rather than using plot_across_days(), because
    # baseline and training have separate day_num values.
    p_lat <- ggplot(
      pellet_cup_pair %>%
        filter(median_latency_sec > 0),
      aes(x = session, y = median_latency_sec)
    ) +
      geom_line(aes(group = animal_id), alpha = 0.3) +
      geom_point(alpha = 0.5, size = 1.8) +
      stat_summary(
        fun = mean,
        geom = "line",
        color = "firebrick",
        linewidth = 1.1,
        aes(group = 1)
      ) +
      stat_summary(
        fun = mean,
        geom = "point",
        color = "firebrick",
        size = 2.8
      ) +
      stat_summary(
        fun.data = mean_se,
        geom = "errorbar",
        color = "firebrick",
        width = 0.15
      ) +
      scale_x_continuous(
        breaks = seq_along(session_levels),
        labels = session_levels
      ) +
      scale_y_log10() +
      labs(
        x = "Session",
        y = paste0("Median ", lab, " (s)"),
        title = paste0(lab, ": baseline through training")
      ) +
      theme_classic(base_size = 11) +
      theme(
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    print(p_lat)
    
    day_effect_test(
      pellet_cup_pair,
      "median_latency_sec",
      label = paste0(lab, " latency")
    )
  }
}

