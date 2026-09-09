## ══════════════════════════════════════════════════════════════════════════
## Training-day performance across days. WE DO NOT HAVE THIS DATA YET.
## ══════════════════════════════════════════════════════════════════════════

# Extend as the training analysis needs grow.
# One row per mouse per training day, tracked by arena (same arena = same
# mouse across training days).
training_only <- daily_all %>% filter(type == "training")

if (nrow(training_only) > 0) {

  training_summary <- training_only %>%
    group_by(mouse_id, day_num) %>%
    summarise(across(c(wheel, pellets, nose_pokes, licks, in_zone_time),
                      ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    pivot_longer(c(wheel, pellets, nose_pokes, licks, in_zone_time),
                 names_to = "behavior", values_to = "value") %>%
    mutate(behavior = recode(behavior,
                             pellets      = "Pellets",
                             wheel        = "Wheel",
                             nose_pokes   = "Nose Pokes",
                             licks        = "Licks",
                             in_zone_time = "In Zone"))

  p_training <- ggplot(training_summary, aes(x = day_num, y = value)) +
    geom_line(aes(group = mouse_id), alpha = 0.3) +
    geom_point(alpha = 0.5, size = 1.8) +
    stat_summary(fun = mean, geom = "line", color = "firebrick", linewidth = 1.1) +
    stat_summary(fun = mean, geom = "point", color = "firebrick", size = 2.8) +
    facet_wrap(~ behavior, scales = "free_y") +
    scale_x_continuous(breaks = function(x) unique(round(x))) +
    labs(x = "Training day", y = NULL, title = "Training performance across days") +
    theme_classic(base_size = 11) +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          strip.background = element_rect(fill = "grey95", color = "black"))

  print(p_training)
} else {
  message("No training-day folders found yet - skipping training summary.")
}
