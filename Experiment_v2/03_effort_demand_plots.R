## ══════════════════════════════════════════════════════════════════════════
## Effort and light comparison: per-behavior trend plots and the demand curve.
## One point per animal per light x effort condition (averaged over days).
## ══════════════════════════════════════════════════════════════════════════

if (!HAS_FORAGE_DATA) {
  message("03: no forage-condition data yet - skipping effort / light plots.")
} else {

  # color_key combines light + effort so each effort level within a light
  # condition gets its own shade, per CONDITION_COLOURS.
  add_color_key <- function(df) {
    df %>% mutate(color_key = factor(paste(light_group, effort, sep = "_"),
                                     levels = CONDITION_COLOUR_ORDER))
  }

  animal_plot_df    <- animal_summary %>% filter(!is.na(light_group)) %>% add_color_key()
  condition_plot_df <- summarise_behaviors(animal_plot_df, c("light_group", "effort", "diet")) %>%
    add_color_key()

  behavior_trend_plot <- function(animal_df, summary_df, y_col, y_label) {

    mean_col <- paste0(y_col, "_mean")
    sem_col  <- paste0(y_col, "_sem")

    ggplot(summary_df, aes(effort, .data[[mean_col]], color = color_key)) +
      geom_jitter(
        data = animal_df,
        aes(effort, .data[[y_col]], color = color_key),
        width = .12, size = 2, alpha = .6, inherit.aes = FALSE
      ) +
      geom_errorbar(
        aes(ymin = .data[[mean_col]] - .data[[sem_col]],
            ymax = .data[[mean_col]] + .data[[sem_col]]),
        width = .15
      ) +
      geom_point(size = 3) +
      facet_grid(diet ~ light_group) +
      scale_color_manual(values = CONDITION_COLOURS) +
      labs(x = "Effort", y = y_label, color = "Condition") +
      theme_classic(base_size = 12) +
      theme(panel.border = element_rect(fill = NA),
            strip.background = element_rect(fill = "grey95"))
  }

  # One trend plot per behaviour that has any data
  for (v in BEHAVIOR_VARS) {
    if (any(is.finite(animal_plot_df[[v]]))) {
      print(behavior_trend_plot(animal_plot_df, condition_plot_df, v, BEHAVIOR_LABELS[[v]]))
    }
  }

  # Demand curve: pellets earned vs effort requirement, one panel per light
  demand_summary_by_light <- animal_plot_df %>%
    group_by(light_group, effort, diet) %>%
    summarise(
      pellets_mean = mean(pellets, na.rm = TRUE),
      pellets_sem  = sem(pellets),
      .groups = "drop"
    )

  p_demand_by_light <- ggplot(demand_summary_by_light,
                              aes(x = effort, y = pellets_mean, group = diet, color = diet)) +
    geom_line(linewidth = 1) +
    geom_errorbar(aes(ymin = pellets_mean - pellets_sem, ymax = pellets_mean + pellets_sem), width = 0.1) +
    geom_point(size = 3) +
    facet_wrap(~ light_group) +
    labs(x = "Effort requirement", y = "Mean daily pellets earned",
         title = "Demand curve by light condition", color = "Diet") +
    theme_classic(base_size = 12) +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          strip.background = element_rect(fill = "grey95", color = "black"))

  print(p_demand_by_light)
}
