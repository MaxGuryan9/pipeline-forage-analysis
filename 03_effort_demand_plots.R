## ══════════════════════════════════════════════════════════════════════════
## Effort and light comparison : per-behavior trend plots and the
## day-resolved demand curve.
## ══════════════════════════════════════════════════════════════════════════


# Only LD Day 3 is kept (as the representative LD baseline point), and LD
# medium effort (which has no day tracking of its own) is folded into that
# same facet so low + medium effort sit side by side for LD. LL Low / LL
# High keep whatever effort levels they have. Everything else (LD Day 1, 2,
# 4, 5, 6, ...) is dropped from this plot.
mouse_day_summary <- forage_only %>%
  mutate(
    day_label = case_when(
      light == "LD" & effort == "medium" & !has_explicit_day ~ "LD",
      light == "LD" & has_explicit_day                       ~ paste0("LD Day ", day_num),
      light == "LD" & effort == "high"                       ~ "LD",
      light == "LL (low)"                                    ~ "LL Low",
      light == "LL (high)"                                   ~ "LL High",
      TRUE ~ light
    )
  ) %>%
  filter(day_label %in% c("LD", "LD Day 3", "LL Low", "LL High")) %>%
  mutate(
    day_label = if_else(day_label == "LD Day 3", "LD", day_label)
  )

day_order <- c("LD", "LL Low", "LL High")

mouse_day_summary <- mouse_day_summary %>%
  mutate(day_label = factor(day_label, levels = day_order))

condition_day_summary <- summarise_behaviors(
  mouse_day_summary,
  c("day_label", "effort", "diet")
) %>%
  mutate(day_label = factor(day_label, levels = day_order))

mouse_day_summary <- mouse_day_summary %>%
  mutate(day_group = as.character(day_label))

condition_day_summary <- condition_day_summary %>%
  mutate(day_group = as.character(day_label))

# color_key combines condition (day_group) + effort so each effort level
# within a condition gets its own shade, per CONDITION_COLOURS above.
mouse_day_summary <- mouse_day_summary %>%
  mutate(color_key = factor(paste(day_group, effort, sep = "_"), levels = CONDITION_COLOUR_ORDER))

condition_day_summary <- condition_day_summary %>%
  mutate(color_key = factor(paste(day_group, effort, sep = "_"), levels = CONDITION_COLOUR_ORDER))

# Plotting the daily results
behavior_trend_plot_by_day <- function(mouse_df, summary_df, y_col, y_label){

  mean_col <- paste0(y_col,"_mean")
  sem_col  <- paste0(y_col,"_sem")

  ggplot(summary_df,
         aes(effort,.data[[mean_col]],color=color_key))+

    geom_jitter(
      data=mouse_df,
      aes(effort,.data[[y_col]],color=color_key),
      width=.12,
      size=2,
      alpha=.6,
      inherit.aes=FALSE
    )+

    geom_errorbar(
      aes(
        ymin=.data[[mean_col]]-.data[[sem_col]],
        ymax=.data[[mean_col]]+.data[[sem_col]]
      ),
      width=.15
    )+

    geom_point(size=3)+

    facet_grid(diet~day_label)+

    scale_color_manual(values=CONDITION_COLOURS)+

    labs(
      x="Effort",
      y=y_label,
      color="Condition"
    )+

    theme_classic(base_size=12)+
    theme(
      panel.border=element_rect(fill=NA),
      strip.background=element_rect(fill="grey95")
    )
}

print(behavior_trend_plot_by_day(mouse_day_summary,condition_day_summary,"pellets","Pellets"))
print(behavior_trend_plot_by_day(mouse_day_summary,condition_day_summary,"wheel","Wheel"))
print(behavior_trend_plot_by_day(mouse_day_summary,condition_day_summary,"nose_pokes","Nose pokes"))
print(behavior_trend_plot_by_day(mouse_day_summary,condition_day_summary,"licks","Licks"))
print(behavior_trend_plot_by_day(mouse_day_summary,condition_day_summary,"in_zone_time","Time in zone"))
# NEW
print(behavior_trend_plot_by_day(mouse_day_summary,condition_day_summary,"in_zone_cup_time","Time in zone (cup)"))


# Day-resolved demand curve 
demand_summary_by_day <- mouse_day_summary %>%
  group_by(day_label, effort, diet) %>%
  summarise(
    pellets_mean = mean(pellets, na.rm = TRUE),
    pellets_sem  = sd(pellets, na.rm = TRUE) / sqrt(sum(!is.na(pellets))),
    .groups = "drop"
  )

p_demand_by_day <- ggplot(demand_summary_by_day, aes(x = effort, y = pellets_mean, group = diet, color = diet)) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = pellets_mean - pellets_sem, ymax = pellets_mean + pellets_sem), width = 0.1) +
  geom_point(size = 3) +
  facet_wrap(~ day_label) +
  labs(x = "Effort requirement", y = "Mean daily pellets earned",
       title = "Demand curve by day (LD days and LL Low/High shown separately)", color = "Diet") +
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(fill = NA, color = "black"),
        strip.background = element_rect(fill = "grey95", color = "black"))

print(p_demand_by_day)
