## ══════════════════════════════════════════════════════════════════════════
## Repeated-measures bar plots comparing conditions, plus the loops that
## generate every combination.
##
## Both function families below use REPEATED-MEASURES stats since these are
## the same animals sampled across conditions:
##   - Omnibus test: linear mixed model (lmer) with a per-animal random
##     intercept, instead of a one-way ANOVA that treats repeated
##     observations from the same animal as independent.
##   - Pairwise test: paired t-tests (matched by animal_id), Holm-adjusted,
##     instead of independent-samples pairwise.t.test().
##   - Plots: only animals with data in every group of that plot are shown,
##     connected by a line per animal, so stat_compare_means(paired = TRUE)
##     lines up correctly.
##
## FAMILY A (bar_totals_by_effort): within ONE effort level, compare light
##   conditions -> LD vs LL Low vs LL High. Colors = CONDITION_BASE_COLOURS,
##   since group labels here are exactly "LD" / "LL Low" / "LL High".
##
## FAMILY B (bar_totals_by_light): within ONE light condition, compare effort
##   levels -> e.g. LD low vs LD medium vs LD high. Colors = the matching 
##   effort-shade slice of CONDITION_COLOURS (e.g. "LD_low", "LD_medium", 
##   "LD_high"), so these stay visually tied to the same palette used elsewhere.
##
## For any light/effort combo where the folder uses explicit day tracking
## (has_explicit_day == TRUE, i.e. the LD baseline/retest design), only
## BASELINE_DAY_CUTOFF (currently day 3) is used as that condition's
## representative point.
## ══════════════════════════════════════════════════════════════════════════

GROUP_LIGHT_LEVELS <- c("LD", "LL Low", "LL High")

# shared helper: does light + effort + explicit-day filter for one target effort
build_light_comparison_df <- function(mouse_df, target_effort) {
  mouse_df %>%
    filter(!is.na(effort)) %>%
    mutate(
      group = case_when(
        light == "LD" &
          effort == target_effort &
          (!has_explicit_day | day_num == BASELINE_DAY_CUTOFF) ~ "LD",
        str_detect(light, regex("LL", ignore_case = TRUE)) &
          str_detect(light, regex("low", ignore_case = TRUE)) &
          effort == target_effort &
          (!has_explicit_day | day_num == BASELINE_DAY_CUTOFF) ~ "LL Low",
        str_detect(light, regex("LL", ignore_case = TRUE)) &
          str_detect(light, regex("high", ignore_case = TRUE)) &
          effort == target_effort &
          (!has_explicit_day | day_num == BASELINE_DAY_CUTOFF) ~ "LL High",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(group)) %>%
    mutate(group = factor(group, levels = intersect(GROUP_LIGHT_LEVELS, unique(group))))
}

# ══════════════════════════════════════════════════════════════════════════
# FAMILY A: compare LD vs LL Low vs LL High, within one effort level
# ══════════════════════════════════════════════════════════════════════════
bar_totals_by_effort <- function(mouse_df, y_col, y_label, target_effort){

  stats_df <- build_light_comparison_df(mouse_df, target_effort)

  if (n_distinct(stats_df$group) < 2) {
    message("Skipping ", y_label, " (", target_effort, " effort): fewer than 2 groups with data.")
    return(invisible(NULL))
  }

  cat("\n=============================\n")
  cat(y_label, "-", target_effort, "effort\n")
  print(
    stats_df %>%
      group_by(group) %>%
      summarise(
        mice = n_distinct(animal_id),
        observations = n(),
        mean = mean(.data[[y_col]], na.rm = TRUE),
        sd = sd(.data[[y_col]], na.rm = TRUE),
        .groups = "drop"
      )
  )

  cat("\nRepeated-measures model (lmer, animal_id random intercept)\n")
  model <- lmer(reformulate(c("group", "(1 | animal_id)"), response = y_col), data = stats_df)
  print(anova(model))

  cat("\nPaired, Holm-adjusted pairwise t tests (matched by animal_id)\n")
  pw <- paired_pairwise_t(stats_df, "group", y_col, "animal_id")
  print(pw)

  complete_ids <- stats_df %>%
    count(animal_id, group) %>%
    count(animal_id) %>%
    filter(n == n_distinct(stats_df$group)) %>%
    pull(animal_id)

  plot_df <- stats_df %>%
    filter(animal_id %in% complete_ids) %>%
    arrange(animal_id, group) %>%
    mutate(group = droplevels(group))

  comparisons <- combn(levels(plot_df$group), 2, simplify = FALSE)

  group_colours <- CONDITION_BASE_COLOURS[levels(plot_df$group)]

  ggplot(
    plot_df,
    aes(x = group, y = .data[[y_col]], fill = group)
  ) +
    stat_summary(fun = mean, geom = "bar", width = .65, alpha = .7) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .15) +
    geom_line(aes(group = animal_id), color = "grey60", alpha = .4) +
    geom_jitter(width = .08, size = 2, alpha = .75) +
    stat_compare_means(
      comparisons = comparisons,
      method = "t.test",
      paired = TRUE,
      p.adjust.method = "holm",
      label = "p.signif",
      step.increase = .10,
      size = 5
    ) +
    scale_fill_manual(values = group_colours) +
    labs(x = NULL, y = y_label, title = paste0(str_to_title(target_effort), " effort")) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 14)
    )
}

# ══════════════════════════════════════════════════════════════════════════
# FAMILY B: compare effort levels (low vs medium vs high), within one light
# ══════════════════════════════════════════════════════════════════════════
bar_totals_by_light <- function(mouse_df, y_col, y_label, target_light){

  light_filter <- switch(target_light,
                         "LD"      = quote(light == "LD"),
                         "LL Low"  = quote(str_detect(light, regex("LL", ignore_case = TRUE)) &
                                             str_detect(light, regex("low", ignore_case = TRUE))),
                         "LL High" = quote(str_detect(light, regex("LL", ignore_case = TRUE)) &
                                             str_detect(light, regex("high", ignore_case = TRUE)))
  )

  stats_df <- mouse_df %>%
    filter(!is.na(effort)) %>%
    filter(!!light_filter) %>%
    filter(!has_explicit_day | day_num == BASELINE_DAY_CUTOFF)

  present_efforts <- intersect(EFFORT_LEVELS, as.character(unique(stats_df$effort)))

  if (length(present_efforts) < 2) {
    message("Skipping ", y_label, " (", target_light, "): fewer than 2 effort levels with data.")
    return(invisible(NULL))
  }

  stats_df <- stats_df %>%
    mutate(group = factor(as.character(effort), levels = present_efforts))

  cat("\n=============================\n")
  cat(y_label, "-", target_light, "\n")
  print(
    stats_df %>%
      group_by(group) %>%
      summarise(
        mice = n_distinct(animal_id),
        observations = n(),
        mean = mean(.data[[y_col]], na.rm = TRUE),
        sd = sd(.data[[y_col]], na.rm = TRUE),
        .groups = "drop"
      )
  )

  cat("\nRepeated-measures model (lmer, animal_id random intercept)\n")
  model <- lmer(reformulate(c("group", "(1 | animal_id)"), response = y_col), data = stats_df)
  print(anova(model))

  cat("\nPaired, Holm-adjusted pairwise t tests (matched by animal_id)\n")
  pw <- paired_pairwise_t(stats_df, "group", y_col, "animal_id")
  print(pw)

  complete_ids <- stats_df %>%
    count(animal_id, group) %>%
    count(animal_id) %>%
    filter(n == n_distinct(stats_df$group)) %>%
    pull(animal_id)

  plot_df <- stats_df %>%
    filter(animal_id %in% complete_ids) %>%
    arrange(animal_id, group) %>%
    mutate(group = droplevels(group))

  comparisons <- combn(levels(plot_df$group), 2, simplify = FALSE)

  # Pull the matching "<light>_<effort>" shades out of the shared palette,
  # e.g. CONDITION_COLOURS[c("LD_low","LD_medium","LD_high")]
  colour_keys   <- paste(target_light, levels(plot_df$group), sep = "_")
  group_colours <- CONDITION_COLOURS[colour_keys]
  names(group_colours) <- levels(plot_df$group)

  ggplot(
    plot_df,
    aes(x = group, y = .data[[y_col]], fill = group)
  ) +
    stat_summary(fun = mean, geom = "bar", width = .65, alpha = .7) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = .15) +
    geom_line(aes(group = animal_id), color = "grey60", alpha = .4) +
    geom_jitter(width = .08, size = 2, alpha = .75) +
    stat_compare_means(
      comparisons = comparisons,
      method = "t.test",
      paired = TRUE,
      p.adjust.method = "holm",
      label = "p.signif",
      step.increase = .10,
      size = 5
    ) +
    scale_fill_manual(values = group_colours) +
    labs(x = "Effort", y = y_label, title = target_light) +
    theme_classic(base_size = 14) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 14)
    )
}

# ══════════════════════════════════════════════════════════════════════════
# ANALYSIS: all LD low-effort days compared to each other (paired by mouse_id)
# Unchanged — this is a within-LD day-progression check, separate from the
# effort/light comparisons above.
# ══════════════════════════════════════════════════════════════════════════
bar_totals_ld_days <- function(mouse_df, y_col, y_label){
  
  stats_df <- mouse_df %>%
    filter(
      light == "LD",
      effort == "low",
      has_explicit_day,
      is.finite(.data[[y_col]])
    ) %>%
    mutate(
      group = paste0("LD Day ", day_num)
    )
  
  if (nrow(stats_df) == 0) {
    stop("No finite observations available for ", y_label)
  }
  
  day_levels <- paste0(
    "LD Day ",
    sort(unique(stats_df$day_num))
  )
  
  stats_df <- stats_df %>%
    mutate(
      group = factor(group, levels = day_levels)
    )

  cat("\n=============================\n")
  cat(y_label, "\n")
  cat("=============================\n")
  
  print(
    stats_df %>%
      group_by(group) %>%
      summarise(
        mice = n_distinct(mouse_id),
        observations = n(),
        mean = mean(.data[[y_col]]),
        sd = sd(.data[[y_col]]),
        .groups = "drop"
      )
  )
  
  complete_ids <- stats_df %>%
    group_by(mouse_id) %>%
    summarise(
      n_groups = n_distinct(group),
      .groups = "drop"
    ) %>%
    filter(n_groups == length(day_levels)) %>%
    pull(mouse_id)
  
  plot_df <- stats_df %>%
    filter(mouse_id %in% complete_ids) %>%
    droplevels() %>%
    arrange(mouse_id, group)
  
  cat("\nComplete mice for paired analysis/plot:",
      length(complete_ids), "\n")
  
  cat("Total mice in filtered data:",
      n_distinct(stats_df$mouse_id), "\n")
  
  cat("\n=============================\n")
  cat("Repeated-measures model (lmer, mouse_id random intercept)\n")
  cat("=============================\n")
  
  model <- lmer(
    reformulate(
      c("group", "(1 | mouse_id)"),
      response = y_col
    ),
    data = stats_df
  )
  
  print(anova(model))
  
  # Check for singular fit
  if (isSingular(model, tol = 1e-5)) {
    cat(
      "\nWARNING: Model is singular. ",
      "One or more random-effect variances may be near zero.\n",
      sep = ""
    )
  }

  cat("\n=============================\n")
  cat("Paired, Holm-adjusted pairwise t tests (matched by mouse_id)\n")
  cat("=============================\n")
  
  pw <- paired_pairwise_t(
    plot_df,
    "group",
    y_col,
    "mouse_id"
  )
  
  print(pw)

  if (length(levels(droplevels(plot_df$group))) >= 2) {
    
    comparisons <- combn(
      levels(droplevels(plot_df$group)),
      2,
      simplify = FALSE
    )
    
  } else {
    
    comparisons <- list()
    
  }
  
  day_colours <- setNames(
    colorRampPalette(
      c("#2CBBAB", "#1B8577")
    )(length(day_levels)),
    day_levels
  )

  p <- ggplot(
    plot_df,
    aes(
      x = group,
      y = .data[[y_col]],
      fill = group
    )
  ) +
    
    stat_summary(
      fun = mean,
      geom = "bar",
      width = 0.65,
      alpha = 0.7
    ) +
    
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 0.15
    ) +
    
    geom_line(
      aes(group = mouse_id),
      color = "grey60",
      alpha = 0.4
    ) +
    
    geom_jitter(
      width = 0.08,
      size = 2,
      alpha = 0.75
    ) +
    
    # Add the paired stat tests
    stat_compare_means(
      comparisons = comparisons,
      method = "t.test",
      paired = TRUE,
      p.adjust.method = "holm",
      label = "p.signif",
      step.increase = 0.10,
      size = 5
    ) +
    
    scale_fill_manual(
      values = day_colours
    ) +
    
    labs(
      x = NULL,
      y = y_label
    ) +
    
    theme_classic(
      base_size = 14
    ) +
    
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 12),
      axis.title.y = element_text(size = 14)
    )
  
  return(p)
}

# ══════════════════════════════════════════════════════════════════════════
# RUN EVERYTHING
# Some stats give warnings, ignore for now especially since n is low.
# ══════════════════════════════════════════════════════════════════════════
BEHAVIOR_METRICS <- tibble::tribble(
  ~y_col,              ~y_label,
  "pellets",           "Pellets earned",
  "wheel",             "Wheel turns",
  "nose_pokes",        "Nose pokes",
  "licks",             "Licks",
  "in_zone_time",      "Time in zone",
  "in_zone_cup_time",  "Time in zone (cup)"
)

present_efforts <- intersect(EFFORT_LEVELS, as.character(unique(mouse_summary$effort)))

# FAMILY A: one set of light-comparison plots per effort level with data
for (eff in present_efforts) {
  for (i in seq_len(nrow(BEHAVIOR_METRICS))) {
    p <- bar_totals_by_effort(
      mouse_summary,
      BEHAVIOR_METRICS$y_col[i],
      BEHAVIOR_METRICS$y_label[i],
      eff
    )
    if (!is.null(p)) print(p)
  }
}

# FAMILY B: one set of effort-comparison plots per light condition with data
for (lt in GROUP_LIGHT_LEVELS) {
  for (i in seq_len(nrow(BEHAVIOR_METRICS))) {
    p <- bar_totals_by_light(
      mouse_summary,
      BEHAVIOR_METRICS$y_col[i],
      BEHAVIOR_METRICS$y_label[i],
      lt
    )
    if (!is.null(p)) print(p)
  }
}

# All-LD-days progression check (unchanged)
for (i in seq_len(nrow(BEHAVIOR_METRICS))) {
  print(bar_totals_ld_days(mouse_summary, BEHAVIOR_METRICS$y_col[i], BEHAVIOR_METRICS$y_label[i]))
}
