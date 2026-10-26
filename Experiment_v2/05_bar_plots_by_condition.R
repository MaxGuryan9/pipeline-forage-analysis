## ══════════════════════════════════════════════════════════════════════════
## Repeated-measures bar plots comparing conditions, plus the loops that
## generate every combination. Both families use paired_bar_plot() from
## 01_helpers.R, i.e. REPEATED-MEASURES stats since these are the same animals
## sampled across conditions:
##   - Omnibus test: linear mixed model (lmer) with a per-animal random intercept
##   - Pairwise test: paired t-tests (matched by animal_id), Holm-adjusted
##   - Plots: only animals with data in every group of that plot are shown,
##     connected by a line per animal
##
## FAMILY A (bar_totals_by_effort): within ONE effort level, compare light
##   conditions -> No light vs White light vs Yellow light.
##
## FAMILY B (bar_totals_by_light): within ONE light condition, compare effort
##   levels -> e.g. White light: low vs medium vs high.
##
## Data = animal_summary: one value per animal per light x effort x diet, i.e.
## the mean over that condition's days (set FORAGE_DAYS_TO_USE in 00_config.R to
## restrict which days count). The Baseline day-vs-day comparison is in 04.
## ══════════════════════════════════════════════════════════════════════════

# FAMILY A: compare light conditions, within one effort level
bar_totals_by_effort <- function(animal_df, y_col, y_label, target_effort) {
  stats_df <- animal_df %>%
    filter(effort == target_effort, !is.na(light_group)) %>%
    mutate(group = droplevels(light_group))

  paired_bar_plot(stats_df, "group", "animal_id", y_col, y_label,
                  title = paste0(str_to_title(target_effort), " effort"),
                  fill_values = CONDITION_BASE_COLOURS)
}

# FAMILY B: compare effort levels, within one light condition
bar_totals_by_light <- function(animal_df, y_col, y_label, target_light) {
  stats_df <- animal_df %>%
    filter(light_group == target_light, !is.na(effort))

  present_efforts <- intersect(EFFORT_LEVELS, as.character(unique(stats_df$effort)))
  stats_df <- stats_df %>% mutate(group = factor(as.character(effort), levels = present_efforts))

  # matching "<light>_<effort>" shades from the shared palette
  group_colours <- CONDITION_COLOURS[paste(target_light, present_efforts, sep = "_")]
  names(group_colours) <- present_efforts

  paired_bar_plot(stats_df, "group", "animal_id", y_col, y_label,
                  title = as.character(target_light), x_label = "Effort",
                  fill_values = group_colours)
}

# ══════════════════════════════════════════════════════════════════════════
# RUN EVERYTHING
# Some stats give warnings, ignore for now especially since n is low.
# ══════════════════════════════════════════════════════════════════════════
if (!HAS_FORAGE_DATA) {
  message("05: no forage-condition data yet - skipping condition bar plots.")
} else {

  present_efforts <- intersect(EFFORT_LEVELS, as.character(unique(animal_summary$effort)))
  present_lights  <- intersect(unname(LIGHT_LABELS), as.character(unique(animal_summary$light_group)))

  # FAMILY A: one set of light-comparison plots per effort level with data
  for (eff in present_efforts) {
    for (i in seq_len(nrow(BEHAVIOR_METRICS))) {
      p <- bar_totals_by_effort(animal_summary, BEHAVIOR_METRICS$y_col[i], BEHAVIOR_METRICS$y_label[i], eff)
      if (!is.null(p)) print(p)
    }
  }

  # FAMILY B: one set of effort-comparison plots per light condition with data
  for (lt in present_lights) {
    for (i in seq_len(nrow(BEHAVIOR_METRICS))) {
      p <- bar_totals_by_light(animal_summary, BEHAVIOR_METRICS$y_col[i], BEHAVIOR_METRICS$y_label[i], lt)
      if (!is.null(p)) print(p)
    }
  }
}
