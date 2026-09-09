## ══════════════════════════════════════════════════════════════════════════
## 3D BEHAVIOR RELATIONSHIP + RANDOM FOREST CLASSIFICATION
## ══════════════════════════════════════════════════════════════════════════

library(plotly)
library(randomForest)
library(caret)
library(dplyr)
library(ggplot2)

# ══════════════════════════════════════════════════════════════════════════
# 3D BEHAVIOR RELATIONSHIP: nose pokes x wheel x pellets 
# ══════════════════════════════════════════════════════════════════════════

# Clean data 
behavior3d_data <- mouse_summary %>%
  filter(is.finite(nose_pokes), is.finite(wheel), is.finite(pellets))

# Restrict LD rows to low/medium/high effort, keep all LL rows 
# LL Low / LL High rows aren't day-numbered, so they pass through untouched.
behavior3d_data <- behavior3d_data %>%
  filter(
    light != "LD" |
      (light == "LD" & effort == "low") |
      (light == "LD" & effort == "medium") |
      (light == "LD" & effort == "high")
  )

# Condition label = light x effort 
behavior3d_data <- behavior3d_data %>%
  mutate(
    arena = str_extract(mouse_id, "[^_]+$"),

    condition_label = case_when(
      light == "LD" ~
        paste("LD", str_to_title(effort), sep = " \u2013 "),

      str_detect(light, regex("LL", ignore_case = TRUE)) &
        str_detect(light, regex("low", ignore_case = TRUE)) ~
        paste("LL Low", str_to_title(effort), sep = " \u2013 "),

      str_detect(light, regex("LL", ignore_case = TRUE)) &
        str_detect(light, regex("high", ignore_case = TRUE)) ~
        paste("LL High", str_to_title(effort), sep = " \u2013 "),

      TRUE ~
        paste(light, str_to_title(effort), sep = " \u2013 ")
    )
  )

# Condition colors
CONDITION_COLOURS_3D <- CONDITION_COLOURS[
  intersect(
    names(CONDITION_COLOURS),
    unique(behavior3d_data$condition_label)
  )
]

# Saturating regression surface: log(wheel) instead of quadratic 
# Quadratic terms weren't significant (ANOVA p = 0.66 on the full dataset)
# and forced a symmetric hump that curved back down at high wheel — not
# supported by the data, which only plateaus. log(wheel + 1) rises fast
# then flattens, matching the real ceiling effect (confirmed by GAM:
# edf ~ 2.65, p = 0.003 on s(wheel)).

pellet_lm_log <- lm(pellets ~ nose_pokes + log(wheel + 1), data = behavior3d_data)

# Prediction grid
grid_n <- 25

np_seq <- seq(min(behavior3d_data$nose_pokes), max(behavior3d_data$nose_pokes), length.out = grid_n)
wh_seq <- seq(min(behavior3d_data$wheel), max(behavior3d_data$wheel), length.out = grid_n)

# nose_pokes varies fastest in expand.grid -> natural matrix is
# nrow = length(np_seq), ncol = length(wh_seq). Transpose to match
# plotly's requirement: nrow = length(y) = length(wh_seq), ncol = length(x) = length(np_seq).
pred_grid <- expand.grid(nose_pokes = np_seq, wheel = wh_seq)
pred_grid$pellets_pred <- predict(pellet_lm_log, newdata = pred_grid)

z_surface_raw <- matrix(pred_grid$pellets_pred, nrow = length(np_seq), ncol = length(wh_seq))
z_surface <- t(z_surface_raw)

# Z-axis range 
z_range <- range(behavior3d_data$pellets, na.rm = TRUE)
z_pad <- diff(z_range) * 0.15
z_axis_range <- c(z_range[1] - z_pad, z_range[2] + z_pad)

# D plot 
p_3d_behavior <- plot_ly() %>%

  add_trace(
    data = behavior3d_data,

    x = ~nose_pokes,
    y = ~wheel,
    z = ~pellets,

    color = ~condition_label,
    colors = CONDITION_COLOURS_3D,

    symbol = ~diet,
    symbols = c(
      sucrose = "circle",
      hfd = "diamond",
      sd = "square"
    ),

    text = ~paste0(
      "Mouse: ", mouse_id,
      "<br>Light: ", light,
      "<br>Effort: ", effort,
      "<br>Condition: ", condition_label,
      "<br>Diet: ", diet,
      "<br>Day #: ", day_num,
      "<br>Nose pokes: ", round(nose_pokes, 1),
      "<br>Wheel: ", round(wheel, 1),
      "<br>Pellets: ", round(pellets, 1)
    ),

    hoverinfo = "text",

    type = "scatter3d",
    mode = "markers",

    marker = list(
      size = 5,
      opacity = 0.85,
      line = list(
        width = 0.5,
        color = "black"
      )
    )
  ) %>%

  add_surface(
    x = np_seq,
    y = wh_seq,
    z = z_surface,

    opacity = 0.35,
    showscale = FALSE,

    colorscale = list(
      c(0, 1),
      c("grey80", "grey80")
    ),

    name = "Fitted regression surface"
  ) %>%

  layout(

    title = paste0(
      "Nose pokes \u00d7 Wheel \u00d7 Pellets \u2014 ",
      "colored by LIGHT x EFFORT, shaped by diet",
      "<br><sup>Grey sheet = fitted saturating surface (log-wheel) \u2014 LD restricted to Day 3</sup>"
    ),

    scene = list(
      xaxis = list(title = "Nose pokes"),
      yaxis = list(title = "Wheel turns"),
      zaxis = list(title = "Pellets earned", range = z_axis_range)
    ),

    legend = list(
      title = list(text = "Light x Effort")
    )
  )

print(p_3d_behavior)


# ══════════════════════════════════════════════════════════════════════════
# Random forest:
# How behaviorally distinguishable are effort x light conditions?
# ══════════════════════════════════════════════════════════════════════════

# Prepare random forest data 

# Target: light x effort
# Diet is NOT part of the classification target.... yet

rf_data <- behavior3d_data %>%
  filter(
    is.na(baseline_phase) |
      baseline_phase == "baseline"
  ) %>%
  mutate(
    condition_full = factor(condition_label)
  ) %>%
  select(
    mouse_id,
    condition_full,
    diet,
    wheel,
    pellets,
    nose_pokes,
    licks,
    in_zone_time
  ) %>%
  # Only remove rows with invalid core behavioral variables. 
  filter(
    is.finite(wheel),
    is.finite(pellets),
    is.finite(nose_pokes),
    is.finite(licks),
    is.finite(in_zone_time)
  )

# Check number of mice per condition 

# Mice per condition before balancing
mouse_counts <- rf_data %>%
  distinct(mouse_id, condition_full) %>%
  count(condition_full)

print(mouse_counts)

# Mice in each condition
rf_data %>%
  distinct(mouse_id, condition_full) %>%
  group_by(condition_full) %>%
  summarise(
    n_mice = n(),
    mice = paste(sort(unique(mouse_id)), collapse = ", "),
    .groups = "drop"
  ) %>%
  print()

# Balance conditions by mouse 
# Each condition contributes the same number of mice (the smallest number
# of mice represented in any condition).

set.seed(42)

mouse_condition <- rf_data %>%
  distinct(mouse_id, condition_full)

min_mice_per_condition <- mouse_condition %>%
  count(condition_full) %>%
  summarise(min_n = min(n)) %>%
  pull(min_n)

# Balancing (not actually needed for us but good to have in place)
cat("Mice per condition used:", min_mice_per_condition, "\n")

balanced_mice <- mouse_condition %>%
  group_by(condition_full) %>%
  slice_sample(n = min_mice_per_condition) %>%
  ungroup()

rf_balanced <- rf_data %>%
  semi_join(balanced_mice, by = c("mouse_id", "condition_full"))

cat("\nBalanced class sizes:\n")
print(table(rf_balanced$condition_full))

# Random forest model 
# Five core behavioral variables: wheel, pellets, nose pokes, licks, time in zone
set.seed(42)

rf_model <- randomForest(
  condition_full ~ wheel + pellets + nose_pokes + licks + in_zone_time,
  data = rf_balanced,
  ntree = 1000,
  importance = TRUE
)

print(rf_model)

# Tune random forest 

tuneRF(
  x = rf_balanced %>% select(wheel, pellets, nose_pokes, licks, in_zone_time),
  y = rf_balanced$condition_full,
  ntreeTry = 1000,
  stepFactor = 1.5,
  improve = 0.01,
  trace = FALSE,
  plot = TRUE
)

# Variable importance

print(importance(rf_model))

varImpPlot(
  rf_model,
  type = 1,
  main = "Variable importance for distinguishing light x effort conditions"
)

# 7. Leave-one-mouse-out cross-validation 
# Entire mice are held out, so observations from the same mouse never
# appear in both training and testing data.

all_mice <- unique(rf_balanced$mouse_id)

loo_results_list <- vector("list", length(all_mice))

for (i in seq_along(all_mice)) {

  test_mouse <- all_mice[i]

  train_i <- rf_balanced %>%
    filter(mouse_id != test_mouse) %>%
    droplevels()

  test_i <- rf_balanced %>%
    filter(mouse_id == test_mouse)

  if (n_distinct(train_i$condition_full) < 2) {
    warning(paste("Skipping mouse:", test_mouse, "- fewer than 2 conditions in training data."))
    next
  }

  model_i <- randomForest(
    condition_full ~ wheel + pellets + nose_pokes + licks + in_zone_time,
    data = train_i,
    ntree = 500
  )

  predictions_i <- predict(model_i, newdata = test_i)

  loo_results_list[[i]] <- data.frame(
    mouse_id = test_i$mouse_id,
    condition_full = as.character(test_i$condition_full),
    predicted = as.character(predictions_i),
    stringsAsFactors = FALSE
  )
}

# Combine LOOCV results

loo_results <- bind_rows(loo_results_list)

# Force actual and predicted to have exactly the same levels.
condition_levels <- levels(droplevels(rf_balanced$condition_full))

loo_results <- loo_results %>%
  mutate(
    condition_full = factor(condition_full, levels = condition_levels),
    predicted = factor(predicted, levels = condition_levels)
  )

cat("Number of observations:", nrow(loo_results), "\n")
cat("Number of mice:", n_distinct(loo_results$mouse_id), "\n")
cat("Number of conditions:", n_distinct(loo_results$condition_full), "\n")
print(head(loo_results))

# Observation-level confusion matrix 

cm <- confusionMatrix(
  data = loo_results$predicted,
  reference = loo_results$condition_full,
  mode = "everything"
)

print(cm)

# Overall classification performance 

overall_accuracy <- mean(loo_results$predicted == loo_results$condition_full, na.rm = TRUE)
balanced_accuracy <- mean(cm$byClass[, "Balanced Accuracy"], na.rm = TRUE)

cat("Overall accuracy:", round(overall_accuracy, 3), "\n")
cat("Mean balanced accuracy:", round(balanced_accuracy, 3), "\n")

# Per-condition performance 
# Sensitivity/recall: of the observations actually belonging to this
# condition, what proportion did the model correctly identify?

per_condition <- data.frame(
  condition = rownames(cm$byClass),
  sensitivity = cm$byClass[, "Sensitivity"],
  specificity = cm$byClass[, "Specificity"],
  balanced_accuracy = cm$byClass[, "Balanced Accuracy"],
  row.names = NULL
)


print(per_condition)

# Per-condition sensitivity plot

p_condition_accuracy <- ggplot(
  per_condition,
  aes(x = reorder(condition, sensitivity), y = sensitivity)
) +
  geom_col(fill = "grey70", color = "black", width = 0.7) +
  geom_hline(
    yintercept = 1 / n_distinct(rf_balanced$condition_full),
    linetype = "dashed",
    color = "grey40"
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    x = "Light x effort condition",
    y = "Sensitivity / recall",
    title = "How well can the random forest identify each condition?",
    subtitle = "Leave-one-mouse-out cross-validation"
  ) +
  theme_classic(base_size = 13) +
  theme(panel.border = element_rect(fill = NA, color = "black"))

print(p_condition_accuracy)

# Row-normalized confusion matrix 
# Each row sums to 100%. High diagonal values = condition is highly
# distinguishable. Off-diagonal values = conditions being confused with
# one another.

cm_counts <- table(
  Actual = factor(loo_results$condition_full, levels = condition_levels),
  Predicted = factor(loo_results$predicted, levels = condition_levels)
)

cm_proportions <- prop.table(cm_counts, margin = 1)

cm_table <- as.data.frame(cm_proportions)

p_confusion <- ggplot(cm_table, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(Freq, accuracy = 1)), size = 3.5) +
  scale_fill_gradient(low = "white", high = "black", limits = c(0, 1), labels = scales::percent) +
  labs(
    x = "Predicted condition",
    y = "Actual condition",
    fill = "Proportion",
    title = "Behavioral distinguishability of light x effort conditions",
    subtitle = "Row-normalized leave-one-mouse-out confusion matrix"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(fill = NA, color = "black")
  )

print(p_confusion)

# Mouse-level predictions 
# If a mouse has multiple behavioral observations, classify the mouse based
# on the most common prediction across its observations, for a true
# mouse-level measure of distinguishability.

mouse_predictions <- loo_results %>%
  group_by(mouse_id) %>%
  summarise(
    actual = first(condition_full),
    predicted = names(sort(table(predicted), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  mutate(
    actual = factor(actual, levels = condition_levels),
    predicted = factor(predicted, levels = condition_levels)
  )

# Mouse-level confusion matrix 
mouse_cm <- confusionMatrix(
  data = mouse_predictions$predicted,
  reference = mouse_predictions$actual,
  mode = "everything"
)

print(mouse_cm)

mouse_accuracy <- mean(mouse_predictions$predicted == mouse_predictions$actual, na.rm = TRUE)
mouse_balanced_accuracy <- mean(mouse_cm$byClass[, "Balanced Accuracy"], na.rm = TRUE)

cat("\nMouse-level accuracy:", round(mouse_accuracy, 3), "\n")
cat("Mouse-level balanced accuracy:", round(mouse_balanced_accuracy, 3), "\n")

# Pairwise classification 
# How behaviorally distinguishable is condition A from condition B?
# Each pair of light x effort conditions is tested separately, leaving one
# mouse out at a time, with one majority-vote prediction per held-out mouse.
# 1.00 = perfectly distinguishable, 0.50 = chance.

condition_levels <- levels(droplevels(rf_balanced$condition_full))

pairwise_results <- list()
pair_counter <- 1

for (a in 1:(length(condition_levels) - 1)) {
  for (b in (a + 1):length(condition_levels)) {

    cond_a <- condition_levels[a]
    cond_b <- condition_levels[b]

    cat("\nTesting:", cond_a, "vs.", cond_b, "\n")

    pair_data <- rf_balanced %>%
      filter(condition_full %in% c(cond_a, cond_b)) %>%
      mutate(condition_full = factor(condition_full, levels = c(cond_a, cond_b)))

    pair_mice <- unique(pair_data$mouse_id)

    pair_predictions <- list()

    for (j in seq_along(pair_mice)) {

      test_mouse <- pair_mice[j]

      train_j <- pair_data %>% filter(mouse_id != test_mouse)
      test_j <- pair_data %>% filter(mouse_id == test_mouse)

      if (n_distinct(train_j$condition_full) < 2) next

      model_j <- randomForest(
        condition_full ~ wheel + pellets + nose_pokes + licks + in_zone_time,
        data = train_j,
        ntree = 500
      )

      pred_j <- predict(model_j, newdata = test_j)

      predicted_mouse <- names(sort(table(pred_j), decreasing = TRUE))[1]
      actual_mouse <- as.character(unique(test_j$condition_full))[1]

      pair_predictions[[j]] <- data.frame(
        mouse_id = test_mouse,
        actual = actual_mouse,
        predicted = predicted_mouse,
        stringsAsFactors = FALSE
      )
    }

    pair_predictions_df <- bind_rows(pair_predictions)

    if (nrow(pair_predictions_df) < 2) next

    pair_predictions_df <- pair_predictions_df %>%
      mutate(
        actual = factor(actual, levels = c(cond_a, cond_b)),
        predicted = factor(predicted, levels = c(cond_a, cond_b))
      )

    pair_accuracy <- mean(pair_predictions_df$predicted == pair_predictions_df$actual, na.rm = TRUE)

    sensitivity_a <- sum(pair_predictions_df$predicted == cond_a & pair_predictions_df$actual == cond_a, na.rm = TRUE) /
      sum(pair_predictions_df$actual == cond_a, na.rm = TRUE)

    sensitivity_b <- sum(pair_predictions_df$predicted == cond_b & pair_predictions_df$actual == cond_b, na.rm = TRUE) /
      sum(pair_predictions_df$actual == cond_b, na.rm = TRUE)

    # Balanced accuracy = average sensitivity across the two conditions —
    # preferable to raw accuracy if the groups don't have equal numbers of
    # held-out mice.
    pair_balanced_accuracy <- mean(c(sensitivity_a, sensitivity_b), na.rm = TRUE)

    pairwise_results[[pair_counter]] <- data.frame(
      condition_1 = cond_a,
      condition_2 = cond_b,
      accuracy = pair_accuracy,
      balanced_accuracy = pair_balanced_accuracy,
      sensitivity_condition_1 = sensitivity_a,
      sensitivity_condition_2 = sensitivity_b,
      n_mice = nrow(pair_predictions_df),
      stringsAsFactors = FALSE
    )

    pair_counter <- pair_counter + 1
  }
}

pairwise_results_df <- bind_rows(pairwise_results)


print(pairwise_results_df %>% arrange(desc(balanced_accuracy)))

# Pairwise distinguishability matrix 
# Higher values = greater behavioral distinguishability. 100% = perfectly
# distinguishable, 50% = chance.

pairwise_matrix <- matrix(
  NA_real_,
  nrow = length(condition_levels),
  ncol = length(condition_levels),
  dimnames = list(condition_levels, condition_levels)
)

diag(pairwise_matrix) <- 1

for (i in seq_len(nrow(pairwise_results_df))) {
  c1 <- pairwise_results_df$condition_1[i]
  c2 <- pairwise_results_df$condition_2[i]
  value <- pairwise_results_df$balanced_accuracy[i]

  pairwise_matrix[c1, c2] <- value
  pairwise_matrix[c2, c1] <- value
}

pairwise_long <- as.data.frame(as.table(pairwise_matrix))
colnames(pairwise_long) <- c("Condition_1", "Condition_2", "Balanced_Accuracy")

# Pairwise distinguishability heatmap

p_pairwise <- ggplot(pairwise_long, aes(x = Condition_1, y = Condition_2, fill = Balanced_Accuracy)) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = ifelse(is.na(Balanced_Accuracy), "", scales::percent(Balanced_Accuracy, accuracy = 1))),
    size = 3
  ) +
  scale_fill_gradient(
    limits = c(0.5, 1),
    low = "white",
    high = "black",
    na.value = "grey90",
    labels = scales::percent
  ) +
  labs(
    x = NULL, y = NULL,
    fill = "Balanced\naccuracy",
    title = "Pairwise behavioral distinguishability",
    subtitle = "Leave-one-mouse-out classification"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    panel.border = element_rect(fill = NA, color = "black")
  )

print(p_pairwise)

# Overall permutation test 
# Condition labels are randomly shuffled while
# behavioral data remain fixed.

get_oob_accuracy <- function(data) {
  model <- randomForest(
    condition_full ~ wheel + pellets + nose_pokes + licks + in_zone_time,
    data = data,
    ntree = 1000
  )

  cm_tmp <- model$confusion[, -ncol(model$confusion)]

  sum(diag(cm_tmp)) / sum(cm_tmp)
}

set.seed(42)
real_accuracy <- get_oob_accuracy(rf_balanced)

# Permutation test

n_perm <- 500
null_accuracies <- numeric(n_perm)

set.seed(1)

for (i in seq_len(n_perm)) {
  shuffled <- rf_balanced
  shuffled$condition_full <- sample(shuffled$condition_full)
  null_accuracies[i] <- get_oob_accuracy(shuffled)
}

p_value <- (sum(null_accuracies >= real_accuracy) + 1) / (n_perm + 1)

cat("Real-label OOB accuracy:", round(real_accuracy, 3), "\n")
cat("Null mean OOB accuracy:", round(mean(null_accuracies), 3), "\n")
cat("Chance level:", round(1 / n_distinct(rf_balanced$condition_full), 3), "\n")
cat("Permutation p-value:", p_value, "\n")

# Permutation distribution 

perm_df <- data.frame(accuracy = null_accuracies)

p_perm_test <- ggplot(perm_df, aes(x = accuracy)) +
  geom_histogram(binwidth = 0.03, fill = "grey70", color = "black", alpha = 0.8) +
  geom_vline(xintercept = real_accuracy, color = "firebrick", linewidth = 1.1, linetype = "dashed") +
  geom_vline(
    xintercept = 1 / n_distinct(rf_balanced$condition_full),
    color = "grey40",
    linetype = "dotted",
    linewidth = 1
  ) +
  annotate(
    "text",
    x = real_accuracy,
    y = Inf,
    label = paste0("Observed = ", round(real_accuracy, 2), "\np = ", signif(p_value, 3)),
    color = "firebrick",
    vjust = 1.5,
    hjust = -0.05,
    size = 4.2,
    fontface = "bold"
  ) +
  labs(
    x = "OOB classification accuracy",
    y = "Count (permutations)",
    title = "Random forest accuracy vs. permuted-label null distribution",
    subtitle = paste0(
      "N = ", n_perm, " permutations \u2014 ",
      n_distinct(rf_balanced$condition_full), " light x effort conditions"
    )
  ) +
  theme_classic(base_size = 12) +
  theme(panel.border = element_rect(fill = NA, color = "black"))

print(p_perm_test)

# Save all key results

rf_results <- list(
  model = rf_model,
  balanced_data = rf_balanced,
  loo_predictions = loo_results,
  confusion_matrix = cm,
  overall_accuracy = overall_accuracy,
  balanced_accuracy = balanced_accuracy,
  per_condition = per_condition,
  mouse_predictions = mouse_predictions,
  mouse_confusion_matrix = mouse_cm,
  mouse_accuracy = mouse_accuracy,
  mouse_balanced_accuracy = mouse_balanced_accuracy,
  pairwise = pairwise_results_df,
  permutation_accuracy = real_accuracy,
  permutation_null = null_accuracies,
  permutation_p = p_value
)

# Final summary 

cat("Number of conditions:", n_distinct(rf_balanced$condition_full), "\n")
cat("Mice per condition:", min_mice_per_condition, "\n")
cat("Observation-level LOOCV accuracy:", round(overall_accuracy, 3), "\n")
cat("Observation-level balanced accuracy:", round(balanced_accuracy, 3), "\n")
cat("Mouse-level accuracy:", round(mouse_accuracy, 3), "\n")
cat("Mouse-level balanced accuracy:", round(mouse_balanced_accuracy, 3), "\n")
cat("Permutation p-value:", signif(p_value, 3), "\n")

