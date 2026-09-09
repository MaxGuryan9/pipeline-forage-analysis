# Pipeline Forage Analysis

An R analysis pipeline for a behavioral neuroscience experiment studying how mice forage for food reward under varying **effort requirements** (low / medium / high) and **light schedules** (standard light/dark cycle vs. constant light, at two intensities). Each animal is recorded in an individual arena that logs wheel running, nose pokes, pellet delivery, licking, and time spent in the wheel and food-cup zones.

The pipeline turns raw per-arena CSV recordings into cleaned summary tables, effort/light behavior comparisons with repeated-measures statistics, behavioral-economics demand curves, and a random forest classifier that tests how behaviorally distinguishable the experimental conditions are.

## Pipeline

Scripts are numbered in run order. `00`–`02` build the shared tables everything else reads from; `03`–`06` branch off those tables for different plots and statistics; `07` recombines them for a 3D model fit and the classifier.

| Script | What it does |
|---|---|
| [`00_config.R`](00_config.R) | Loads packages, sets `BASE_DIR`, defines zeitgeber-time offset rules, and builds the shared color palette (light condition × effort level). Run first. |
| [`01_helpers.R`](01_helpers.R) | Reusable functions: folder-name parsing, CSV reading, zone-occupancy bout extraction, wheel→cup latency, and paired repeated-measures t-tests. |
| [`02_load_and_index.R`](02_load_and_index.R) | Indexes every arena CSV under `BASE_DIR`, parses its condition metadata, and builds the per-file, per-day, and per-mouse summary tables (`daily_all`, `forage_only`, `mouse_summary`) that later scripts read. |
| [`03_effort_demand_plots.R`](03_effort_demand_plots.R) | Per-behavior trend plots and a day-resolved demand curve (reward intake vs. effort) comparing light conditions on a representative baseline day. |
| [`04_training_summary.R`](04_training_summary.R) | Training-day performance across days, once training recordings exist. |
| [`05_bar_plots_by_condition.R`](05_bar_plots_by_condition.R) | Repeated-measures comparisons — linear mixed models (`lmer`) with a per-animal random intercept, plus paired, Holm-adjusted t-tests — for every behavior × condition combination. |
| [`06_efficiency_demand_analysis.R`](06_efficiency_demand_analysis.R) | Demand curves, running efficiency, cost-per-pellet metrics, peak running rate, wheel→cup latency, and a baseline-drift check across repeated days. |
| [`07_3d_plot_and_random_forest.R`](07_3d_plot_and_random_forest.R) | A 3D fit of pellets earned as a function of nose pokes and wheel running, and a random forest that tests whether five behavioral measures can classify which light × effort condition a mouse was recorded under — validated with leave-one-mouse-out cross-validation and a label-permutation test. |

## Setup

1. Open `Pipeline forage analysis.Rproj` in RStudio (this sets the working directory automatically) or otherwise run scripts with this repo's root as the working directory.
2. Update `BASE_DIR` in `00_config.R` to point at your local data folder. Everything else derives from that one path — no other file needs to change.
3. Source the scripts in order, `00` through `07`. `00_config.R` will install any missing packages automatically.

### Expected data layout

```
<BASE_DIR>/
├── LD low effort sucrose day 1/   *_Arena1.csv, *_Arena2.csv, ...
├── LD low effort sucrose day 2/
├── LD medium effort sucrose/
├── LL (low) low effort sucrose/
├── LL (high) low effort sucrose/
└── Training day 1/                (once training recordings exist)
```

Folder names encode the condition (light schedule, effort level, diet, and optionally an explicit day number); the pipeline parses this automatically in `01_helpers.R`. The `data/` and `processed/` folders in this repo keep that structure as empty placeholders — the actual recordings and derived tables aren't published here, since this is unpublished lab data.

## Methods notes

- **Repeated-measures statistics.** The same animals are recorded across multiple conditions, so comparisons use a linear mixed model with a per-animal random intercept (not a one-way ANOVA) and paired, Holm-adjusted t-tests (not independent-samples tests).
- **Leave-one-mouse-out cross-validation.** The random forest classifier is validated by holding out one entire animal at a time, so no animal's data appears in both training and test sets for its own prediction.
- **Permutation testing.** Classification accuracy is compared against a null distribution built by repeatedly shuffling condition labels and retraining, to check that the classifier is using real behavioral signal rather than exploiting incidental structure in a small sample.

## Stack

R, tidyverse, `lme4`/`lmerTest` (mixed models), `ggpubr` (paired stats plots), `randomForest`/`caret` (classification), `plotly` (3D visualization).

---
*Plots and example figures to come as the dataset grows.*
