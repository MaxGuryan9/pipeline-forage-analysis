# Idempotency tests for the ingest pipeline, on fake EthoVision-style files.
# Runs in a temporary copy of Experiment_v2, so the real data/ is never touched.
#   Rscript Experiment_v2/tests/test_ingest.R      (from the repo root)
# Re-run after any change to ingest or the store helpers in 01_helpers.R.
exp_src <- normalizePath(if (file.exists("DATA_DECISIONS.md")) "." else "Experiment_v2")
test_dir <- file.path(tempfile("ingest_test_"), "Experiment_v2")
for (d in c("incoming", "raw", "store")) dir.create(file.path(test_dir, "data", d), recursive = TRUE)
file.copy(list.files(exp_src, pattern = "\\.R$|^DATA_DECISIONS\\.md$", full.names = TRUE), test_dir)
setwd(test_dir)
cat("Testing in", test_dir, "\n")

suppressMessages({ source("00_config.R"); source("01_helpers.R") })
source(file.path(exp_src, "tests", "make_fake.R"))

pass <- function(msg) cat("PASS:", msg, "\n")
put <- function(folder, arenas = 1:8, seed = 1, names = paste0("Arena ", arenas, ".csv")) {
  d <- file.path(INCOMING_DIR, folder); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_along(arenas)) make_fake_file(file.path(d, names[i]), seed * 100 + arenas[i])
}
fill_chow <- function(folder, bad = FALSE) {
  f <- file.path(INCOMING_DIR, folder, CHOW_FILE)
  ch <- read_csv(f, col_types = cols(.default = col_character()), show_col_types = FALSE)
  ch$standard_chow_start <- "25.0"; ch$standard_chow_end <- as.character(20 + seq_len(nrow(ch)) / 10)
  if (bad) { ch$standard_chow_end[1] <- "30"; ch <- ch[-2, ] }
  write_csv(ch, f, na = "")
}
counts <- function() sapply(names(STORE_FILES), function(n) nrow(read_store(n)))
store_md5 <- function() unname(tools::md5sum(STORE_FILES[file.exists(STORE_FILES)]))
expect_error <- function(expr, pattern) {
  msg <- tryCatch({ expr; NA_character_ }, error = function(e) conditionMessage(e))
  if (is.na(msg) || !grepl(pattern, msg)) stop("Expected error matching '", pattern, "', got: ", msg)
  invisible(msg)
}
quiet <- function(expr) invisible(capture.output(expr))

D1 <- "low_effort_no_light_sucrose day 1"
D2 <- "low_effort_no_light_sucrose day 2"

# 1. first run writes chow template, stores nothing
put(D1, seed = 1)
quiet(ingest())
stopifnot(file.exists(file.path(INCOMING_DIR, D1, CHOW_FILE)), !file.exists(STORE_FILES[["daily"]]))
pass("1 chow template written, store untouched")

# 2. fill chow, ingest day 1
fill_chow(D1); quiet(ingest())
c1 <- counts(); print(c1)
stopifnot(c1[["daily"]] == 8, c1[["log"]] == 8, c1[["latency"]] > 0,
          length(list.files(INCOMING_DIR)) == 0,
          length(list.files(file.path(RAW_DIR, D1))) == 9)
pass("2 day 1 ingested (8 rows), incoming emptied, 8 CSVs + chow archived")

# 3. re-run with nothing new
md5_before <- store_md5(); quiet(ingest())
stopifnot(identical(store_md5(), md5_before))
pass("3 re-run with empty incoming: store byte-identical")

# 4. same files dropped again (incl. a renamed copy)
dir.create(file.path(INCOMING_DIR, D1))
file.copy(list.files(file.path(RAW_DIR, D1), full.names = TRUE), file.path(INCOMING_DIR, D1))
file.copy(file.path(RAW_DIR, D1, "Arena 1.csv"), file.path(INCOMING_DIR, D1, "Arena 1 export2.csv"))
expect_error(quiet(ingest()), "More than one file for ARENA1 day 1")
unlink(file.path(INCOMING_DIR, D1, "Arena 1 export2.csv"))
quiet(ingest())
stopifnot(identical(store_md5(), md5_before), length(list.files(INCOMING_DIR)) == 0)
pass("4 reloading the same files: all skipped, store byte-identical")
put(D1, arenas = 1, seed = 1, names = "Arena 1 renamed.csv")
quiet(ingest())
stopifnot(identical(store_md5(), md5_before), length(list.files(INCOMING_DIR)) == 0,
          !file.exists(file.path(RAW_DIR, D1, "Arena 1 renamed.csv")))
pass("4b renamed copy of a loaded file: skipped by hash")

# 5. different file for an already-loaded mouse-day
put(D1, arenas = 3, seed = 99)
expect_error(quiet(ingest()), "Already loaded from a DIFFERENT file")
stopifnot(identical(store_md5(), md5_before))
unlink(file.path(INCOMING_DIR, D1), recursive = TRUE)
pass("5 conflicting re-export: error, store byte-identical")

# 6. bad chow
put(D2, seed = 2); quiet(ingest()); fill_chow(D2, bad = TRUE)
msg <- expect_error(quiet(ingest()), "Problems in chow.csv")
stopifnot(grepl("no row for", msg), grepl("end \\(30 g\\) > start", msg), identical(store_md5(), md5_before))
pass("6 bad chow (missing row, end > start): error, store byte-identical")

# 7. fixed chow -> day 2 in
unlink(file.path(INCOMING_DIR, D2, CHOW_FILE)); quiet(ingest())   # fresh template
fill_chow(D2); quiet(ingest())
stopifnot(counts()[["daily"]] == 16)
pass("7 day 2 ingested (16 rows)")

# 8. missing day label / bad folder
put("low_effort_no_light_sucrose", arenas = 1, seed = 3)
expect_error(quiet(ingest()), "No 'day N'")
unlink(file.path(INCOMING_DIR, "low_effort_no_light_sucrose"), recursive = TRUE)
put("LD low effort sucrose day 1", arenas = 1, seed = 3)
expect_error(quiet(ingest()), "Folder name not recognised")
unlink(file.path(INCOMING_DIR, "LD low effort sucrose day 1"), recursive = TRUE)
stopifnot(counts()[["daily"]] == 16)
pass("8 missing day label / unknown folder: refused")

# 9. interrupted run: data files swapped, log not
D3 <- "low_effort_no_light_sucrose day 3"
c2 <- counts()
put(D3, seed = 4); quiet(ingest()); fill_chow(D3); quiet(ingest())
stopifnot(counts()[["daily"]] == 24)
file.copy(sub("\\.parquet$", ".prev.parquet", STORE_FILES[["log"]]), STORE_FILES[["log"]], overwrite = TRUE)
expect_error(check_store(), "missing from ingest_log")
quiet(ingest())          # recovery runs at the start of every ingest
stopifnot(identical(counts(), c2))
check_store()
pass("9 interrupted run detected and rolled back to 16 rows")
unlink(file.path(RAW_DIR, D3), recursive = TRUE)

# 10. explicit replacement
put(D2, arenas = 2, seed = 77)
REPLACE_FILES <- "Arena 2.csv"
quiet(ingest(replace_files = REPLACE_FILES)); fill_chow(D2); quiet(ingest(replace_files = REPLACE_FILES))
d <- read_store("daily")
stopifnot(nrow(d) == 16, sum(d$mouse_day_id == "ARENA2_none_low_sucrose_d2") == 1,
          d$source_hash[d$mouse_day_id == "ARENA2_none_low_sucrose_d2"] ==
            unname(tools::md5sum(file.path(RAW_DIR, D2, "Arena 2.csv"))),
          length(list.files(file.path(RAW_DIR, D2), pattern = "replaced_")) == 2,
          nrow(read_csv(file.path(RAW_DIR, D2, CHOW_FILE), show_col_types = FALSE)) == 8)
check_store()
pass("10 REPLACE_FILES swaps one mouse-day; old copies kept; archived chow still complete")

# 11. second condition + baseline, for 02-07
put("high_effort_white_light_sucrose day 1", seed = 5)
put("Baseline day 1", seed = 6)
quiet(ingest()); for (f in list.files(INCOMING_DIR)) fill_chow(f); quiet(ingest())
stopifnot(counts()[["daily"]] == 32)
rf <- build_rf_features()
stopifnot(nrow(rf) == 32, all(RF_FEATURES %in% names(rf)), all(is.finite(rf$standard_chow_eaten)))
pass("11 build_rf_features: one row per mouse-day, all RF_FEATURES present")

# 12. rebuild from raw gives the same rows
before <- read_store("daily") %>% select(-source_hash) %>% arrange(mouse_day_id)
CONFIRM_REBUILD <- TRUE
src <- readLines("02c_rebuild_store.R"); src <- src[!grepl("^CONFIRM_REBUILD <- FALSE", src)]
quiet(eval(parse(text = src)))
after <- read_store("daily") %>% select(-source_hash) %>% arrange(mouse_day_id)
stopifnot(ok, isTRUE(all.equal(before, after, check.attributes = FALSE)))
pass("12 02c rebuild from raw/ reproduces the store")

cat("\nALL INGEST TESTS PASSED\n"); print(counts())
