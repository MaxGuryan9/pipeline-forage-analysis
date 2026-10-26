## ══════════════════════════════════════════════════════════════════════════
## REBUILD: recomputes the whole store from the archived CSVs in data/raw/.
## Use after changing how a summary is calculated (e.g. a new behaviour in
## process_file()). Run 00_config.R and 01_helpers.R first.
##
## The current store is moved to data/store_backup_<time>/ first and put back
## if the rebuild fails.
## ══════════════════════════════════════════════════════════════════════════

CONFIRM_REBUILD <- FALSE   # set to TRUE to run

if (!CONFIRM_REBUILD) {
  message("02c: set CONFIRM_REBUILD <- TRUE to rebuild the store from data/raw/.")
} else {
  backup_dir <- file.path(DATA_DIR, paste0("store_backup_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  file.rename(STORE_DIR, backup_dir)
  dir.create(STORE_DIR)
  cat("Old store moved to", backup_dir, "\n")

  ok <- tryCatch({
    ingest(RAW_DIR, archive = FALSE, replace_files = c())
    # every archived CSV must now be in the log (ingest returns early, without
    # error, if a folder is missing its chow.csv)
    n_raw <- nrow(index_folder_files(RAW_DIR) %||% tibble())
    n_log <- nrow(read_store("log"))
    if (n_log != n_raw) stop(n_raw, " CSVs in data/raw/ but ", n_log, " loaded")
    TRUE
  }, error = function(e) { message("Rebuild failed: ", conditionMessage(e)); FALSE })

  if (ok) {
    file.create(file.path(STORE_DIR, ".gitkeep"))
    check_store(verbose = TRUE)
  } else {
    unlink(STORE_DIR, recursive = TRUE)
    file.rename(backup_dir, STORE_DIR)
    message("The previous store was restored unchanged.")
  }
}
