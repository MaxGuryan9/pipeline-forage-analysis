## ══════════════════════════════════════════════════════════════════════════
## DAILY UPDATE: loads new EthoVision CSVs into the Parquet store.
## Run 00_config.R and 01_helpers.R first.
##
##   1. Put the day's CSVs in data/incoming/<condition folder> day N/
##      e.g. data/incoming/low_effort_no_light_sucrose day 2/Arena 1.csv
##   2. Run this script. The first time for a folder it writes chow.csv there
##      and stops - fill in standard_chow_start / standard_chow_end (grams).
##   3. Run this script again. The files are added to data/store/ and moved
##      to data/raw/.
##
## Safe to run any number of times: files already loaded are skipped, and if
## anything is wrong nothing is written. See DATA_DECISIONS.md.
## ══════════════════════════════════════════════════════════════════════════

ingest(INCOMING_DIR)
check_store(verbose = TRUE)
