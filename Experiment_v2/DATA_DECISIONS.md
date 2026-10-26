# Experiment_v2 data decisions

## Status: PROVISIONAL — built before any real EthoVision export existed

The ingest pipeline (`02a_ingest_new_data.R`) was designed and tested on **fake** EthoVision-style
files. Several decisions depend on what a real export contains. When the first real file arrives,
work through the checklist below before trusting the store, and change any decision that a real
file shows to be wrong.

## Current design

- **Storage:** three continuously growing Parquet files in `data/store/`, queried with DuckDB.
  - `daily_summary.parquet` — one row per mouse × day (behaviour totals, peak wheel rate, chow weights).
  - `latency_events.parquet` — one row per zone → zone trip (wheel → cup, pellet → cup).
  - `ingest_log.parquet` — one row per source CSV loaded (file name, MD5 hash, rows read, ingest time).
- **Stored level of detail:** daily summaries + latency events, the same tables the pipeline
  already computed. Frame-by-frame storage was considered and deferred pending supervisor review.
- **Drop-off:** new CSVs go in `data/incoming/<condition folder> day N/`; loaded files move to
  `data/raw/<same folder name>/`.
- **Condition:** parsed from the folder name (`parse_condition()` in `01_helpers.R`).
- **Day number:** must come from a `day N` label in the folder name or file name. Ingest refuses
  files without one. Automatic "next day" numbering was rejected: a re-export of an already-loaded
  day would be numbered as a new day and slip past the duplicate check.
- **Unique ID:** `mouse_day_id = <arena>_<condition>_d<day>` (e.g. `ARENA3_none_low_sucrose_d2`).
- **Chow weights:** entered in `chow.csv`, which ingest generates per incoming folder with the IDs
  pre-filled; the user fills `standard_chow_start` / `standard_chow_end` (grams).
- **Duplicates:** a file whose MD5 hash is already logged is skipped; a different file for an
  already-loaded `mouse_day_id` stops ingest unless listed in `REPLACE_FILES`.

## Checklist for the first real export

- [ ] Header: does it contain a recording start date/time? If yes, consider switching
      `mouse_day_id` to `<arena>_<date>` and dropping the `day N` folder-label requirement.
      (Ingest already stores the header's "Start time" line, if any, in `recording_start`.)
- [ ] Header: is there an arena / subject field? Compare with the arena parsed from the file name.
- [ ] Line 1 has "Number of header lines"? (`find_header_skip()` relies on it, with fallbacks.)
- [ ] Column names match `COLUMN_MAP` in `00_config.R` exactly (case-insensitive).
- [ ] `pellet earned`, `licks`, `wheel`, `nose pokes` are per-frame counts, not running totals
      (the code sums them).
- [ ] Frame rate and file size — relevant if frame-level storage is revisited.
- [ ] Recordings start at ZT 1 (`DEFAULT_ZT_OFFSET`).
- [ ] Run `02a` on the real files, then `check_store()`, and spot-check one mouse-day by hand.
