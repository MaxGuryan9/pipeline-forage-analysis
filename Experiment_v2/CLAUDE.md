# Experiment_v2 — notes for Claude Code

- Read `DATA_DECISIONS.md` first. The ingest design is PROVISIONAL: it was built before any real
  EthoVision export existed. When the user adds or mentions a first real file, go through its
  checklist and raise any decision the real file contradicts.
- Hard rule: the store must never contain duplicate rows. Duplicates invalidate the experiment.
  Any change to `02a_ingest_new_data.R` or the store helpers in `01_helpers.R` must keep ingest
  idempotent, and must be tested by ingesting the same files twice and checking `check_store()`.
- Data under `data/` is git-ignored; never commit it.
