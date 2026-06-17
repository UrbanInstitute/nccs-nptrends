# CLAUDE.md

Data-engineering code for the **Nonprofit Trends and Impacts (NPTrends)** survey
dashboard (Urban Institute). It ingests the restricted per-year survey datasets,
computes weighted summary statistics for ~90 metrics, formats them to match the
dashboard developer's template, and emits the final dashboard dataset plus
Urban data-catalog workbooks.

Survey years in scope: **Year 4 = 2024, Year 5 = 2025, Year 6 = 2026.**

## Pipeline — run in order from the project root

The numbered scripts in `R/` are the pipeline; run them in sequence (e.g. in
RStudio with the `.Rproj` open, or `Rscript R/0X_*.R`). Each reads the previous
stage's output file, so order matters.

| Step | Script | Reads | Writes |
|------|--------|-------|--------|
| 00 | `R/00_data_extract.R`   | raw `Y:` per-year CSVs | `data/intermediate/nptrends_full_preprocessed.csv` |
| 01 | `R/01_data_transform.R` | preprocessed | `data/intermediate/nptrends_full_transformed.csv` |
| 02 | `R/02_data_load.R`      | transformed + template | `data/processed/nptrends_full_filtered.csv` + runs the test suite |

Non-numbered scripts are sourced as helpers, not run standalone:
- `R/config.R` — all file paths, the `survey_analysis_vars` column list, per-metric
  `metrics` (summary method), recode groupings, weight selection, and year/state
  exclusions. **This is the main knob board.**
- `R/summarize_survey_results.R` — `svy_tfm()` and helpers: the weighted
  filter/groupby/summarise engine. Methods: `"median"` (weighted median via
  `spatstat.univar::weighted.median`), `"average of %"` (weighted mean),
  `"% of respondents"` (weighted proportion by response option).
- `R/metadata.R` — loads the template + `metricsMaster`, and holds the
  `metricID_lookup` (metric name ↔ metricID 1–90) and response-option lookups.
- `R/data_catalog.R` — separate downstream step: reads
  `data/processed/nptrends_full_filtered.csv` and writes the Urban data-catalog
  `.xlsx` workbooks + Data Dictionary to `data/catalog/`.

## Receiving a new data template (recurring task)

The dashboard developer (Jeff MacInnes) periodically sends a new `dataTemplate`
(the canonical output skeleton — every metric/filter/split/responseOpt row, with
`value` blanked) and sometimes a new `metricsMaster`.

1. Drop the template in `data/validate/` named `dataTemplate_YYYYMMDD.csv`. The
   date is the **template's version date**, not the file's mtime.
2. Update `template_path` in `R/config.R` to point at it. If a new
   `metricsMaster` came too, save it as `data/validate/metricsMaster_YYYYMMDD.csv`
   and update `metricsMaster_path` in **both** `R/config.R` and `R/data_catalog.R`.
3. Cheap sanity check before running: the new template's header and row count
   should match the previous one (`head -1` + `wc -l`). A structural change there
   will silently break the keyed merge in `R/02_data_load.R`.
4. Run the pipeline (steps 00→02). Step 02 runs `testthat` at the end.

## Environment

- **`Y:` drive access is required.** Raw restricted datasets are read from
  `Y:/CNP/Generosity Commission/Year {4,5,6}/...` (paths in `R/config.R:8-12`).
  This is a mapped Windows network drive — it is **not** visible from WSL unless
  explicitly mounted (`sudo mount -t drvfs Y: /mnt/y`), so the pipeline normally
  runs on the Windows side. Do not commit any local repointing of these paths.
- Key R packages: `tidyverse`, `data.table`, `spatstat.univar`, `tidylog`,
  `rlang`, `here`, `openxlsx`, `testthat`. On a fresh Linux R, install
  `spatstat.univar`, `tidylog`, `tidyverse`, `spatstat` (Posit P3M binaries for
  the matching Ubuntu codename are far faster than source CRAN).

## Tests

`tests/testthat/` (run automatically by step 02, or `testthat::test_dir("tests/testthat")`):
- `test-template_permutations.R` — output rows conform to the template skeleton.
- `test-data-validation.R` / `test-sanity-checks.R` — value-level checks against
  `data/validate/` fixtures.
- `test-data-dictionary.R` — data-dictionary coverage.

## Conventions & gotchas

- **Year 6 raw differs from Y4/Y5** in many encodings; `R/00_data_extract.R`
  normalizes them on read (uppercase `CENSUS_URBAN_AREA`, string `SizeStrata`
  buckets, `"U"/"R"` urban codes, `EH`→`HE` subsector typo, integer64 casts) and
  via `year == "2026"` branches in the recodes. When adding a metric, check
  whether Y6 ships a pre-cleaned `*_clean`/`*_new` variant.
- **Per-metric Y6 weights**: most metrics use `weight_year6plus`/`stateweightplus`;
  the `FndRaise_*` Rcv/Chng/TotExp set uses `weight_year6`/`stateweight` — see
  `y6_metric_weights` in `R/config.R`. Y4/Y5 use `year4wt`/`year5wt` + `stateweight`.
- **Cross-year comparability**: some Y6 question redesigns make Y4/Y5 values
  non-comparable; those are blanked for 2024/2025 via `metrics_drop_y4_y5`
  (`R/config.R`) in step 02. Per-year descriptive CIs only — between-year χ² is
  invalid because the panel overlaps.
- **Min-response suppression**: groups with < 25 responses are set to `NA` in
  step 02; whole states are dropped per year via `states_to_exclude`.
- Commit messages follow Conventional Commits. Don't commit large data files
  (>100 MB) or `.claude/settings.local.json`.
</content>
</invoke>
