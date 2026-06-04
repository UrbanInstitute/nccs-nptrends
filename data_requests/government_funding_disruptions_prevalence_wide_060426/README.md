# Government funding disruptions — prevalence (wider variable set)

Date: 2026-06-04

Companion to `government_funding_disruptions_prevalence_052626`, extended to a
wider set of variables. This request runs **only two test families**:
design-based **confidence intervals** (weighted proportions, logit method) and
design-based **chi-square** tests (Rao-Scott). No ANOVA / t-test rows.

## Scope

The 64-row template `data/input/Government Funding Disruptions - CI and Stats
Tests.xlsx` drives everything. Variables span disruption indicators
(`LLAnyDisruption`, `LLLost`, `LLDelay`, `LLStop`), staffing
(`LLItems_Employees`, `StaffingPlans_Hire`, `StaffingPlans_Layoff`), demand
(`MetDemand`), and program-service change (`PrgSrvc_Amt_Num`,
`PrgSrvc_Amt_Srvc`, `PrgSrvc_Amt_Area`). Disaggregators: `census_urban_area`,
`LLAnyDisruption`, and a special year-comparison grouping (see below). Some CI
rows have no disaggregation (overall estimate).

## Datasets (all git-ignored — restricted microdata)

| Dataset | Weight | Used for |
|---|---|---|
| `nptrends_y5_clean.csv` | `year5wt` | Year 5 (2025) PrgSrvc CI rows |
| `nptrends_y6_clean.csv` | `weight_year6plus` | Year 6 (2026) disruption / demand / PrgSrvc rows |
| `nptrends_y6_staffed_clean.csv` | `weight_year6plus` | Year 6 **staffed orgs only** (n=2,119) — staffing rows 9–20 |

## What each script does

- **`R/00_load.R`** — reads the three CSVs and the template.
- **`R/01_prep.R`** — builds three weighted designs (`des_y5`, `des_y6`,
  `des_staffed`), parses each template row, and defines the runners
  (`run_ci`, `run_chisq`, `run_year_compare`) plus `dispatch()`. Encodes the
  PrgSrvc denominator rule and the year-comparison handling described below.
- **`R/02_tests.R`** — iterates the 64 rows, fills the `result` column, writes
  a first cut of `data/output/stats_tests_results.xlsx`, and accumulates a tidy
  per-estimate frame.
- **`R/03_report.R`** — writes the long-format `data/output/stats_tests_pvalues.csv`
  (one row per estimate / test), computes Holm and Benjamini–Hochberg adjusted
  p-values across the chi-square family, and **re-writes the xlsx with
  `p_raw` / `p_holm` / `p_bh` columns** appended (populated for chi-square rows,
  blank for CI / year-comparison rows) so the spreadsheet itself carries the
  multiple-comparison context.
- **`R/04_panel_tests.R`** *(optional, standalone)* — paired-test prototype for
  year-over-year change: links Y5/Y6 on `EIN` (823-org panel) and runs McNemar /
  Stuart–Maxwell. See *"Paired panel tests"* below. Writes
  `data/output/panel_tests.csv`.

## Two decisions baked into the pipeline

### 1. `PrgSrvc_Amt_*` denominator: keep 97, drop 98

Per `data/dictionary/data_dictionary_20260527.xlsx`, the change-in-program-
service items are coded 1 = significant decrease, 2 = moderate decrease,
3 = no change, 4 = moderate increase, 5 = significant increase, 97 = N/A,
98 = unsure. The dictionary instruction is **"Keep 97 in the denominator but
remove 98 from the denominator."** So the analysis universe for any
`PrgSrvc_Amt_*` variable is `{1,2,3,4,5,97}`; codes 98 and missing are dropped.
(Verified: the six category CIs sum to 1.000 within each subgroup.) All other
variables use their full non-missing universe.

### 2. Year-comparison rows are descriptive, not a pooled chi-square

Template rows **35, 36, 49, 50, 63, 64** (spreadsheet rows 36, 37, 50, 51, 64,
65) are labeled "chi-square" with the disaggregation *"year 5 vs [year 6 where
LLAnyDisruption == 0] vs [year 6 where LLAnyDisruption == 1]"*. We do **not**
run a pooled between-year chi-square on these. Instead each of the three groups
gets its own weighted-proportion confidence interval under its own year's
weight, and the reader compares the intervals — the **descriptive** option the
client selected.

Why (grounded in `Methodology_of_the_National_Surveys…2024_2025_and_2026.docx`):

- **Panel overlap breaks the independence a chi-square assumes.** 1,859 orgs
  responded in ≥ 2 waves and 518 in all three, so the Y5 and Y6 "groups" share
  a large block of the *same* organizations. A χ² test of independence treats
  the groups as disjoint samples; the overlap makes its variance wrong
  (anti-conservative). The correct inferential tool for a test of change is a
  *paired* member of the chi-square family run on the linked panel — see
  **"Paired panel tests"** below, prototyped in `R/04_panel_tests.R`.
- **Separately-calibrated weights.** `year5wt` and `weight_year6plus` correct
  for different response-rate adjustment sets (region, size, subsector, and
  different state lists per wave). Each year's weighted proportion is a valid
  population estimate *for that year*, which is exactly what the descriptive
  side-by-side reports — but pooling them into one inferential target is not a
  use either weight construction supports.

The descriptive approach sidesteps both problems: no pooled design, no
independence assumption, just three valid year-specific estimates with CIs.
In the long-format CSV these rows carry `test_type =
"confidence interval (year comparison)"`.

### Paired panel tests (inferential alternative — `R/04_panel_tests.R`)

For anyone who *does* want a formal test of year-over-year change, the right
tool is the **paired** branch of the chi-square family, run on the same
organizations across waves. `R/04_panel_tests.R` is a prototype:

- **Links the panel on `EIN`** — a clean 1:1 key present in both files. The
  Y5 ∩ Y6 overlap is **823 organizations** (= the 518 all-three-wave + 305
  2025-&-2026-only respondents from the methodology doc's panel table).
- **McNemar's test** on each binary collapse (decrease = 1–2, increase = 4–5),
  mirroring the template's "1 and 2 combined" / "4 and 5 combined" rows.
- **Stuart–Maxwell test** on the 3-category collapse (decrease / no change /
  increase) for overall marginal homogeneity.
- Writes `data/output/panel_tests.csv`.

These respect the within-org correlation the independence χ² ignores. **Three
caveats must travel with any result** (also in the script header):

1. **Unweighted.** No panel weight exists in the files (only the cross-sectional
   `year5wt` / `weight_year6plus`); the prototype uses unweighted panel counts.
   A design-based weighted version needs a panel-weight decision from whoever
   built the weights.
2. **Attrition / selection.** The 823 panel orgs are repeat responders, not a
   random subset — results generalize to that group, not the full population.
3. **Period-referenced items.** `PrgSrvc_Amt_*` ask about change vs the prior
   year, so this compares the 2025 reference window to the 2026 one, not a fixed
   trait measured twice.

## Multiple-comparison adjustments (Holm and BH)

Only the **11 chi-square rows** carry a p-value (CI and year-comparison rows are
estimation, not testing). `03_report.R` reports both:

- **`p_holm`** — Holm step-down, controls the family-wise error rate. Use for
  confirmatory claims; a row surviving Holm is a robust finding.
- **`p_bh`** — Benjamini–Hochberg, controls the false discovery rate. Use for
  exploratory screening; a row surviving only BH is preliminary.

A row significant on raw `p_value` but neither adjusted column should generally
not be reported as a finding from this batch.

## Result-string conventions

| Test | Format |
|---|---|
| confidence interval | `level: est [lo, hi] (n=k)` joined by ` \| ` |
| chi-square | `X^2(df) = stat, p = p, n = n` |
| year comparison | `year5: … \| year6_LLAny0: … \| year6_LLAny1: …` (each a CI) |

## Outputs

- `data/output/stats_tests_results.xlsx` — template with `result` filled, plus
  `p_raw` / `p_holm` / `p_bh` columns on the chi-square rows
- `data/output/stats_tests_pvalues.csv` — long format, one row per estimate /
  test, with Holm / BH columns on the chi-square family. Includes an `se`
  column: the design-based SE of the proportion on the **linear** scale,
  provided to support group-difference contrasts. Because the CIs are
  logit-method (asymmetric), `est ± 1.96·se` does **not** reproduce `ci_l` /
  `ci_u` — use the columns for their intended purposes.
- `data/output/panel_tests.csv` — *(from the optional `R/04_panel_tests.R`)*
  McNemar / Stuart–Maxwell paired tests of Y5→Y6 change on the 823-org EIN
  panel. One row per (variable × test). Columns:
  - `variable` — the PrgSrvc_Amt_* outcome tested
  - `test` — `McNemar` (binary) or `Stuart-Maxwell` (3-category)
  - `contrast` — `decrease (vs not)`, `increase (vs not)`, or
    `3-cat marginal homogeneity`
  - `n_pairs` — panel orgs with a valid response in **both** waves (the test's
    effective n)
  - `b_discordant` / `c_discordant` — counts of orgs that switched *into* vs
    *out of* the category Y5→Y6 (McNemar rows only; the test is driven by these)
  - `prop_y5` / `prop_y6` — panel marginal proportions in each wave (McNemar
    rows only)
  - `statistic`, `df`, `p_value` — the test result

## Run order

```r
source("R/00_load.R")
source("R/01_prep.R")
source("R/02_tests.R")
source("R/03_report.R")
source("R/04_panel_tests.R")  # optional — paired panel tests of year-over-year change
```

Requires R packages `survey`, `data.table`, `openxlsx`, `here`.
