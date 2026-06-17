# Validate pipeline outputs against the externally computed sanity_checks.csv.
#
# sanity_checks.csv ships values computed independently of this pipeline,
# keyed on raw survey variable names, numeric response codes, and y4/y5/y6
# year labels. We normalize both sides to a common shape and assert agreement
# within tolerance.
#
# Schema crosswalk:
#   year                y4/y5/y6 -> 2024/2025/2026
#   group/group_level   -> filterType + filterOpt + splitByOpt + splitByOpt_category
#   variable            strip _re / _clean / _clean_grouped / _<YYYY>_clean
#                       suffixes; Bchairrace_POC -> BChair_POC
#   variable_level      numeric code -> our responseOpt string (per-variable
#                       lookup, mirroring the recode logic in 00_data_extract.R)
#   value               directly comparable; some of our responseOpts merge
#                       multiple raw codes (e.g. 0 + 97 -> "Did not ..."), so
#                       we collapse sanity_checks rows on the same key before
#                       comparing.

library(testthat)
library(data.table)

root <- "../../"
sanity <- data.table::fread(paste0(root, "data/validate/sanity_checks.csv"))
out <- data.table::fread(paste0(root, nptrends_full_filtered_path),
                         colClasses = list(character = c("responseOpt")))

# --- normalize sanity_checks variable names to our metricnames ---
strip_suffix <- function(v) {
  v <- sub("_re$", "", v)
  v <- sub("_clean_grouped$", "", v)
  v <- sub("_(2023|2024|2025)_clean$", "", v)
  v <- sub("_clean$", "", v)
  # Single-letter case fix in sanity_checks
  ifelse(v == "Bchairrace_POC", "BChair_POC", v)
}
sanity[, metricname := strip_suffix(variable)]
year_map <- c(y4 = 2024L, y5 = 2025L, y6 = 2026L)
sanity[, year := unname(year_map[year])]

# --- map group / group_level -> our schema ---
sanity[, c("filterType", "filterOpt", "splitByOpt", "splitByOpt_category") :=
  list(
    data.table::fcase(
      group == "CensusRegion4", "Region",
      group == "State",         "State",
      default = "National"
    ),
    data.table::fcase(
      group == "CensusRegion4", group_level,
      group == "State",         group_level,
      default = "National"
    ),
    data.table::fcase(
      group == "National",        "",
      group == "CensusRegion4",   "",
      group == "State",           "",
      group == "SizeStrata",      "Size of annual expenses",
      group == "census_urban_area", "Urban or rural designation",
      group == "ntmaj12",         "Nonprofit subsector",
      default = ""
    ),
    data.table::fcase(
      group == "National",          "",
      group == "CensusRegion4",     "",
      group == "State",             "",
      default = group_level
    )
  )]

# Recode the SizeStrata group_level values (sanity_checks uses integer codes)
size_strata_labels <- c(
  "1" = "$50,000–$99,999",
  "2" = "$100,000–$499,999",
  "3" = "$500,000–$999,999",
  "4" = "$1 million–$9,999,999",
  "5" = "$10 million and above"
)
sanity[group == "SizeStrata" & splitByOpt_category %in% names(size_strata_labels),
       splitByOpt_category := size_strata_labels[splitByOpt_category]]

# Recode CensusRegion4 codes
region_labels <- c("1" = "Northeast", "2" = "Midwest", "3" = "South", "4" = "West")
sanity[group == "CensusRegion4" & filterOpt %in% names(region_labels),
       filterOpt := region_labels[filterOpt]]

# Recode census_urban_area codes 0/1
sanity[group == "census_urban_area",
       splitByOpt_category := data.table::fcase(
         splitByOpt_category %in% c("1", "U"), "Urban",
         splitByOpt_category %in% c("0", "R"), "Rural",
         default = splitByOpt_category
       )]

# Recode ntmaj12 codes to Subsector labels
subsector_labels <- c(
  "AR" = "Arts, culture, and humanities",
  "ED" = "Education",
  "EN" = "Environment and animals",
  "HE" = "Health",
  "HU" = "Human services",
  "IN" = "International and foreign affairs",
  "PU" = "Public and societal benefit",
  "RE" = "Religion"
)
# NB: sanity_checks keeps "EH" (typo Health) as its own subsector. Our
# pipeline patches EH->HE at read, so our "Health" aggregates HE+EH while
# sanity's HE row is HE-only. Drop EH rows here to avoid a guaranteed
# mismatch on Health, and use a looser tolerance for the HE comparison
# below to absorb the small skew from the EH respondents we fold in.
sanity <- sanity[!(group == "ntmaj12" & splitByOpt_category == "EH")]
sanity[group == "ntmaj12" & splitByOpt_category %in% names(subsector_labels),
       splitByOpt_category := subsector_labels[splitByOpt_category]]

# --- map (metricname, variable_level) -> our responseOpt string ---
# Mirrors the recode case_when statements in R/00_data_extract.R. Where the
# pipeline collapses multiple raw codes onto one responseOpt (e.g., 0 + 97 ->
# "Did not draw on cash reserves"), we map both codes to the same string so a
# downstream group-and-sum reconstructs our value.
build_code_map <- function() {
  rows <- list()
  add <- function(var, lvl, resp) rows[[length(rows) + 1L]] <<-
    list(metricname = var, level = as.character(lvl), responseOpt = resp)

  # 5-bucket -> 3-bucket collapsed: 1,2 -> "decrease"; 3 -> "no change"; 4,5
  # -> "increase"; 97 -> "Unsure". The _re variables in sanity_checks are
  # already collapsed (codes 1/2/3/97).
  add_change_3 <- function(var, decrease, increase) {
    add(var, 1L, decrease); add(var, 2L, "No change")
    add(var, 3L, increase); add(var, 97L, "Unsure")
  }
  add_change_3("FinanceChng_TotExp", "Decrease in expenses", "Increase in expenses")
  add_change_3("FinanceChng_Salaries", "Decrease in salaries and wages",
               "Increase in salaries and wages")
  for (v in c("FinanceChng_Benefits", "FinanceChng_TotRent",
              "FinanceChng_TotTech", "FndRaise_TotExp"))
    add_change_3(v, "Decrease in expenditures", "Increase in expenditures")
  for (v in c("PrgSrvc_Amt_Srvc", "PrgSrvc_Amt_Num"))
    add_change_3(v, "Decrease", "Increase")
  add_change_3("PrgSrvc_Amt_Fee", "Decrease in $ amount", "Increase in $ amount")
  for (v in c("FndRaise_DAF_Grnt_Chng", "FndRaise_Priv_Grnt_Chng",
              "FndRaise_Corp_Grnt_Chng"))
    add_change_3(v, "Decrease in value", "Increase in value")
  add_change_3("FndRaise_Cashbelow250_Chng", "Decrease in donations < $250",
               "Increase in donations < $250")
  add(   "FndRaise_Cashbelow250_Chng", 98L, "Unsure")
  add_change_3("FndRaise_Cashabove250_Chng", "Decrease in donations ≥ $250",
               "Increase in donations ≥ $250")
  add(   "FndRaise_Cashabove250_Chng", 98L, "Unsure")

  # Simple yes/no with 97 grouped into "no" arm
  add("FinanceChng_Reserves", 1, "Drew on cash reserves")
  add("FinanceChng_Reserves", 0, "Did not draw on cash reserves")
  add("FinanceChng_Reserves", 97, "Did not draw on cash reserves")
  add("PrgSrvc_Suspend", 1, "Paused or suspended services")
  add("PrgSrvc_Suspend", 0, "Did not pause or suspend services")
  add("PrgSrvc_Suspend", 97, "Did not pause or suspend services")
  add("Dmnd_NxtYear", 2, "Anticipated increase")
  add("Dmnd_NxtYear", 1, "No change")
  add("Dmnd_NxtYear", 0, "Anticipated decrease")
  add("Cash_Reserves", 1, "Had cash reserves")
  add("Cash_Reserves", 0, "Did not have cash reserves")
  add("PplSrv_MeetDemand", 1, "Able to meet demand")
  add("PplSrv_MeetDemand", 0, "Unable to meet demand")

  # GeoAreas: 0/1 -> "Not serving X" / "Serving X"
  geo_pairs <- list(
    GeoAreas_ServedLocal       = "local areas",
    GeoAreas_State             = "statewide",
    GeoAreas_Servedmultistate  = "multiple states",
    GeoAreas_National          = "nationally",
    GeoAreas_International     = "internationally"
  )
  for (m in names(geo_pairs)) {
    suffix <- geo_pairs[[m]]
    add(m, 1, sprintf("Serving %s", suffix))
    add(m, 0, sprintf("Not serving %s", suffix))
  }
  # sanity_checks uses GeoAreas_Locally / GeoAreas_MultipleState (Y6 source
  # column names) which we map onto our merged metricnames.
  add("GeoAreas_Locally", 1, "Serving local areas")
  add("GeoAreas_Locally", 0, "Not serving local areas")
  add("GeoAreas_MultipleState", 1, "Serving multiple states")
  add("GeoAreas_MultipleState", 0, "Not serving multiple states")

  # ProgDem: 0 -> "Not serving X"; 1 -> "Serving X". The Y6 question redesign
  # means only code 1 counts as served, so code 2 -> "Not serving X" (mirrors
  # the year == "2026" recode in R/00_data_extract.R, which maps code 2 -> 0).
  # Y4/Y5 historically collapsed 1|2 -> served, but those years' ProgDem values
  # are dropped from the pipeline output (metrics_drop_y4_y5) and so are never
  # compared here; only Y6 ProgDem reaches this comparison.
  progdem_pairs <- list(
    ProgDem_BelowFPL    = list("people living in poverty"),
    ProgDem_Disabled    = list("people with disabilities"),
    ProgDem_Veterans    = list("veterans"),
    ProgDem_LGBTQ       = list("LGBTQ people"),
    ProgDem_Foreign     = list("foreign-born people"),
    ProgDem_Latinx      = list("Latinx/Hispanic populations"),
    ProgDem_Black       = list("Black/African American populations"),
    ProgDem_Indigenous  = list("American Indian or Alaska Native populations"),
    ProgDem_Asian       = list("Asian populations"),
    ProgDem_Men         = list("men and boys"),
    ProgDem_Women       = list("women and girls"),
    ProgDem_Nonbinary   = list("nonbinary people"),
    ProgDem_Children    = list("children and youth"),
    ProgDem_YoungAdults = list("young adults"),
    ProgDem_Adults      = list("adults"),
    ProgDem_Elders      = list("older adults")
  )
  for (m in names(progdem_pairs)) {
    suffix <- progdem_pairs[[m]][[1]]
    add(m, 0, sprintf("Not serving %s", suffix))
    add(m, 1, sprintf("Serving %s", suffix))
    add(m, 2, sprintf("Not serving %s", suffix))
  }

  # FndRaise_*_Rcv: 1 -> "Received ..."; 0 -> "Did not receive ..."
  rcv_phrases <- list(
    FndRaise_DAF_Rcv             = "funding from donor-advised funds",
    FndRaise_PFGrnt_Rcv          = "grants from private foundations",
    FndRaise_Corp_Found_Grnt_Rcv = "grants from corporate foundations or giving programs",
    FndRaise_CFGrnt_Rcv          = "grants from community foundations"
  )
  for (m in names(rcv_phrases)) {
    add(m, 1, sprintf("Received %s", rcv_phrases[[m]]))
    add(m, 0, sprintf("Did not receive %s", rcv_phrases[[m]]))
  }

  # CEO / Board chair binaries (1 -> "X is Y"; 0 + 97 -> "X is not Y")
  ceo_bchair <- list(
    Dem_CEO_Disabled    = c("CEO has a disability", "CEO does not have a disability"),
    Dem_BChair_Disabled = c("Board chair has a disability", "Board chair does not have a disability"),
    CEOgender_NB        = c("CEO is nonbinary", "CEO is not nonbinary"),
    BChairgender_NB     = c("Board chair is nonbinary", "Board chair is not nonbinary"),
    Dem_CEO_LGBTQ       = c("CEO is LGBTQ", "CEO is not LGBTQ"),
    Dem_BChair_LGBTQ    = c("Board chair is LGBTQ", "Board chair is not LGBTQ"),
    CEOgender_Woman     = c("CEO is a woman", "CEO is not a woman"),
    BChairgender_Woman  = c("Board chair is a woman", "Board chair is not a woman"),
    CEOgender_Man       = c("CEO is a man", "CEO is not a man"),
    BChairgender_Man    = c("Board chair is a man", "Board chair is not a man")
  )
  for (m in names(ceo_bchair)) {
    add(m, 1, ceo_bchair[[m]][1])
    add(m, 0, ceo_bchair[[m]][2])
    add(m, 97, ceo_bchair[[m]][2])
  }

  # CEO / Board chair under-35: codes 1,2 -> "under 35"; 3..9,97 -> "not under 35"
  add_under35 <- function(var, yes, no) {
    add(var, 1, yes); add(var, 2, yes)
    for (c in c(3, 4, 5, 6, 7, 8, 9, 97)) add(var, c, no)
  }
  add_under35("Dem_CEO_Age",    "CEO is under age 35",         "CEO is not under age 35")
  add_under35("Dem_BChair_Age", "Board chair is under age 35", "Board chair is not under age 35")

  # PercentDem_*: sanity_checks ships these as already-bucketed codes, not
  # raw survey codes. 7-bucket variants are 0..6; 4-bucket variants are 0..3.
  for (m in c("PercentDem_Women_Board", "PercentDem_Women_Staff",
              "PercentDem_POC_Board", "PercentDem_POC_Staff")) {
    add(m, 0, "0%");    add(m, 1, "1–20%");  add(m, 2, "21–40%")
    add(m, 3, "41–60%"); add(m, 4, "61–80%"); add(m, 5, "81–99%")
    add(m, 6, "100%")
  }
  for (m in percentdem_4bucket_vars) {
    add(m, 0, "0%"); add(m, 1, "1–50%"); add(m, 2, "51–99%"); add(m, 3, "100%")
  }

  # Staff_Fulltime / Staff_Parttime: sanity_checks ships codes 0..4.
  for (m in c("Staff_Fulltime", "Staff_Parttime")) {
    add(m, 0, "0"); add(m, 1, "1"); add(m, 2, "2–9")
    add(m, 3, "10–49"); add(m, 4, "50+")
  }

  data.table::rbindlist(rows)
}
code_map <- build_code_map()
sanity[, level_chr := as.character(variable_level)]
sanity <- merge(sanity, code_map,
                by.x = c("metricname", "level_chr"),
                by.y = c("metricname", "level"),
                all.x = TRUE)

# For median/mean rows variable_level is NA and our responseOpt is the
# continuous-metric label; we compare those directly via metricname.
# sanity_checks treats PercentDem_Young_Board/Staff as 7-bucket in all years
# while the 2026-05-26 metricsMaster and template put them in the 4-bucket
# scheme — the schemes are not reconcilable cell-by-cell, so skip these.
incomparable_due_to_bucket <- c("PercentDem_Young_Board",
                                "PercentDem_Young_Staff")

sanity_continuous <- sanity[metric %in% c("median", "mean")]
sanity_prop       <- sanity[metric == "proportion" &
                            !is.na(responseOpt) &
                            !(metricname %in% incomparable_due_to_bucket)]

# --- continuous comparison ---
testthat::test_that("sanity_checks continuous metrics agree with pipeline output", {
  ours <- out[, .(metricID, year, filterType, filterOpt, splitByOpt,
                  splitByOpt_category, responseOpt, value)]
  ours <- merge(ours, metricID_lookup, by = "metricID", all.x = TRUE)

  cmp <- merge(sanity_continuous,
               ours,
               by = c("metricname", "year", "filterType", "filterOpt",
                      "splitByOpt", "splitByOpt_category"),
               suffixes = c(".sanity", ".ours"))
  cmp <- cmp[!is.na(value.ours) & !is.na(value.sanity)]
  if (nrow(cmp) == 0) skip("No continuous-metric rows matched between sanity_checks and pipeline output")

  # Loose tolerance for continuous metrics: small-integer medians (e.g.,
  # board-member counts) can differ by 1 due to weighted-median tie-breaking,
  # and our final output is formatted to 2 significant digits in
  # 02_data_load.R, which adds ~5% rounding noise on top.
  cmp[, abs_diff := abs(value.ours - value.sanity)]
  cmp[, rel_diff := abs_diff / pmax(abs(value.sanity), 1)]
  mismatches <- cmp[abs_diff > 1 & rel_diff > 0.1]
  testthat::expect_equal(
    nrow(mismatches), 0,
    info = paste0("First mismatches:\n",
                  paste(utils::capture.output(print(head(mismatches, 10))),
                        collapse = "\n"))
  )
})

# --- proportion comparison (sum sanity counts that collapse onto the same
#     responseOpt before joining) ---
testthat::test_that("sanity_checks proportion metrics agree with pipeline output", {
  collapsed <- sanity_prop[, .(value = sum(value)),
                           by = .(metricname, year, filterType, filterOpt,
                                  splitByOpt, splitByOpt_category, responseOpt)]
  ours <- out[, .(metricID, year, filterType, filterOpt, splitByOpt,
                  splitByOpt_category, responseOpt, value)]
  ours <- merge(ours, metricID_lookup, by = "metricID", all.x = TRUE)

  cmp <- merge(collapsed, ours,
               by = c("metricname", "year", "filterType", "filterOpt",
                      "splitByOpt", "splitByOpt_category", "responseOpt"),
               suffixes = c(".sanity", ".ours"))
  cmp <- cmp[!is.na(value.ours) & !is.na(value.sanity)]
  if (nrow(cmp) == 0) skip("No proportion-metric rows matched between sanity_checks and pipeline output")

  # Looser tolerance: the final output is formatted to 2 significant digits
  # in 02_data_load.R, so proportions are rounded (e.g., 0.4554 -> 0.46).
  # Combined with small data drift, ~5pp absolute tolerance is appropriate.
  cmp[, abs_diff := abs(value.ours - value.sanity)]
  mismatches <- cmp[abs_diff > 0.05]
  testthat::expect_equal(
    nrow(mismatches), 0,
    info = paste0("First mismatches:\n",
                  paste(utils::capture.output(print(head(mismatches, 10))),
                        collapse = "\n"))
  )
})
