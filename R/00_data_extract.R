# Data processing script for Year 4 and 5 of NP Trends survey

# Libraries
library(data.table)
library(tidyverse)
library(spatstat)
library(rlang)
library(stats)

# Helper functions
source(here::here("R", "summarize_survey_results.R"))
source(here::here("R", "config.R"))

# Load in raw survey data — only request the columns that exist in each year's
# raw file so adding Y6-specific columns to survey_analysis_vars doesn't break
# reads against the unchanged Y4/Y5 files.
read_year <- function(path, year_label, vars) {
  header <- names(data.table::fread(path, nrows = 0L))
  # Year 6 supplies CENSUS_URBAN_AREA in uppercase; the rest of the pipeline
  # uses the lowercase Y4/Y5 spelling. Request whichever name is on disk and
  # rename to the canonical lowercase form.
  rename_map <- c(census_urban_area = "CENSUS_URBAN_AREA")
  for (canonical in names(rename_map)) {
    raw <- rename_map[[canonical]]
    if (canonical %in% vars && !(canonical %in% header) && raw %in% header) {
      vars <- c(setdiff(vars, canonical), raw)
    }
  }
  df <- data.table::fread(path, select = intersect(vars, header))
  for (canonical in names(rename_map)) {
    raw <- rename_map[[canonical]]
    if (raw %in% names(df)) data.table::setnames(df, raw, canonical)
  }
  # Year 6 supplies SizeStrata as pre-labeled strings (with ASCII hyphens and
  # a "<$100,000" bottom bucket). Reverse-map to the Y4/Y5 integer codes so
  # bind_rows sees a uniform type and the downstream factor() recode works.
  # Note: "<$100,000" is mapped to code 1 ($50,000-$99,999) on the assumption
  # that Y6's sample frame still starts at $50k. Confirm with the data team
  # if Y6 begins below $50k.
  # Year 6 prefixes CensusRegion4 codes with a label ("1: Northeast"); Y4/Y5
  # use bare integers. Extract the leading integer.
  if ("CensusRegion4" %in% names(df) && is.character(df$CensusRegion4)) {
    df[, CensusRegion4 := as.integer(sub(":.*$", "", CensusRegion4))]
  }
  # Year 6 raw contains a typo where Health is sometimes coded "EH" instead
  # of "HE". Patch it on read so the subsector mapping picks it up.
  if ("ntmaj12" %in% names(df)) {
    df[ntmaj12 == "EH", ntmaj12 := "HE"]
  }
  # Year 6 sometimes parses large numerics (e.g. TotExp) as integer64 because
  # the column has no fractional values. Y4/Y5 read as double. Cast back to
  # double so bind_rows succeeds and arithmetic stays consistent.
  int64_cols <- names(df)[vapply(df, bit64::is.integer64, logical(1))]
  for (col in int64_cols) {
    df[, (col) := as.numeric(get(col))]
  }
  # Year 6 codes census_urban_area as "U"/"R"; Y4/Y5 use 1/0. Reverse-map to
  # integers so bind_rows + the downstream case_when both work.
  if ("census_urban_area" %in% names(df) && is.character(df$census_urban_area)) {
    df[, census_urban_area := data.table::fcase(
      census_urban_area == "U", 1L,
      census_urban_area == "R", 0L,
      default = NA_integer_
    )]
  }
  if ("SizeStrata" %in% names(df) && is.character(df$SizeStrata)) {
    size_lookup <- c(
      "<$100,000"             = 1L,
      "$50,000-$99,999"       = 1L,
      "$50,000–$99,999"  = 1L,
      "$100,000-$499,999"     = 2L,
      "$100,000–$499,999"= 2L,
      "$500,000-$999,999"     = 3L,
      "$500,000–$999,999"= 3L,
      "$1 million-$9,999,999" = 4L,
      "$1 million–$9,999,999" = 4L,
      "$10 million and above" = 5L
    )
    df[, SizeStrata := size_lookup[SizeStrata]]
  }
  df |> dplyr::mutate(year = year_label)
}
nptrends_y4_raw <- read_year(nptrends_y4_raw_path, "2024", survey_analysis_vars)
nptrends_y5_raw <- read_year(nptrends_y5_raw_path, "2025", survey_analysis_vars)
nptrends_y6_raw <- read_year(nptrends_y6_raw_path, "2026", survey_analysis_vars)
nptrends_full_raw <- dplyr::bind_rows(nptrends_y4_raw,
                                      nptrends_y5_raw,
                                      nptrends_y6_raw)

# Preprocess data
nptrends_full_preprocessed <- nptrends_full_raw |>
  dplyr::mutate(
    SizeStrata = factor(
      SizeStrata,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "$50,000–$99,999",
        "$100,000–$499,999",
        "$500,000–$999,999",
        "$1 million–$9,999,999",
        "$10 million and above"
      )
    ),
    Subsector = dplyr::case_when(
      ntmaj12 == "AR" ~ "Arts, culture, and humanities",
      ntmaj12 == "ED" ~ "Education",
      ntmaj12 == "EN" ~ "Environment and animals",
      ntmaj12 == "HE" ~ "Health",
      ntmaj12 == "HU" ~ "Human services",
      ntmaj12 == "IN" ~ "International and foreign affairs",
      ntmaj12 == "PU" ~ "Public and societal benefit",
      ntmaj12 == "RE" ~ "Religion",
      .default = NA
    ),
    census_urban_area = dplyr::case_when(census_urban_area == 1 ~ "Urban", census_urban_area == 0 ~ "Rural", ),
    CensusRegion4 = factor(
      CensusRegion4,
      levels = c(1, 2, 3, 4),
      labels = c("Northeast", "Midwest", "South", "West")
    ),
    state = dplyr::coalesce(state, State),
    CensusRegion4 = dplyr::case_when(
      state == "NY" & CensusRegion4 == "West" ~ "Northeast",
      state == "CO" & CensusRegion4 == "Midwest" ~ "West",
      state == "MO" & CensusRegion4 == "West" ~ "Midwest",
      state == "CA" & CensusRegion4 == "South" ~ "West",
      state == "AZ" & CensusRegion4 == "South" ~ "West",
      .default = CensusRegion4
    ),
    National = "National"
  ) %>%
  # Percent variables shouold be under 100%
  dplyr::mutate(
    dplyr::across(
      .cols = percent_vars,
      .fns = ~ ifelse(.x > 1, NA_real_, .x)
    )
  ) %>% 
  # For Year 6, the data team supplies cleaned variants of several variables
  # (Reserves_Est_clean, PplSrv_NumWait_clean, Staff_*_2025_clean, etc.) and
  # split the geography questions into single yes/no columns. Coalesce the Y6
  # values into the Y4/Y5 names so downstream recodes work uniformly.
  dplyr::mutate(
    Reserves_Est = dplyr::if_else(year == "2026",
                                  Reserves_Est_clean,
                                  Reserves_Est)
  ) %>%
  # Binary flags for new variables where 1 if any of the variables are 1 and 0 if all are 0
  dplyr::mutate(
    percent_expenses_inCashReserves = dplyr::case_when(
      is.na(Reserves_Est) & Reserves_NA_X == 1 ~ 0,
      is.na(TotExp) | TotExp == 0 ~ NA_real_,
      .default = Reserves_Est / TotExp
    ),
    GeoAreas_ServedLocal_y4y5 = binary_flag(
      data = .,
      GeoAreas_Local,
      GeoAreas_MultipleLocal,
      GeoAreas_RegionalWithin
    ),
    GeoAreas_ServedLocal = dplyr::if_else(
      year == "2026",
      as.numeric(GeoAreas_Locally),
      as.numeric(GeoAreas_ServedLocal_y4y5)
    ),
    GeoAreas_Servedmultistate_y4y5 = binary_flag(data = .,
                                                 GeoAreas_MultipleState,
                                                 GeoAreas_RegionalAcross),
    GeoAreas_Servedmultistate = dplyr::if_else(
      year == "2026",
      as.numeric(GeoAreas_MultipleState),
      as.numeric(GeoAreas_Servedmultistate_y4y5)
    ),
    FndRaise_DnrBlw250_ratio = FndRaise_DnrBlw250 / (FndRaise_DnrAbv250 + FndRaise_DnrBlw250),
    FndRaise_DnrAbv250_ratio = FndRaise_DnrAbv250 / (FndRaise_DnrBlw250 + FndRaise_DnrAbv250),
    Staff_RegVlntr_2023 = dplyr::case_when(
      is.na(Staff_RegVlntr_2023) &
        Staff_RegVlntr_NA == 1 ~ 0,
      .default = Staff_RegVlntr_2023
    ),
    Staff_EpsdVlntr_2023 = dplyr::case_when(
      is.na(Staff_EpsdVltnr_2023) &
        Staff_EpsdVltnr_NA == 1 ~ 0,
      .default = Staff_EpsdVltnr_2023
    ),
    Staff_RegVlntr_2024 = dplyr::case_when(
      is.na(Staff_RegVlntr_2024) &
        Staff_RegVlntr_NA == 1 ~ 0,
      .default = Staff_RegVlntr_2024
    ),
    Staff_EpsdVlntr_2024 = dplyr::case_when(
      is.na(Staff_EpsdVltnr_2024) &
        Staff_EpsdVltnr_NA == 1 ~ 0,
      .default = Staff_EpsdVltnr_2024
    ),
    Staff_Boardmmbr_2023 = dplyr::case_when(
      is.na(Staff_Boardmmbr_2023) &
        Staff_Boardmmbr_NA == 1 ~ 0,
      .default = Staff_Boardmmbr_2023
    ),
    Staff_Boardmmbr_2024 = dplyr::case_when(
      is.na(Staff_Boardmmbr_2024) &
        Staff_Boardmmbr_NA == 1 ~ 0,
      .default = Staff_Boardmmbr_2024
    ),
    PplSrv_MeetDemand = dplyr::case_when(
      year == "2026" & PplSrv_NumWait_clean == 0 ~ "Able to meet demand",
      year == "2026" & PplSrv_NumWait_clean > 0 ~ "Unable to meet demand",
      year == "2026" ~ NA_character_,
      PplSrv_NumWait == 0 | (PplSrv_NumWait_NA_X == 1 & PplSrv_NumServed > 0) ~ "Able to meet demand",
      PplSrv_NumWait > 0 ~ "Unable to meet demand",
      .default = NA_character_
    ),
    dplyr::across(
      .cols = numstaff_vars[grepl("Fulltime", numstaff_vars)],
      .fns = ~ dplyr::case_when(
        Staff_Fulltime_NA == 1 & is.na(.x) ~ "0",
        .x == 0 ~ "0",
        .x == 1 ~ "1",
        .x >= 2 &  .x <= 9 ~ "2–9",
        .x >= 10 & .x <= 49 ~ "10–49",
        .x >= 50 ~ "50+",
        .default = NA_character_
      )
    ),
    dplyr::across(
      .cols = numstaff_vars[grepl("Parttime", numstaff_vars)],
      .fns = ~ dplyr::case_when(
        Staff_Parttime_NA == 1 & is.na(.x) ~ "0",
        .x == 0 ~ "0",
        .x == 1 ~ "1",
        .x >= 2 &  .x <= 9 ~ "2–9",
        .x >= 10 & .x <= 49 ~ "10–49",
        .x >= 50 ~ "50+",
        .default = NA_character_
      )
    ),
    # Y6: bucket the cleaned 2025 counts. The data team already imputed NAs
    # upstream, so no NA-flag fallback is needed.
    Staff_Fulltime_2025 = dplyr::case_when(
      Staff_Fulltime_2025_clean == 0 ~ "0",
      Staff_Fulltime_2025_clean == 1 ~ "1",
      Staff_Fulltime_2025_clean >= 2 & Staff_Fulltime_2025_clean <= 9 ~ "2–9",
      Staff_Fulltime_2025_clean >= 10 & Staff_Fulltime_2025_clean <= 49 ~ "10–49",
      Staff_Fulltime_2025_clean >= 50 ~ "50+",
      .default = NA_character_
    ),
    Staff_Parttime_2025 = dplyr::case_when(
      Staff_Parttime_2025_clean == 0 ~ "0",
      Staff_Parttime_2025_clean == 1 ~ "1",
      Staff_Parttime_2025_clean >= 2 & Staff_Parttime_2025_clean <= 9 ~ "2–9",
      Staff_Parttime_2025_clean >= 10 & Staff_Parttime_2025_clean <= 49 ~ "10–49",
      Staff_Parttime_2025_clean >= 50 ~ "50+",
      .default = NA_character_
    ),
    Dem_BChair_Under35 = dplyr::case_when(
      Dem_BChair_Age %in% c(1, 2) ~ "Board chair is under age 35",
      Dem_BChair_Age %in% c(3, 4, 5, 6, 7, 8, 9, 97) ~ "Board chair is not under age 35",
      .default = NA_character_
    ),
    Dem_CEO_Under35 = dplyr::case_when(
      Dem_CEO_Age %in% c(1, 2) ~ "CEO is under age 35",
      Dem_CEO_Age %in% c(3, 4, 5, 6, 7, 8, 9, 97) ~ "CEO is not under age 35",
      .default = NA_character_
    ),
    BChair_POC = dplyr::case_when(
      year == "2026" & BChairrace_new %in% c(1, 2, 3, 4, 5, 6, 7, 9) ~ "Board chair is a person of color",
      year == "2026" & !is.na(BChairrace_new) ~ "Board chair is not a person of color",
      BChairrace %in% c(1, 2, 3, 4, 6, 7) ~ "Board chair is a person of color",
      BChairrace %in% c(5) ~ "Board chair is not a person of color",
      .default = NA_character_
    ),
    CEOrace_POC = dplyr::case_when(
      year == "2026" & CEOrace_new %in% c(1, 2, 3, 4, 5, 6, 7, 9) ~ "CEO is a person of color",
      year == "2026" & !is.na(CEOrace_new) ~ "CEO is not a person of color",
      CEOrace %in% c(1, 2, 3, 4, 6, 7) ~ "CEO is a person of color",
      CEOrace %in% c(5) ~ "CEO is not a person of color",
      .default = NA_character_
    )
  ) |>
  dplyr::mutate(
    # ProgDem_* collapse: Y4/Y5 treat "1 or 2" as served; Y6 changed the
    # question so only code 1 means served (per data dictionary). Y4/Y5 rows
    # are dropped from these metrics downstream (see metrics_drop_y4_y5).
    dplyr::across(
      .cols = dplyr::starts_with("ProgDem_"),
      .fns = ~ dplyr::case_when(
        year == "2026" & .x == 1 ~ 1L,
        year == "2026" & .x == 0 ~ 0L,
        year == "2026" & .x == 2 ~ 0L,
        year == "2026" ~ NA_integer_,
        .x == 1 | .x == 2 ~ 1L,
        .x == 0 ~ 0L,
        .default = NA_integer_
      )
    ),
    FinanceChng_Reserves = dplyr::case_when(
      FinanceChng_Reserves == 1 ~ "Drew on cash reserves",
      FinanceChng_Reserves %in% c(97, 0) ~ "Did not draw on cash reserves",
      .default = NA_character_
    ),
    GeoAreas_ServedLocal = dplyr::case_when(
      GeoAreas_ServedLocal == 1 ~ "Serving local areas",
      GeoAreas_ServedLocal == 0 ~ "Not serving local areas",
      .default = NA_character_
    ),
    GeoAreas_State = dplyr::case_when(
      GeoAreas_State == 1 ~ "Serving statewide",
      GeoAreas_State == 0 ~ "Not serving statewide",
      .default = NA_character_
    ),
    GeoAreas_Servedmultistate = dplyr::case_when(
      GeoAreas_Servedmultistate == 1 ~ "Serving multiple states",
      GeoAreas_Servedmultistate == 0 ~ "Not serving multiple states",
      .default = NA_character_
    ),
    GeoAreas_National = dplyr::case_when(
      GeoAreas_National == 1 ~ "Serving nationally",
      GeoAreas_National == 0 ~ "Not serving nationally",
      .default = NA_character_
    ),
    GeoAreas_International = dplyr::case_when(
      GeoAreas_International == 1 ~ "Serving internationally",
      GeoAreas_International == 0 ~ "Not serving internationally",
      .default = NA_character_
    ),
    ProgDem_BelowFPL = dplyr::case_when(
      ProgDem_BelowFPL == 1 ~ "Serving people living in poverty",
      ProgDem_BelowFPL == 0 ~ "Not serving people living in poverty",
      .default = NA_character_
    ),
    ProgDem_Disabled = dplyr::case_when(
      ProgDem_Disabled == 1 ~ "Serving people with disabilities",
      ProgDem_Disabled == 0 ~ "Not serving people with disabilities",
      .default = NA_character_
    ),
    ProgDem_Veterans = dplyr::case_when(
      ProgDem_Veterans == 1 ~ "Serving veterans",
      ProgDem_Veterans == 0 ~ "Not serving veterans",
      .default = NA_character_
    ),
    ProgDem_LGBTQ = dplyr::case_when(
      ProgDem_LGBTQ == 1 ~ "Serving LGBTQ people",
      ProgDem_LGBTQ == 0 ~ "Not serving LGBTQ people",
      .default = NA_character_
    ),
    ProgDem_Foreign = dplyr::case_when(
      ProgDem_Foreign == 1 ~ "Serving foreign-born people",
      ProgDem_Foreign == 0 ~ "Not serving foreign-born people",
      .default = NA_character_
    ),
    ProgDem_Latinx = dplyr::case_when(
      ProgDem_Latinx == 1 ~ "Serving Latinx/Hispanic populations",
      ProgDem_Latinx == 0 ~ "Not serving Latinx/Hispanic populations",
      .default = NA_character_
    ),
    ProgDem_Black = dplyr::case_when(
      ProgDem_Black == 1 ~ "Serving Black/African American populations",
      ProgDem_Black == 0 ~ "Not serving Black/African American populations",
      .default = NA_character_
    ),
    ProgDem_Indigenous = dplyr::case_when(
      ProgDem_Indigenous == 1 ~ "Serving American Indian or Alaska Native populations",
      ProgDem_Indigenous == 0 ~ "Not serving American Indian or Alaska Native populations",
      .default = NA_character_
    ),
    ProgDem_Asian = dplyr::case_when(
      ProgDem_Asian == 1 ~ "Serving Asian populations",
      ProgDem_Asian == 0 ~ "Not serving Asian populations",
      .default = NA_character_
    ),
    ProgDem_Men = dplyr::case_when(
      ProgDem_Men == 1 ~ "Serving men and boys",
      ProgDem_Men == 0 ~ "Not serving men and boys",
      .default = NA_character_
    ),
    ProgDem_Women = dplyr::case_when(
      ProgDem_Women == 1 ~ "Serving women and girls",
      ProgDem_Women == 0 ~ "Not serving women and girls",
      .default = NA_character_
    ),
    ProgDem_Nonbinary = dplyr::case_when(
      ProgDem_Nonbinary == 1 ~ "Serving nonbinary people",
      ProgDem_Nonbinary == 0 ~ "Not serving nonbinary people",
      .default = NA_character_
    ),
    ProgDem_Children = dplyr::case_when(
      ProgDem_Children == 1 ~ "Serving children and youth",
      ProgDem_Children == 0 ~ "Not serving children and youth",
      .default = NA_character_
    ),
    ProgDem_YoungAdults = dplyr::case_when(
      ProgDem_YoungAdults == 1 ~ "Serving young adults",
      ProgDem_YoungAdults == 0 ~ "Not serving young adults",
      .default = NA_character_
    ),
    ProgDem_Adults = dplyr::case_when(
      ProgDem_Adults == 1 ~ "Serving adults",
      ProgDem_Adults == 0 ~ "Not serving adults",
      .default = NA_character_
    ),
    ProgDem_Elders = dplyr::case_when(
      ProgDem_Elders == 1 ~ "Serving older adults",
      ProgDem_Elders == 0 ~ "Not serving older adults",
      .default = NA_character_
    ),
    PrgSrvc_Suspend = dplyr::case_when(
      PrgSrvc_Suspend == 1 ~ "Paused or suspended services",
      PrgSrvc_Suspend %in% c(0, 97) ~ "Did not pause or suspend services",
      .default = NA_character_
    ),
    Dmnd_NxtYear  = dplyr::case_when(
      Dmnd_NxtYear  == 2 ~ "Anticipated increase",
      Dmnd_NxtYear == 1 ~ "No change",
      Dmnd_NxtYear == 0 ~ "Anticipated decrease",
      .default = NA_character_
    ),
    # Consult ceo_bchair_binary_ls object in R/config.R
    dplyr::across(
      .cols = names(ceo_bchair_binary_ls),
      .fns = ~ dplyr::case_when(
        .x == 1 ~ ceo_bchair_binary_ls[[dplyr::cur_column()]][[1]], 
        .x %in% c(0, 97) ~ ceo_bchair_binary_ls[[dplyr::cur_column()]][[2]], 
        .default = NA_character_
      )
    ),
    dplyr::across(
      .cols = dplyr::all_of(multi_select_cols),
      .fns = ~ dplyr::case_when(
        .x %in% c(1, 2) ~ "Decrease",
        .x == 3 ~ "No change",
        .x %in% c(4, 5) ~ "Increase",
        .x == 97 ~ "Unsure",
        .default = NA_character_
      )
    ),
    dplyr::across(
      .cols = dplyr::all_of(total_expenditure_cols),
      .fns = ~ dplyr::case_when(
        .x %in% c(1, 2) ~ "Decrease in expenditures",
        .x == 3 ~ "No change",
        .x %in% c(4, 5) ~ "Increase in expenditures",
        .x == 97 ~ "Unsure",
        .default = NA_character_
      )
    ),
    dplyr::across(
      .cols = dplyr::all_of(grant_value_change_cols),
      .fns = ~ dplyr::case_when(
        .x %in% c(1, 2) ~ "Decrease in value",
        .x == 3 ~ "No change",
        .x %in% c(4, 5) ~ "Increase in value",
        .x == 97 ~ "Unsure",
        .default = NA_character_
      )
    ),
    dplyr::across(
      .cols = dplyr::all_of(names(binary_rcv_cols_ls)),
      .fns = ~ dplyr::case_when(.x == 1 ~ binary_rcv_cols_ls[[dplyr::cur_column()]],
                                .x == 0 ~ gsub("Received",
                                               "Did not receive",
                                               binary_rcv_cols_ls[[dplyr::cur_column()]]), 
                                .default = NA_character_)
    ),
    # 7-bucket scheme for PercentDem_* variables that still use the original
    # response options in the new (2026-05) template.
    dplyr::across(
      .cols = dplyr::all_of(setdiff(percentdem_vars, percentdem_4bucket_vars)),
      .fns = ~ dplyr::case_when(
        .x == 0 ~ "0%",
        .x %in% c(1, 2) ~ "1–20%",
        .x %in% c(3, 4) ~ "21–40%",
        .x %in% c(5, 6) ~ "41–60%",
        .x %in% c(7, 8) ~ "61–80%",
        .x %in% c(9, 10) ~ "81–99%",
        .x == 11 ~ "100%",
        .x == 97 ~ NA_character_,
        .default = NA_character_
      )
    ),
    # 4-bucket scheme introduced with the 2026-05 template for the LGBTQ,
    # Disabled, and ReceivedServices Board/Staff demographics. Y4/Y5 raw
    # uses the 0–11 (+97) coding and we collapse here; Y6 raw ships the
    # pre-bucketed 0/1/2/3 (+97) coding so we map straight through.
    dplyr::across(
      .cols = dplyr::all_of(percentdem_4bucket_recoded_y6_vars),
      .fns = ~ dplyr::case_when(
        year == "2026" & .x == 0 ~ "0%",
        year == "2026" & .x == 1 ~ "1–50%",
        year == "2026" & .x == 2 ~ "51–99%",
        year == "2026" & .x == 3 ~ "100%",
        year == "2026" ~ NA_character_,
        .x == 0 ~ "0%",
        .x %in% c(1, 2, 3, 4, 5) ~ "1–50%",
        .x %in% c(6, 7, 8, 9, 10) ~ "51–99%",
        .x == 11 ~ "100%",
        .x == 97 ~ NA_character_,
        .default = NA_character_
      )
    ),
    # Same 4-bucket target as above, but the Y6 raw still uses the 0–11
    # (+97) coding for PercentDem_Young_Board/Staff — confirmed by the
    # 2026 survey response options being unchanged for these two items.
    # Apply the Y4/Y5 collapse uniformly across all years.
    dplyr::across(
      .cols = dplyr::all_of(percentdem_4bucket_legacy_y6_vars),
      .fns = ~ dplyr::case_when(
        .x == 0 ~ "0%",
        .x %in% c(1, 2, 3, 4, 5) ~ "1–50%",
        .x %in% c(6, 7, 8, 9, 10) ~ "51–99%",
        .x == 11 ~ "100%",
        .x == 97 ~ NA_character_,
        .default = NA_character_
      )
    ),
    # Year 6 introduces per-metric weight selection. We materialize two
    # general weights and two state weights here so the summarize step can
    # pick the right one based on the metric being computed:
    #   svywt           — default Y6 weight is weight_year6plus
    #   svywt_alt       — alternate Y6 weight is weight_year6 (for FndRaise_*
    #                      Rcv / Chng / TotExp / Cash*250_Chng metrics)
    #   stateweight     — default state-level Y6 weight is stateweightplus
    #   stateweight_alt — alternate state-level Y6 weight is the raw
    #                      stateweight column (for the same FndRaise_*
    #                      metrics).
    # Y4/Y5 keep their original single weight pair, so svywt == svywt_alt
    # and stateweight == stateweight_alt for those rows.
    svywt = dplyr::case_when(
      year == "2026" ~ weight_year6plus,
      .default = dplyr::coalesce(year4wt, year5wt)
    ),
    svywt_alt = dplyr::case_when(
      year == "2026" ~ weight_year6,
      .default = dplyr::coalesce(year4wt, year5wt)
    ),
    stateweight_alt = stateweight,
    stateweight = dplyr::case_when(
      year == "2026" ~ stateweightplus,
      .default = stateweight
    )
  ) |>
  dplyr::mutate(
    Staff_RegVlntr = dplyr::coalesce(Staff_RegVlntr_2023,
                                     Staff_RegVlntr_2024,
                                     Staff_RegVlntr_2025_clean),
    Staff_EpsdVlntr = dplyr::coalesce(Staff_EpsdVlntr_2023,
                                      Staff_EpsdVlntr_2024,
                                      Staff_EpsdVltnr_2025_clean),
    Staff_Boardmmbr = dplyr::coalesce(Staff_Boardmmbr_2023,
                                      Staff_Boardmmbr_2024,
                                      Staff_Boardmmbr_2025_clean),
    Staff_Fulltime = dplyr::coalesce(Staff_Fulltime_2023,
                                     Staff_Fulltime_2024,
                                     Staff_Fulltime_2025),
    Staff_Parttime = dplyr::coalesce(Staff_Parttime_2023,
                                     Staff_Parttime_2024,
                                     Staff_Parttime_2025)
  ) |>
  dplyr::mutate(
    Cash_Reserves = dplyr::case_when(
      is.na(Reserves_Est) & Reserves_NA_X == 1 ~ "Did not have cash reserves",
      Reserves_Est > 0 ~ "Had cash reserves",
      Reserves_Est <= 0 ~ "Did not have cash reserves",
      .default = NA_character_
    ),
    FndRaise_Cashbelow250_Chng = dplyr::case_when(
      FndRaise_Cashbelow250_Chng %in% c(1, 2) ~ "Decrease in donations < $250",
      FndRaise_Cashbelow250_Chng == 3 ~ "No change",
      FndRaise_Cashbelow250_Chng %in% c(4, 5) ~ "Increase in donations < $250",
      year == 2024 & FndRaise_Cashbelow250_Chng == 98 ~ "Unsure",
      year == 2025 & FndRaise_Cashbelow250_Chng == 97 ~ "Unsure",
      year == "2026" & FndRaise_Cashbelow250_Chng == 97 ~ "Unsure",
      .default = NA_character_
    ),
    FndRaise_Cashabove250_Chng = dplyr::case_when(
      FndRaise_Cashabove250_Chng %in% c(1, 2) ~ "Decrease in donations ≥ $250",
      FndRaise_Cashabove250_Chng == 3 ~ "No change",
      FndRaise_Cashabove250_Chng %in% c(4, 5) ~ "Increase in donations ≥ $250",
      year == 2024 & FndRaise_Cashabove250_Chng == 98 ~ "Unsure",
      year == 2025 & FndRaise_Cashabove250_Chng == 97 ~ "Unsure",
      year == "2026" & FndRaise_Cashabove250_Chng == 97 ~ "Unsure",
      .default = NA_character_
    ),
    FinanceChng_TotExp = dplyr::case_when(
      FinanceChng_TotExp %in% c(1, 2) ~ "Decrease in expenses",
      FinanceChng_TotExp == 3 ~ "No change",
      FinanceChng_TotExp %in% c(4, 5) ~ "Increase in expenses",
      FinanceChng_TotExp == 97 ~ "Unsure",
      .default = NA_character_
    ),
    FinanceChng_Salaries = dplyr::case_when(
      FinanceChng_Salaries %in% c(1, 2) ~ "Decrease in salaries and wages",
      FinanceChng_Salaries == 3 ~ "No change",
      FinanceChng_Salaries %in% c(4, 5) ~ "Increase in salaries and wages",
      FinanceChng_Salaries == 97 ~ "Unsure",
      .default = NA_character_
    ),
    PrgSrvc_Amt_Fee = dplyr::case_when(
      PrgSrvc_Amt_Fee %in% c(1, 2) ~ "Decrease in $ amount",
      PrgSrvc_Amt_Fee == 3 ~ "No change",
      PrgSrvc_Amt_Fee %in% c(4, 5) ~ "Increase in $ amount",
      PrgSrvc_Amt_Fee == 97 ~ "Unsure",
      .default = NA_character_
    )
  )

# Save intermediate file
data.table::fwrite(nptrends_full_preprocessed,
                   "data/intermediate/nptrends_full_preprocessed.csv")
