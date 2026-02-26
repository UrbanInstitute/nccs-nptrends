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

# Load in raw survey data
nptrends_y4_raw <- data.table::fread(nptrends_y4_raw_path, 
                                     select = survey_analysis_vars) |>
  dplyr::mutate(year = "2024") 
nptrends_y5_raw <- data.table::fread(nptrends_y5_raw_path,
                                     select = survey_analysis_vars) |>
  dplyr::mutate(year = "2025")
nptrends_full_raw <- nptrends_y4_raw |>
  dplyr::bind_rows(nptrends_y5_raw)

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
  # Binary flags for new variables where 1 if any of the variables are 1 and 0 if all are 0
  dplyr::mutate(
    percent_expenses_inCashReserves = dplyr::case_when(
      is.na(Reserves_Est) & Reserves_NA_X == 1 ~ 0,
      is.na(TotExp) | TotExp == 0 ~ NA_real_,
      .default = Reserves_Est / TotExp
    ),
    GeoAreas_ServedLocal = binary_flag(
      data = .,
      GeoAreas_Local,
      GeoAreas_MultipleLocal,
      GeoAreas_RegionalWithin
    ),
    GeoAreas_Servedmultistate = binary_flag(data = ., 
                                            GeoAreas_MultipleState, 
                                            GeoAreas_RegionalAcross),
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
      BChairrace %in% c(1, 2, 3, 4, 6, 7) ~ "Board chair is a person of color",
      BChairrace %in% c(5) ~ "Board chair is not a person of color",
      .default = NA_character_
    ),
    CEOrace_POC = dplyr::case_when(
      CEOrace %in% c(1, 2, 3, 4, 6, 7) ~ "CEO is a person of color",
      CEOrace %in% c(5) ~ "CEO is not a person of color",
      .default = NA_character_
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::starts_with("ProgDem_"),
      .fns = ~ dplyr::case_when(.x == 1 | .x == 2 ~ 1, 
                                .x == 0 ~ 0, 
                                .default = NA_integer_)
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
      ProgDem_Indigenous == 1 ~ "Serving Indigenous/Native American and Alaska Native populations",
      ProgDem_Indigenous == 0 ~ "Not serving Indigenous/Native American and Alaska Native populations",
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
      ProgDem_Elders == 1 ~ "Serving seniors",
      ProgDem_Elders == 0 ~ "Not serving seniors",
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
    dplyr::across(
      .cols = dplyr::all_of(percentdem_vars),
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
    svywt = dplyr::coalesce(year4wt, year5wt)
  ) |>
  dplyr::mutate(
    Staff_RegVlntr = dplyr::coalesce(Staff_RegVlntr_2023, Staff_RegVlntr_2024),
    Staff_EpsdVlntr = dplyr::coalesce(Staff_EpsdVlntr_2023, Staff_EpsdVltnr_2024),
    Staff_Boardmmbr = dplyr::coalesce(Staff_Boardmmbr_2023, Staff_Boardmmbr_2024),
    Staff_Fulltime = dplyr::coalesce(Staff_Fulltime_2023, Staff_Fulltime_2024),
    Staff_Parttime = dplyr::coalesce(Staff_Parttime_2023, Staff_Parttime_2024)
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
      .default = NA_character_
    ),
    FndRaise_Cashabove250_Chng = dplyr::case_when(
      FndRaise_Cashabove250_Chng %in% c(1, 2) ~ "Decrease in donations ≥ $250",
      FndRaise_Cashabove250_Chng == 3 ~ "No change",
      FndRaise_Cashabove250_Chng %in% c(4, 5) ~ "Increase in donations ≥ $250",
      year == 2024 & FndRaise_Cashabove250_Chng == 98 ~ "Unsure",
      year == 2025 & FndRaise_Cashabove250_Chng == 97 ~ "Unsure",
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
