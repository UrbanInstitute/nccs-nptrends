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
        "< $100,000",
        "$100,000 - $499,999",
        "$500,000 - $999,999",
        "$1 million - $9,999,999",
        "$10 million and above"
      )
    ),
    Subsector = dplyr::case_when(
      ntmaj12 == "AR" ~ "Arts, culture, and humanities",
      ntmaj12 == "ED" ~ "Education",
      ntmaj12 == "EN" ~ "Environment and animals",
      ntmaj12 == "HE" ~ "Health",
      ntmaj12 == "HU" ~ "Human services",
      ntmaj12 == "IN" ~ "International, foreign affairs",
      ntmaj12 == "PU" ~ "Public, societal benefit",
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
    Reserves_Est = dplyr::case_when(
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
      PplSrv_NumWait == 0 | (PplSrv_NumWait_NA_X == 1 & PplSrv_NumServed > 0) ~ "Did not meet demand",
      PplSrv_NumWait > 0 ~ "Met demand",
      .default = NA_character_
    ),
    dplyr::across(
      .cols = numstaff_vars[grepl("Fulltime", numstaff_vars)],
      .fns = ~ dplyr::case_when(
        Staff_Fulltime_NA == 1 & is.na(.x) ~ "0",
        .x == 0 ~ "0",
        .x == 1 ~ "1",
        .x >= 2 &  .x <= 9 ~ "2-9",
        .x >= 10 & .x <= 49 ~ "10-49",
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
        .x >= 2 &  .x <= 9 ~ "2-9",
        .x >= 10 & .x <= 49 ~ "10-49",
        .x >= 50 ~ "50+",
        .default = NA_character_
      )
    ),
    Dem_BChair_Under35 = dplyr::case_when(
      Dem_BChair_Age %in% c(1, 2) ~ "Under 35 years old",
      Dem_BChair_Age %in% c(3, 4, 5, 6, 7, 8, 9, 97) ~ "Not under 35 years old",
      .default = NA_character_
    ),
    Dem_CEO_Under35 = dplyr::case_when(
      Dem_CEO_Age %in% c(1, 2) ~ "Under 35 years old",
      Dem_CEO_Age %in% c(3, 4, 5, 6, 7, 8, 9, 97) ~ "Not under 35 years old",
      .default = NA_character_
    ),
    BChair_POC = dplyr::case_when(
      BChairrace %in% c(1, 2, 3, 4, 6, 7) ~ "Person of color",
      BChairrace %in% c(5) ~ "Not person of color",
      .default = NA_character_
    ),
    CEOrace_POC = dplyr::case_when(
      CEOrace %in% c(1, 2, 3, 4, 6, 7) ~ "Person of color",
      CEOrace %in% c(5) ~ "Not person of color",
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
      GeoAreas_ServedLocal == 1 ~ "Served local",
      GeoAreas_ServedLocal == 0 ~ "Did not serve local",
      .default = NA_character_
    ),
    GeoAreas_State = dplyr::case_when(
      GeoAreas_State == 1 ~ "Served state-wide",
      GeoAreas_State == 0 ~ "Did not serve state-wide",
      .default = NA_character_
    ),
    GeoAreas_Servedmultistate = dplyr::case_when(
      GeoAreas_Servedmultistate == 1 ~ "Served multi state",
      GeoAreas_Servedmultistate == 0 ~ "Did not serve multi state",
      .default = NA_character_
    ),
    GeoAreas_National = dplyr::case_when(
      GeoAreas_National == 1 ~ "Served national",
      GeoAreas_National == 0 ~ "Did not serve national",
      .default = NA_character_
    ),
    GeoAreas_International = dplyr::case_when(
      GeoAreas_International == 1 ~ "Served international",
      GeoAreas_International == 0 ~ "Did not serve international",
      .default = NA_character_
    ),
    ProgDem_BelowFPL = dplyr::case_when(
      ProgDem_BelowFPL == 1 ~ "Served families and individuals in poverty",
      ProgDem_BelowFPL == 0 ~ "Did not serve families and individuals in poverty",
      .default = NA_character_
    ),
    ProgDem_Disabled = dplyr::case_when(
      ProgDem_Disabled == 1 ~ "Served individuals with physical or cognitive disabilities",
      ProgDem_Disabled == 0 ~ "Did not serve individuals with physical or cognitive disabilities",
      .default = NA_character_
    ),
    ProgDem_Veterans = dplyr::case_when(
      ProgDem_Veterans == 1 ~ "Served veterans",
      ProgDem_Veterans == 0 ~ "Did not serve veterans",
      .default = NA_character_
    ),
    ProgDem_LGBTQ = dplyr::case_when(
      ProgDem_LGBTQ == 1 ~ "Served LGBTQ+",
      ProgDem_LGBTQ == 0 ~ "Did not serve LGBTQ+",
      .default = NA_character_
    ),
    ProgDem_Foreign = dplyr::case_when(
      ProgDem_Foreign == 1 ~ "Served foreign born population",
      ProgDem_Foreign == 0 ~ "Did not serve foreign born populations",
      .default = NA_character_
    ),
    ProgDem_Latinx = dplyr::case_when(
      ProgDem_Latinx == 1 ~ "Served Latinx/Hispanic/Hispanic Origin",
      ProgDem_Latinx == 0 ~ "Did not serve Latinx/Hispanic/Hispanic Origin",
      .default = NA_character_
    ),
    ProgDem_Black = dplyr::case_when(
      ProgDem_Black == 1 ~ "Served Black/African American",
      ProgDem_Black == 0 ~ "Did not serve Black/African American",
      .default = NA_character_
    ),
    ProgDem_Indigenous = dplyr::case_when(
      ProgDem_Indigenous == 1 ~ "Served indigenous/Native American/Native Alaskan",
      ProgDem_Indigenous == 0 ~ "Did not serve indigenous/Native American/Native Alaskan",
      .default = NA_character_
    ),
    ProgDem_Asian = dplyr::case_when(
      ProgDem_Asian == 1 ~ "Served Asian/Native Hawaiian/other Pacific Islander",
      ProgDem_Asian == 0 ~ "Did not serve Asian/Native Hawaiian/other Pacific Islander",
      .default = NA_character_
    ),
    ProgDem_Men = dplyr::case_when(
      ProgDem_Men == 1 ~ "Served men/boys",
      ProgDem_Men == 0 ~ "Did not serve men/boys",
      .default = NA_character_
    ),
    ProgDem_Women = dplyr::case_when(
      ProgDem_Women == 1 ~ "Served women/girls",
      ProgDem_Women == 0 ~ "Did not serve women/girls",
      .default = NA_character_
    ),
    ProgDem_Nonbinary = dplyr::case_when(
      ProgDem_Nonbinary == 1 ~ "Served individuals of non-binary gender",
      ProgDem_Nonbinary == 0 ~ "Did not serve individuals of non-binary gender",
      .default = NA_character_
    ),
    ProgDem_Children = dplyr::case_when(
      ProgDem_Children == 1 ~ "Served children and youth",
      ProgDem_Children == 0 ~ "Did not serve children and youth",
      .default = NA_character_
    ),
    ProgDem_YoungAdults = dplyr::case_when(
      ProgDem_YoungAdults == 1 ~ "Served young adults",
      ProgDem_YoungAdults == 0 ~ "Did not serve young adults",
      .default = NA_character_
    ),
    ProgDem_Adults = dplyr::case_when(
      ProgDem_Adults == 1 ~ "Served adults",
      ProgDem_Adults == 0 ~ "Did not serve adults",
      .default = NA_character_
    ),
    ProgDem_Elders = dplyr::case_when(
      ProgDem_Elders == 1 ~ "Served seniors",
      ProgDem_Elders == 0 ~ "Did not serve seniors",
      .default = NA_character_
    ),
    PrgSrvc_Suspend = dplyr::case_when(
      PrgSrvc_Suspend == 1 ~ "Experiencing",
      PrgSrvc_Suspend %in% c(0, 97) ~ "Not experiencing",
      .default = NA_character_
    ),
    Dmnd_NxtYear  = dplyr::case_when(
      Dmnd_NxtYear  == 2 ~ "Anticipated increase",
      Dmnd_NxtYear == 1 ~ "No change",
      Dmnd_NxtYear == 0 ~ "Anticipated decrease",
      .default = NA_character_
    ),
    dplyr::across(
      .cols = c("CEOgender_Man", "BChairgender_Man"),
      .fns = ~ dplyr::case_when(.x == 1 ~ "Man", 
                                .x == 0 ~ "Not a man", 
                                .default = NA_character_)
    ),
    dplyr::across(
      .cols = c("CEOgender_Woman", "BChairgender_Woman"),
      .fns = ~ dplyr::case_when(.x == 1 ~ "Woman", 
                                .x == 0 ~ "Not a woman", 
                                .default = NA_character_)
    ),
    dplyr::across(
      .cols = c("Dem_CEO_LGBTQ", "Dem_BChair_LGBTQ"),
      .fns = ~ dplyr::case_when(.x == 1 ~ "LGBTQ+", 
                                .x %in% c(0, 97) ~ "Not LGBTQ+", 
                                .default = NA_character_)
    ),
    dplyr::across(
      .cols = c("CEOgender_NB", "BChairgender_NB"),
      .fns = ~ dplyr::case_when(
        .x == 1 ~ "Individual of non-binary gender",
        .x == 0 ~ "Not individual of non-binary gender",
        .default = NA_character_
      )
    ),
    dplyr::across(
      .cols = c("Dem_CEO_Disabled", "Dem_BChair_Disabled"),
      .fns = ~ dplyr::case_when(.x == 1 ~ "Disabled", 
                                .x %in% c(0, 97) ~ "Not disabled", 
                                .default = NA_character_)
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
      .cols = dplyr::all_of(binary_rcv_cols),
      .fns = ~ dplyr::case_when(.x == 1 ~ "Received",
                                .x == 0 ~ "Did not receive", 
                                .default = NA_character_)
    ),
    dplyr::across(
      .cols = dplyr::all_of(percentdem_vars),
      .fns = ~ dplyr::case_when(
        .x == 0 ~ "0%",
        .x %in% c(1, 2) ~ "1-20%",
        .x %in% c(3, 4) ~ "21-40%",
        .x %in% c(5, 6) ~ "41-60%",
        .x %in% c(7, 8) ~ "61-80%",
        .x %in% c(9, 10) ~ "81-99%",
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
  )

# Save intermediate file
data.table::fwrite(nptrends_full_preprocessed,
                   "data/intermediate/nptrends_full_preprocessed.csv")
