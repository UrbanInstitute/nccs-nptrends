# Libraries
library(data.table)
library(tidyverse)
library(spatstat)
library(rlang)
library(stats)

# Helper functions
source(here::here("R", "summarize_survey_results.R"))

# (1) - Extract Data
nptrends_y4_raw <- data.table::fread("Y:/CNP/Generosity Commission/Year 4/Restricted Dataset/RESTRICTED_Y4.csv")
metrics_metadata <- readxl::read_xlsx(
  "Y:/CNP/Generosity Commission/Year 4/metricsMaster_HMnotes.xlsx"
)

# (1.1) - Preprocess data


nptrends_y4 <- nptrends_y4_raw |>
  dplyr::filter(
    ! state %in% c(
      "AK",
      "CT",
      "DC",
      "DE",
      "HI",
      "IA",
      "KS",
      "ME",
      "ND",
      "NE",
      "NH",
      "NJ",
      "RI",
      "SC",
      "SD",
      "VT",
      "WV",
      "WY"
    )
  ) |>
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
      ntmaj12 == "AR" ~ "Arts, Culture, and Humanities",
      ntmaj12 == "ED" ~ "Education",
      ntmaj12 == "EN" ~ "Environment and Animals",
      ntmaj12 == "HE" ~ "Health",
      ntmaj12 == "HU" ~ "Human Services",
      ntmaj12 == "IN" ~ "International, foreign affairs",
      ntmaj12 == "PU" ~ "Public, societal benefit",
      ntmaj12 == "RE" ~ "Religion",
      .default = NA
    ),
    census_urban_area = dplyr::case_when(
      census_urban_area == 1 ~ "Urban",
      census_urban_area == 0 ~ "Rural",
    ),
    CensusRegion4 = factor(
      CensusRegion4,
      levels = c(1, 2, 3, 4),
      labels = c("Northeast", "Midwest", "South", "West")
    ),
    National = "National") %>% 
  # Binary flags for new variables where 1 if any of the variables are 1 and 0 if all are 0
  dplyr::mutate(
    GeoAreas_ServedLocal = binary_flag(
      data = .,
      GeoAreas_Local, 
      GeoAreas_MultipleLocal, 
      GeoAreas_RegionalWithin
    ),
    GeoAreas_Servedmultistate = binary_flag(data = .,
                                            GeoAreas_MultipleState, 
                                            GeoAreas_RegionalAcross
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
      .default = Staff_EpsdVlntr_2023
    ),
    Staff_Boardmmbr_2023 = dplyr::case_when(
      is.na(Staff_Boardmmbr_2023) & 
        Staff_Boardmmbr_NA == 1 ~ 0,
      .default = Staff_Boardmmbr_2023
    ),
    PplSrv_NumWait = dplyr::case_when(
      PplSrv_NumServed_NA_X == 1 ~ NA_character_,
      PplSrv_NumWait == 0 ~ "Met demand",
      is.na(PplSrv_NumWait) ~ NA_character_,
      .default = "Did not meet demand"
    ),
    Staff_Boardmmbr_2023 = dplyr::case_when(
      is.na(Staff_Boardmmbr_2023) & 
        Staff_Boardmmbr_NA == 1 ~ 0,
      .default = Staff_Boardmmbr_2023
    ),
    Staff_Fulltime_2023 = dplyr::case_when(
      Staff_Fulltime_2023 == 0 ~ "0",
      Staff_Fulltime_2023 == 1 ~ "1",
      Staff_Fulltime_2023 >= 2 &  Staff_Fulltime_2023 <= 9 ~ "2-9",
      Staff_Fulltime_2023 >= 10 & Staff_Fulltime_2023 <= 49 ~ "10-49",
      Staff_Fulltime_2023 >= 50 ~ "50+",
      Staff_Fulltime_NA == 1 & is.na(Staff_Fulltime_2023) ~ "0",
      .default = NA_character_
    ),
    Staff_Parttime_2023 = dplyr::case_when(
      Staff_Parttime_2023 == 0 ~ "0",
      Staff_Parttime_2023 == 1 ~ "1",
      Staff_Parttime_2023 >= 2 &  Staff_Parttime_2023 <= 9 ~ "2-9",
      Staff_Parttime_2023 >= 10 & Staff_Parttime_2023 <= 49 ~ "10-49",
      Staff_Parttime_2023 >= 50 ~ "50+",
      Staff_Parttime_NA == 1 & is.na(Staff_Parttime_2023) ~ "0",
      .default = NA_character_
    ),
    Dem_BChair_Under35 = dplyr::case_when(
      Dem_BChair_Age %in% c(1, 2) ~ "Under 35 years old",
      Dem_BChair_Age %in% c(3, 4, 6, 7, 8, 9) ~ "Not under 35 years old",
      .default = NA_character_
    ),
    Dem_CEO_Under35 = dplyr::case_when(
      Dem_CEO_Age %in% c(1, 2) ~ "Under 35 years old",
      Dem_CEO_Age %in% c(3, 4, 6, 7, 8, 9) ~ "Not under 35 years old",
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
      .fns = ~ dplyr::case_when(
        .x == 1 | .x == 2 ~ 1,
        .x == 0 ~ 0,
        .default = NA_integer_
      )
    ),
    FinanceChng_Reserves = dplyr::case_when(
      FinanceChng_Reserves == 1 ~ "Drew on cash reserves",
      FinanceChng_Reserves == 0 ~ "Did not draw on cash reserves",
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
    GeoAreas_MultipleState = dplyr::case_when(
      GeoAreas_MultipleState == 1 ~ "Served multi state",
      GeoAreas_MultipleState == 0 ~ "Did not serve multi state",
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
      PrgSrvc_Suspend == 0 ~ "Not Experiencing",
      .default = NA_character_
    ),
    Dmnd_NxtYear  = dplyr::case_when(
      Dmnd_NxtYear  == 2 ~ "Anticipated increase",
      Dmnd_NxtYear == 1 ~ "No change",
      Dmnd_NxtYear == 0 ~ "Anticipated decrease",
      .default = NA_character_
    ),
    Dem_CEO_LGBTQ = dplyr::case_when(
      Dem_CEO_LGBTQ == 1 ~ "LGBTQ+",
      Dem_CEO_LGBTQ == 0 ~ "Not LGBTQ+",
      .default = NA_character_
    ),
    Dem_CEO_Disabled = dplyr::case_when(
      Dem_CEO_Disabled == 1 ~ "Disabled",
      Dem_CEO_Disabled == 0 ~ "Not disabled",
      .default = NA_character_
    ),
    dplyr::across(
      .cols = c("CEOgender_Man", "BChairgender_Man"),
      .fns = ~ dplyr::case_when(
        .x == 1 ~ "Man",
        .x == 0 ~ "Not a man",
        .default = NA_character_
      )
    ),
    dplyr::across(
      .cols = c("CEOgender_Woman", "BChairgender_Woman"),
      .fns = ~ dplyr::case_when(
        .x == 1 ~ "Woman",
        .x == 0 ~ "Not a woman",
        .default = NA_character_
      )
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
      .cols = dplyr::all_of(multi_select_cols),
      .fns = ~ dplyr::case_when(
        .x == 1 ~ "Significant decrease",
        .x == 2 ~ "Decrease",
        .x == 3 ~ "No change",
        .x == 4 ~ "Increase",
        .x == 5 ~ "Significant increase",
        .default = NA_character_
      )
    ),
    dplyr::across(
      .cols = dplyr::all_of(binary_rcv_cols),
      .fns = ~ dplyr::case_when(
        .x == 1 ~ "Received",
        .x == 0 ~ "Did not receive",
        .default = NA_character_
      )
    )
  )

# (2) - Transform data

# (2.1) - Define parameters

# Metrics
# Full list of metrics, with 3 associated methods:
# median = weighted median
# average of % = weighted average
# % of respondents = weighted proportion of respondents providing specific answer
metrics <- list(
  TotRev_clean = "median",
  FinanceChng_Reserves = "% of respondents",
  FinanceChng_TotExp = "% of respondents",
  FinanceChng_Salaries = "% of respondents",
  FinanceChng_Benefits = "% of respondents",
  FinanceChng_TotRent = "% of respondents",
  FinanceChng_TotTech = "% of respondents",
  GeoAreas_ServedLocal = "% of respondents",
  GeoAreas_State = "% of respondents",
  GeoAreas_Servedmultistate = "% of respondents",
  GeoAreas_National = "% of respondents",
  GeoAreas_International = "% of respondents",
  ProgDem_BelowFPL = "% of respondents",
  ProgDem_Disabled = "% of respondents",
  ProgDem_Veterans = "% of respondents",
  ProgDem_LGBTQ = "% of respondents",
  ProgDem_Foreign = "% of respondents",
  ProgDem_Latinx = "% of respondents",
  ProgDem_Black = "% of respondents",
  ProgDem_Indigenous = "% of respondents",
  ProgDem_Asian = "% of respondents",
  ProgDem_Men = "% of respondents",
  ProgDem_Women = "% of respondents",
  ProgDem_Nonbinary = "% of respondents",
  ProgDem_Children = "% of respondents",
  ProgDem_YoungAdults = "% of respondents",
  ProgDem_Adults = "% of respondents",
  ProgDem_Elders = "% of respondents",
  PplSrv_NumWait = "% of respondents",
  PrgSrvc_Suspend = "% of respondents",
  PrgSrvc_Amt_Srvc = "% of respondents",
  PrgSrvc_Amt_Num = "% of respondents",
  Dmnd_NxtYear = "% of respondents",
  FndRaise_DnrBlw250 = "average of %",
  FndRaise_DnrAbv250 = "average of %",
  FndRaise_DAF_Rcv = "% of respondents",
  FndRaise_PFGrnt_Rcv = "% of respondents",
  FndRaise_Corp_Found_Grnt_Rcv = "% of respondents",
  FndRaise_CFGrnt_Rcv = "% of respondents",
  CARES_Rcv = "% of respondents",
  PrgSrvc_Amt_Fee = "% of respondents",
  FndRaise_TotExp = "% of respondents",
  FndRaise_TotDigAppeal = "% of respondents",
  FndRaise_TotDirMail = "% of respondents",
  FndRaise_TotVirtEvent = "% of respondents",
  FndRaise_TotInPersEvent = "% of respondents",
  PercentDem_Women_Staff = "average of %",
  Staff_RegVlntr_2023 = "median",
  Staff_EpsdVltnr_2023 = "median",
  PercentDem_Women_Board = "average of %",
  PercentDem_LGBTQ_Board = "average of %",
  PercentDem_Disabled_Board = "average of %",
  PercentDem_Young_Board = "average of %",
  PercentDem_ReceivedServices_Board = "average of %",
  PercentDem_POC_Staff = "average of %",
  PercentDem_LGBTQ_Staff = "average of %",
  PercentDem_Disabled_Staff = "average of %",
  PercentDem_Young_Staff = "average of %",
  PercentDem_ReceivedServices_Staff = "average of %",
  Staff_Boardmmbr_2023 = "median",
  PercentDem_POC_Board = "average of %",
  CEOrace_POC = "% of respondents",
  Dem_CEO_LGBTQ = "% of respondents",
  Dem_CEO_Disabled = "% of respondents",
  Dem_CEO_Under35 = "% of respondents",
  BChair_POC = "% of respondents",
  Dem_BChair_LGBTQ = "% of respondents",
  Dem_BChair_Disabled = "% of respondents",
  Dem_BChair_Under35 = "% of respondents",
  Staff_Fulltime_2023 = "% of respondents",
  Staff_Parttime_2023 = "% of respondents",
  CEOgender_Man = "% of respondents",
  CEOgender_Woman = "% of respondents",
  CEOgender_NB = "% of respondents",
  BChairgender_Man = "% of respondents",
  BChairgender_Woman = "% of respondents",
  BChairgender_NB = "% of respondents"
)

# All possible nested aggregations
groupby_ls <- list(
  "National" = c(
    "National",
    "CensusRegion4", 
    "state", 
    "SizeStrata", 
    "Subsector",
    "census_urban_area"
  ), 
  "CensusRegion4" = c("CensusRegion4", 
                      "state", 
                      "SizeStrata",  
                      "Subsector",
                      "census_urban_area"), 
  "state" = c("state", 
              "SizeStrata", 
              "Subsector",
              "census_urban_area") 
)

# (2.2) - Perform transformations for each metric. Single function call
survey_metrics <- purrr::imap(
  .x = metrics,
  .f = function(method, metric){
    rs <- purrr::imap(
      .x = groupby_ls,
      .f = svy_tfm,
      metric = metric,
      method = method,
      df = nptrends_y4,
      .progress = TRUE
    ) |>
      purrr::list_rbind()
    return(rs)
  },
  .progress = TRUE
) |> purrr::list_rbind()

# (3) - Load

# (3.1) - Postprocess Data
survey_processed <- survey_metrics |>
  dplyr::mutate(
    splitByOpt = dplyr::case_when(
      filterType == "National" ~ "National",
      filterType == "CensusRegion4" ~ "Region",
      filterType == "state" ~ "State",
      filterType == "SizeStrata" ~ "Size",
      filterType == "Subsector" ~ "Sector",
      .default = NA_character_
    ),
    splitByOpt_category = ifelse(is.na(splitByOpt_category), "", splitByOpt_category),
    filterType = dplyr::case_when(
      filterType == "National" ~ "National",
      filterType == "CensusRegion4" ~ "Region",
      filterType == "state" ~ "State",
      .default = NA_character_
    ),
    filterOpt = ifelse(is.na(filterOpt), "", filterOpt)
  )

# (3.2) - Merge with metric metadata

metrics_metadata <- data.table::fread(
  "data/nptrends_y4_metrics_metadata.csv"
)

survey_formatted <- survey_processed |>
  dplyr::mutate(
    value = format(value, digits = 2, scientific = TRUE),
    splitByOpt = dplyr::case_when(
      splitByOpt_category %in% c(
        "< $100,000", 
        "$100,000 - $499,999", 
        "$500,000 - $999,999", 
        "$1 million - $9,999,999", 
        "$10 million and above"
      ) ~ "Size",
      splitByOpt_category %in% c(
        "Arts, Culture, and Humanities", 
        "Education", 
        "Environment and Animals", 
        "Health", 
        "Human Services", 
        "International, foreign affairs", 
        "Public, societal benefit"
      ) ~ "Sector",
      splitByOpt_category %in% c("Rural", "Urban") ~ "Rural/Urban",
      .default = splitByOpt
    )
  ) |>
  tidylog::left_join(metrics_metadata, by = "metricname") |>
  dplyr::rename(responseOpt_value = responseOpt) |>
  dplyr::mutate(year = "2024") |>
  dplyr::mutate(
    responseOpt_value = dplyr::case_when(
      responseOpt_value == "TotRev_clean" ~ "Total Revenue",
      responseOpt_value == "FndRaise_DnrBlw250_ratio" ~ "< $250",
      responseOpt_value == "FndRaise_DnrAbv250_ratio" ~ ">= $250",
      responseOpt_value == "PercentDem_Women_Staff" ~ "Women",
      responseOpt_value == "Staff_RegVlntr_2023" ~ "Regular volunteers",
      responseOpt_value == "Staff_EpsdVlntr_2023" ~ "Episodic volunteers",
      responseOpt_value == "PercentDem_Women_Board" ~ "Women",
      responseOpt_value == "PercentDem_LGBTQ_Board" ~ "LGBTQ+",
      responseOpt_value == "PercentDem_Disabled_Board" ~ "Disabled",
      responseOpt_value == "PercentDem_Young_Board" ~ "Under 35 years old",
      responseOpt_value == "PercentDem_ReceivedServices_Board" ~ "Past or current recipient of nonprofits’ services",
      responseOpt_value == "PercentDem_POC_Staff" ~ "Person of color",
      responseOpt_value == "PercentDem_LGBTQ_Staff" ~ "LGBTQ+",
      responseOpt_value == "PercentDem_Disabled_Staff" ~ "Disabled",
      responseOpt_value == "PercentDem_Young_Staff" ~ "Under 35 years old",
      responseOpt_value == "PercentDem_ReceivedServices_Staff" ~ "Past or current recipient of nonprofits’ services",
      responseOpt_value == "Staff_Boardmmbr_2023" ~ "Number of board members",
      responseOpt_value == "PercentDem_POC_Board" ~ "Person of color",
      .default = responseOpt_value
    )
  )

# (3.3) - Save outputs

data.table::fwrite(
  survey_formatted,
  "data/processed/nptrends_y4_formatted.csv"
)