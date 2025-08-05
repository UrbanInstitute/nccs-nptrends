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

# Variables to recode with 1 if any are 1, otherwise 0 if all are 0


nptrends_y4 <- nptrends_y4_raw |>
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
    FndRaise_DnrBlw250_ratio = FndRaise_DnrBlw250 / FndRaise_DnrAbv250 + FndRaise_DnrBlw250,
    FndRaise_DnrAbv250_ratio = FndRaise_DnrAbv250 / FndRaise_DnrBlw250 + FndRaise_DnrAbv250,
    Staff_RegVlntr_2023 = dplyr::case_when(
      Staff_RegVlntr_2023 == NA & 
        Staff_RegVlntr_NA == 1 ~ 0,
      .default = Staff_RegVlntr_2023
    ),
    Staff_EpsdVltnr_2023 = dplyr::case_when(
      Staff_EpsdVltnr_2023 == NA & 
        Staff_EpsdVltnr_NA == 1 ~ 0,
      .default = Staff_EpsdVltnr_2023
    ),
    Staff_Boardmmbr_2023 = dplyr::case_when(
      Staff_Boardmmbr_2023 == NA & 
        Staff_Boardmmbr_NA == 1 ~ 0,
      .default = Staff_Boardmmbr_2023
    ),
    PplSrv_NumWait = dplyr::case_when(
      !is.na(PplSrv_NumServed_NA_X) &
        is.na(PplSrv_NumWait) &
      PplSrv_NumWait_NA_X == 1
       ~ 0,
      PplSrv_NumWait == 0 ~ 0,
      .default = NA_integer_
    ),
    Staff_Boardmmbr_2023 = dplyr::case_when(
      Staff_Boardmmbr_2023 == NA & 
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
      Dem_BChair_Age %in% c(1, 2) ~ "Yes",
      Dem_BChair_Age %in% c(3, 4, 6, 7, 8, 9) ~ "No",
      .default = NA_character_
    ),
    BChair_POC = dplyr::case_when(
      BChairrace %in% c(1, 2, 3, 4, 6, 7) ~ "POC",
      BChairrace %in% c(5) ~ "Non-POC",
      .default = NA_character_
    ),
    CEOrace_POC = dplyr::case_when(
      CEOrace %in% c(1, 2, 3, 4, 6, 7) ~ "POC",
      CEOrace %in% c(5) ~ "Non-POC",
      .default = NA_character_
    )
  ) |>
  # recoding 1 or 2 (served) and 0 did not serve
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::starts_with("ProgDem_"),
      .fns = ~ dplyr::case_when(
        .x == 1 | .x == 2 ~ "Served",
        .x == 0 ~ "Did not Serve",
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
  Dem_CEO_Age = "% of respondents",
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
    splitByOpt = dplyr::case_when(
      splitByOpt_category %in% c(
        "<$100,000", 
        "$100,000-$499,999", 
        "$500,000-$999,999", 
        "$1 million-$9,999,999", 
        "$10 million and above"
      ) ~ "Size",
      splitByOpt_category %in% c(
        "Arts, culture, and humanities", 
        "Education", 
        "Environment and animals", 
        "Health", "Human services", 
        "International, foreign affairs", 
        "Public, societal benefit"
      ) ~ "Sector",
      splitByOpt_category %in% c("Rural", "Urban") ~ "Rural/Urban",
      .default = splitByOpt
    )
  ) |>
  tidylog::left_join(metrics_metadata, by = "metricname") |>
  dplyr::rename(responseOpt_value = responseOpt) |>
  dplyr::mutate(year = "2024")

# (3.3) - Save outputs

data.table::fwrite(
  survey_formatted,
  "data/processed/nptrends_y4_formatted.csv"
)
