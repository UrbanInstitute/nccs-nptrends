rm(list = ls())

source("C:/Users/tpoongundranar/Documents/Urban/NCCS/nccs-nptrends/adhoc_requests/fidelity_hmartin_022224/fidelity_scoping_helpers.R")
setwd("Y:/CNP/Generosity Commission/")

library(readr)
library(dplyr)
library(rio)

# Global Variables

# Target states in Mountain West and South Central
SCMW_STATES_ABBR <- scan(
  text='Alabama
        Arizona
        Arkansas
        Colorado
        Idaho
        Kentucky
        Louisiana
        Mississippi
        Montana
        Nevada
        New Mexico
        Oklahoma
        Tennessee
        Texas
        Utah
        Wyoming
        ',
  what = "", 
  sep=",", 
  quiet=TRUE, 
  strip.white=TRUE
) |> usdata::state2abbr()

MW_STATES_ABBR <- scan(
  text = 'Arizona 
  Colorado 
  Idaho 
  Montana 
  Nevada 
  New Mexico 
  Utah 
  Wyoming',
  what = "", 
  sep=",", 
  quiet=TRUE, 
  strip.white=TRUE
) |> usdata::state2abbr()

SC_STATES_ABBR <- scan(
  text = 'Alabama 
  Arkansas 
  Kentucky 
  Louisiana 
  Mississippi 
  Oklahoma 
  Tennessee 
  Texas',
  what = "", 
  sep=",", 
  quiet=TRUE, 
  strip.white=TRUE
) |> usdata::state2abbr()

VARS_YEAR1_TARGET <- scan(
  text = "DonImportance VolImportance FRchanges_1 FRchanges_2 FRchanges_3 FRchanges_2_1 FRchanges_2_2
FRchanges_2_3 Funding2_1_5_1 CEOrace BCrace",
  what = "",
  sep = "",
  quiet=TRUE,
  strip.white=TRUE
)

VARS_YEAR2_TARGET <- scan(
  text = "DonImportance
VolImportance
FRchanges_1
FRchanges_2
FRchanges_3
FRchanges_4
FRchanges_5
FRchanges_6
FRchanges_7
FRchanges_8
FRchanges_9
FRchanges_1_1
FRchanges_1_2
FRchanges_1_3
FRchanges_1_6
FRchanges_1_7
Funding1_1_9
Funding1_2_9
CEOrace_1
CEOrace_2
CEOrace_3
CEOrace_4
CEOrace_5
CEOrace_6
CEOrace_7
BCrace_1
BCrace_2
BCrace_3
BCrace_4
BCrace_5
BCrace_6
BCrace_7
",
  what = "",
  sep = "",
  quiet=TRUE,
  strip.white=TRUE
)


# Load in Survey data

year1_raw <- readr::read_csv("Qualtrics Survey/Intermediate/gc_survey_27apr_21.csv")
year2_raw <- rio::import("Qualtrics Survey/Intermediate/nptrends_year2_13jan23_with_y2_weight.sav")
year3_raw <- readr::read_csv("Qualtrics Survey/Year 3/Nonprofit Trends and Impacts 2023 - Complete.csv")
year3_raw <- year3_raw[c(-1, -2), ]

lat <- year3_raw$LocationLatitude
lon <- year3_raw$LocationLongitude

census_api <- function(lat, lon){
  
  query_params <- list( latitude = as.character( lat ),
                        longitude = as.character( lon ),
                        censusYear = "2020",
                        format = "json" )
  
  response <- httr::GET( "https://geo.fcc.gov/api/census/area?",
                         query = query_params )
  
  body <- httr::content( response, 
                         "parsed" )
  
  return(body$State$code)
  
}

state_codes <- purrr::map2(.x = lat,
                           .y = lon,
                           .f = census_api)

scmw_y3 <- year3_raw %>% 
  dplyr::filter(ExternalReference %in% year1_SCMWregion$EIN)

get_multi_variable_count <- function( variables, data, year, region ){
  varcounts_df <- purrr::map(
    .x = variables,
    .f = get_single_variable_count,
    dataset = data
  ) |> purrr::list_rbind()
  varcounts_df$`Year` <- as.character(year)
  varcounts_df$`Region` <- as.character(region)
  return(varcounts_df)
}


get_single_variable_count <- function(var_name, dataset){
  df <- dataset %>% 
    dplyr::select(tidyselect::all_of(var_name)) %>% 
    dplyr::filter(.data[[var_name]] != -99) %>% 
    tidyr::drop_na() %>% 
    dplyr::summarize("Number of Responses" = sum(! is.na(.data[[var_name]])),
                     "Variable Name" = var_name)
  return(df)
}


eins <- year1_SCMWregion %>% 
  dplyr::filter(state %in% MW_STATES_ABBR) %>%
  dplyr::pull("EIN")

scmw_y3 %>% 
  dplyr::filter(ExternalReference %in% eins)|>
  get_multi_variable_count(
    variables = names(year3_raw),
    year = 3,
    region = "South Central and Mountain West"
  ) |>
  View()

year1_SCMWregion %>% 
  dplyr::filter(EIN %in% eins,
                EIN %in% scmw_y3$ExternalReference)|>
  get_multi_variable_count(
    variables = VARS_YEAR1_TARGET,
    year = 1,
    region = "South Central and Mountain West"
  ) |> View()

View(rs)


# Filter for target sates: South Central and Mountain West = SCMW

year1_SCMWregion <- year1_raw %>% 
  dplyr::filter(state %in% SCMW_STATES_ABBR)

year1_SCMWregion_varcounts_df <- year1_SCMWregion |>
  get_multi_variable_count(
    variables = VARS_YEAR1_TARGET,
    year = 1,
    region = "South Central and Mountain West"
  )

year1_MWregion_varcounts_df <- year1_SCMWregion %>% 
  dplyr::filter(state %in% MW_STATES_ABBR) |>
  get_multi_variable_count(
    variables = VARS_YEAR1_TARGET,
    year = 1,
    region = "Mountain West"
  )

year1_SCregion_varcounts_df <- year1_SCMWregion %>% 
  dplyr::filter(state %in% SC_STATES_ABBR) |>
  get_multi_variable_count(
    variables = VARS_YEAR1_TARGET,
    year = 1,
    region = "South Central"
  )

# Combine Year 1 Results
year1_varcounts_df <- purrr::list_rbind(
  list(
    year1_SCMWregion_varcounts_df,
    year1_SCregion_varcounts_df,
    year1_MWregion_varcounts_df
  )
)

# Unique EINs for South Central and Mountain West Regions
ein_SCMWregion <- unique(year1_SCMWregion$EIN)
ein_MWregion <- year1_SCMWregion %>% 
  dplyr::filter(state %in% MW_STATES_ABBR) %>% 
  dplyr::pull("EIN")
ein_SCregion <- year1_SCMWregion %>% 
  dplyr::filter(state %in% SC_STATES_ABBR) %>% 
  dplyr::pull("EIN")

# Year 2 data sets
year2_SCMWregion <- year2_raw %>% 
  dplyr::filter(ein %in% ein_SCMWregion)

year2_SCregion <- year2_raw %>% 
  dplyr::filter(ein %in% ein_SCregion)

year2_MWregion <- year2_raw %>% 
  dplyr::filter(ein %in% ein_MWregion)

# Process year 2 data

# Board race columns
race_board_cols <- c(
  "BCrace_1",
  "BCrace_2",
  "BCrace_3",
  "BCrace_4",
  "BCrace_5",
  "BCrace_7"
)

# CEO race columns
race_ceo_cols <- c(
  "CEOrace_1",
  "CEOrace_2",
  "CEOrace_3",
  "CEOrace_4",
  "CEOrace_5",
  "CEOrace_7"
)

all_race_cols <- c(race_ceo_cols, race_board_cols)

# EINs that have not changed board members in Year 2 Survey
ein_ceorace_chng <- year2_SCMWregion %>% 
  dplyr::filter(! (LeadershipChanges_4 %in% c(1) | LeadershipChanges_5 %in% c(1) ))   %>% 
  dplyr::pull("ein")

# EINs that have not changed CEOs in Year 2 Survey
ein_brace_chng <- year2_SCMWregion %>% 
  dplyr::filter( ! LeadershipChanges_6 %in% c(1)) %>% 
  dplyr::pull("ein")

# Get Survey responses for race of Board members from Year 1 for participants
# in year 2 that reported no change
year1_SCMWregion_brace_chng <- year1_SCMWregion %>% 
  dplyr::filter(EIN %in% ein_brace_chng,
                ! BCrace %in% c(-99, NA)) %>% 
  dplyr::select(state, BCrace) %>% 
  dplyr::mutate(BCrace = paste0("BCrace_", BCrace)) %>%
  dplyr::group_by(state, BCrace) %>% 
  dplyr::summarise("Number of Responses" = n()) %>% 
  dplyr::rename("Variable Name" = "BCrace")

year1_SCregion_brace_chng <- year1_SCMWregion_brace_chng %>% 
  dplyr::filter(state %in% SC_STATES_ABBR)

year1_MWregion_brace_chng <- year1_SCMWregion_brace_chng %>% 
  dplyr::filter(state %in% MW_STATES_ABBR)

# Get Survey responses for race of CEOs from Year 1 for participants
# in year 2 that reported no change
year1_SCMWregion_ceorace_chng <- year1_SCMWregion %>% 
  dplyr::filter(EIN %in% ein_ceorace_chng,
                ! CEOrace %in% c(-99, NA)) %>% 
  dplyr::select(state, CEOrace) %>% 
  dplyr::mutate(CEOrace = paste0("CEOrace_", CEOrace)) %>%
  dplyr::group_by(state, CEOrace) %>% 
  dplyr::summarise("Number of Responses" = n()) %>% 
  dplyr::rename("Variable Name" = "CEOrace")

year1_SCregion_ceorace_chng <- year1_SCMWregion_ceorace_chng %>% 
  dplyr::filter(state %in% SC_STATES_ABBR)

year1_MWregion_ceorace_chng <- year1_SCMWregion_ceorace_chng %>% 
  dplyr::filter(state %in% MW_STATES_ABBR)

# Recode Board Member and CEO race variables in Year 2 Survey data for EINs
# to include a category for bi-racial classifications
year2_SCMWregion_proc <- year2_SCMWregion %>%
  dplyr::rowwise() %>% 
  dplyr::mutate(BCrace_6 = ifelse(sum(dplyr::across(race_board_cols),
                                      na.rm = TRUE) > 1,
                                  1,
                                  NA)) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(CEOrace_6 = ifelse(sum(dplyr::across(race_ceo_cols),
                                       na.rm = TRUE) > 1,
                                   1,
                                   NA)) %>% 
  dplyr::mutate_at(race_board_cols, ~ ifelse(! is.na(BCrace_6), NA, . )) %>% 
  dplyr::mutate_at(race_ceo_cols, ~ ifelse(! is.na(CEOrace_6), NA, . )) %>% 
  dplyr::mutate_at(all_race_cols, ~ dplyr::na_if(.x, 0)) 


year2_SCregion_proc <- year2_SCMWregion_proc %>% 
  dplyr::filter(ein %in% ein_SCregion)

year2_MWregion_proc <- year2_SCMWregion_proc %>% 
  dplyr::filter(ein %in% ein_MWregion)

# Get counts of responses for year 2 survey participants
year2_SCMWregion_responses <- get_multi_variable_count(
  variables = VARS_YEAR2_TARGET,
  data = year2_SCMWregion_proc,
  year = 2,
  region = "South Central and Mountain West"
)

year2_SCregion_responses <- get_multi_variable_count(
  variables = VARS_YEAR2_TARGET,
  data = year2_SCregion_proc,
  year = 2,
  region = "South Central"
)

year2_MWregion_responses <- get_multi_variable_count(
  variables = VARS_YEAR2_TARGET,
  data = year2_MWregion_proc,
  year = 2,
  region = "Mountain West"
)

# Get counts of responses including year 1 responses for race when CEO/Board
# Member race in Year 2 does not change
year2_SCMWregion_varcounts_df <- purrr::list_rbind(
  list(
    year2_SCMWregion_responses, 
    year1_SCMWregion_brace_chng %>% 
      dplyr::select(!state),
    year1_SCMWregion_ceorace_chng %>% 
      dplyr::select(!state)
  )
) %>% 
  dplyr::group_by(`Variable Name`) %>% 
  dplyr::summarise("Number of Responses" = sum(`Number of Responses`, na.rm = TRUE))

year2_SCregion_varcounts_df <- purrr::list_rbind(
  list(
    year2_SCregion_responses, 
    year1_SCregion_brace_chng %>% 
      dplyr::select(!state),
    year1_SCregion_ceorace_chng %>% 
      dplyr::select(!state)
  )
) %>% 
  dplyr::group_by(`Variable Name`) %>% 
  dplyr::summarise("Number of Responses" = sum(`Number of Responses`, na.rm = TRUE))

year2_MWregion_varcounts_df <- purrr::list_rbind(
  list(
    year2_MWregion_responses, 
    year1_MWregion_brace_chng %>% 
      dplyr::select(!state),
    year1_MWregion_ceorace_chng %>% 
      dplyr::select(!state)
  )
) %>% 
  dplyr::group_by(`Variable Name`) %>% 
  dplyr::summarise("Number of Responses" = sum(`Number of Responses`, na.rm = TRUE))


year2_varcounts_df <- purrr::list_rbind(
  list(
    year2_SCMWregion_varcounts_df %>% 
      dplyr::mutate(Year = "2",
                    Region = "South Central and Mountain West"),
    year2_SCregion_varcounts_df  %>% 
      dplyr::mutate(Year = "2",
                    Region = "South Central"),
    year2_MWregion_varcounts_df  %>% 
      dplyr::mutate(Year = "2",
                    Region = "Mountain West")
  )
)
year2_varcounts_df

# Combine all results together

varcounts_df <- purrr::list_rbind(
  list(
    year1_varcounts_df,
    year2_varcounts_df
  )
)

View(varcounts_df)

write.csv(
  varcounts_df,
  "C:/Users/tpoongundranar/Documents/Urban/NCCS/nccs-nptrends/adhoc_requests/fidelity_hmartin_022224/scmw_varcounts.csv"
)
