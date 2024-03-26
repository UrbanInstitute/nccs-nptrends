rm(list = ls())

# Load in packages
library(readr)
library(dplyr)
library(rio)
library(readxl)

# Load in crosswalk
xwalk_df <- readxl::read_xlsx(
  "C:/Users/tpoongundranar/Documents/Urban/NCCS/nccs-nptrends/dd-nptrends-wave-02.xlsx",
  sheet = 3
)

# Load in helper functions
source("C:/Users/tpoongundranar/Documents/Urban/NCCS/nccs-nptrends/adhoc_requests/fidelity_hmartin_022224/fidelity_scoping_helpers.R")

# Load in datasets
setwd("Y:/CNP/Generosity Commission/")
year1_raw <- readr::read_csv("Qualtrics Survey/Intermediate/gc_survey_27apr_21.csv")
year2_raw <- readr::read_csv("DATA-PREP/02-data-intermediate/02-wave-two/wave-02-data-intermediate-recoded.csv")
year3_raw <- readr::read_csv("Qualtrics Survey/Year 3/Nonprofit Trends and Impacts 2023 - Complete.csv")
year3_raw <- year3_raw[c(-1, -2), ]

# Select relevant columns and convert with crosswalk

## Year 1 Survey Data
year1_xwalk <- xwalk_df %>% 
  dplyr::select(vname, vname_y1) %>% 
  tidyr::drop_na()
year1_cols <- c("state", "EIN", year1_xwalk$vname_y1)
year1_subset <- year1_raw %>% 
  dplyr::select(tidyselect::all_of(year1_cols)) %>% 
  dplyr::rename_with(~ year1_xwalk$vname, .cols = year1_xwalk$vname_y1) %>% 
  dplyr::mutate(EIN = as.character(EIN))

## Year 2 Survey Data
year2_xwalk <- xwalk_df %>% 
  dplyr::select(vname, vname_y2) %>% 
  tidyr::drop_na()
year2_cols <- c("EIN", 
                year2_xwalk$vname,
                "CEOrace_Bi",
                "BChairrace_Bi")
year2_subset <- year2_raw %>% 
  dplyr::select(tidyselect::all_of(year2_cols)) %>% 
  dplyr::mutate(EIN = as.character(EIN))

## Year 3 Survey Data
year3_xwalk <- xwalk_df %>% 
  dplyr::select(vname, vname_y3) %>% 
  tidyr::drop_na()
year3_cols <- c("ExternalReference", year3_xwalk$vname_y3)
year3_subset <- year3_raw %>% 
  dplyr::select(tidyselect::all_of(year3_cols)) %>% 
  dplyr::rename("EIN" = "ExternalReference") %>% 
  dplyr::rename_with(~ year3_xwalk$vname, .cols = year3_xwalk$vname_y3)

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

# Filter out EINs not in state and left join EINs
scmw_year1 <- year1_subset %>% 
  dplyr::filter(state %in% SCMW_STATES_ABBR)

scmw_df <- scmw_year1 %>% 
  dplyr::select(EIN, state)

scmw_eins <- scmw_df$EIN

scmw_year2 <- year2_subset %>% 
  dplyr::filter(EIN %in% scmw_year1$EIN) %>% 
  tidylog::left_join(scmw_df, by = "EIN") %>% 
  dplyr::mutate(CEOrace = dplyr::select(., tidyr::contains("CEOrace")) %>%  rowSums(na.rm = TRUE),
                CEOgender = dplyr::select(., tidyr::contains("CEOgender")) %>%  rowSums(na.rm = TRUE),
                BChairrace = dplyr::select(., tidyr::contains("BChairrace")) %>%  rowSums(na.rm = TRUE),
                BChairgender = dplyr::select(., tidyr::contains("BChairgender")) %>%  rowSums(na.rm = TRUE))


scmw_year3 <- year3_subset %>% 
  dplyr::filter(EIN %in% scmw_year1$EIN) %>% 
  tidylog::left_join(scmw_df, by = "EIN")

# Get response rates

## Year 1 Response Rates
year1_responses <- purrr::map(.x = year1_xwalk$vname,
           .f = function(v){
             if (grepl("DAF", v)){
               missing_vals <- c(NA)
             } else {
               missing_vals <- c(-99, NA)
             }
             scmw_year2 %>% 
               dplyr::select(state, v) %>% 
               dplyr::filter(! .data[[v]] %in% missing_vals) %>% 
               dplyr::group_by(state) %>% 
               dplyr::summarise(count = n()) %>% 
               dplyr::mutate(qn = v)
           }) |> 
  purrr::list_rbind() 

readr::write_csv(year1_responses, "year1_response.rates_state.csv")

## Year 2 Response Rates

### Create new column for race
year2_vars <- c(year2_xwalk$vname[1:38])
year2_vars <- year2_vars[! grepl("race|gender", year2_vars)]
year2_vars <- c(year2_vars, "CEOrace", "CEOgender", "BChairrace", "BChairgender")

year2_responses <- purrr::map(.x = year2_vars,
                              .f = function(v){
                                if (grepl("DAF", v)){
                                  missing_vals <- c(NA, 97, 98, 99)
                                } else {
                                  missing_vals <- c(-99, NA, 97, 98, 99)
                                }
                                scmw_year2 %>% 
                                  dplyr::select(state, v) %>% 
                                  dplyr::filter(! .data[[v]] %in% missing_vals) %>% 
                                  dplyr::group_by(state) %>% 
                                  dplyr::summarise(count = n()) %>% 
                                  dplyr::mutate(qn = v)
                              }) |> 
  purrr::list_rbind()

readr::write_csv(year2_responses, "year2_response.rates_state.csv")

## Year 3 Response Rates
year3_responses <- purrr::map(.x = year3_xwalk$vname,
                              .f = function(v){
                                if (grepl("DAF", v)){
                                  missing_vals <- c(NA)
                                } else {
                                  missing_vals <- c(-99, NA)
                                }
                                scmw_year3 %>% 
                                  dplyr::select(state, v) %>% 
                                  dplyr::filter(! .data[[v]] %in% missing_vals) %>% 
                                  dplyr::group_by(state) %>% 
                                  dplyr::summarise(count = n()) %>% 
                                  dplyr::mutate(qn = v)
                              }) |> 
  purrr::list_rbind() 

readr::write_csv(year3_responses, "year3_response.rates_state.csv")

# Augment year 3 responses with year 1 variables

## Find EINs of respondents where CEO race and gender unchanged
no_ceo_chng_ein <- year3_subset %>% 
  dplyr::filter(
    ! (LeadershipChng_HireCEO == 1 | LeadershipChng_IntrmCEO == 1),
    EIN %in% scmw_eins
  ) %>% 
  dplyr::pull("EIN")

no_bchair_chng_ein <- year3_subset %>% 
  dplyr::filter(
    LeadershipChng_ChngBC != 1,
    EIN %in% scmw_eins
  ) %>% 
  dplyr::pull("EIN")

scmw_year1 %>% 
  dplyr::select(EIN, BChairgender, state) %>% 
  dplyr::filter(EIN %in% no_bchair_chng_ein) %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarise(count = n())

## Pull and wrangle year2 responses with CEO race and gender
race_gender_cols <- year2_cols[grepl("race|gender", year2_cols)]

year2_CEOchng <- year2_subset %>% 
  dplyr::select(EIN, tidyselect::all_of(race_gender_cols)) %>% 
  tidyr::pivot_longer(!EIN, names_to = "")

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
