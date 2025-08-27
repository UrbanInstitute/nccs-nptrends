# Retrieve government grants variables from np trends and efile

# libraries
library(tidyverse)
library(data.table)
library(jsonlite)

# Helper scripts
source(here::here("R", "config.R"))

# vars
id_vars <- c("ResponseId", "EIN")
govfunding_vars <- c("ResponseId", 
                     "GovGrants", 
                     "GovFunding", 
                     "PercentGov", 
                     "Finance_Rev_GovtMain_Pct")

# Extract raw data
# ResponseId-EINs
nptrends_y5_ids <- data.table::fread(nptrends_y5_id_path, select = id_vars) 
nptrends_y4_ids <- data.table::fread(nptrends_y4_id_path, select = id_vars) 
# NP Trends survey data
nptrends_y4_raw <- data.table::fread(nptrends_y4_raw_path)
nptrends_y5_raw <- data.table::fread(nptrends_y5_raw_path)
nptrends_y5_efile_eins <- unique(nptrends_y5_efile_index$EIN)
# NP Trends efile data
nptrends_efile_data <- data.table::fread(
  "data_requests/nptrends_govgrants_082225/data/efile_nptrends_govgrants_combined.csv"
)

# Transform raw data
nptrends_efile_data_proc <- nptrends_efile_data |>
  dplyr::select(EIN, 
                TaxYear, 
                GovernmentGrantsAmt,
                TotalRevenueCY) |>
  dplyr::rename(Year = TaxYear) |>
  dplyr::mutate(Year = as.character(Year))

nptrends_y5_gov_grants <- nptrends_y5_raw |>
  dplyr::select(dplyr::any_of(govfunding_vars)) |> 
  dplyr::mutate(Year = "2024") |>
  tidylog::left_join(nptrends_y5_ids, by = "ResponseId") 

nptrends_y4_gov_grants <- nptrends_y4_raw |>
  dplyr::select(dplyr::any_of(govfunding_vars)) |>
  dplyr::mutate(Year = "2023") |>
  dplyr::rename(PercentGov = Finance_Rev_GovtMain_Pct) |>
  tidylog::left_join(nptrends_y4_ids, by = "ResponseId") |>
  dplyr::filter(EIN %in% nptrends_y5_gov_grants$EIN)


nptrends_govgrnts_full <- nptrends_y4_gov_grants |>
  dplyr::bind_rows(nptrends_y5_gov_grants) |>
  dplyr::filter(EIN %in% nptrends_efile_data_proc$EIN)

nptrends_govgrnts_full_eins <- unique(nptrends_govgrnts_full$EIN)

# Save EINs for python processing
nptrends_govgrnts_eins_json <- jsonlite::toJSON(nptrends_govgrnts_full_eins)
jsonlite::write_json(nptrends_govgrnts_eins_json,
                     "data_requests/nptrends_govgrants_082225/data/nptrends_y5_eins.json")

nptrends_govgrnts_efile_merge <- nptrends_govgrnts_full |>
  tidylog::left_join(nptrends_efile_data_proc, by = c("EIN", "Year"))

# Load processed data
nptrends_govgrnts_full_proc <- nptrends_govgrnts_efile_merge |>
  dplyr::mutate(`percentage of total revenue from gov grants from 990` = 
                  GovernmentGrantsAmt / TotalRevenueCY,
                `y/n gov grants on 990` = ifelse(
                  ! is.na(GovernmentGrantsAmt) & GovernmentGrantsAmt != 0,
                  "Yes",
                  "No"
                ),
                dplyr::across(
                  .cols = c("GovGrants", "GovFunding"),
                  .fns = ~ dplyr::case_when(.x == 1 ~ "Yes",
                                            .x == 0 ~ "No",
                                            .default = "Did not answer question")
                )) |>
  dplyr::rename(
    `y/n gov grants on survey` = GovGrants,
    `y/n any gov funding on survey` = GovFunding,
    `percentage of total revenue from gov from survey` = PercentGov
  ) |>
  dplyr::mutate(
    `percentage of total revenue from gov grants from 990` = scales::percent(`percentage of total revenue from gov grants from 990`,
                                                                             accuracy = 1),
    `percentage of total revenue from gov from survey` = scales::percent(
      `percentage of total revenue from gov from survey`,
      scale = 1,
      accuracy = 1
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = c("percentage of total revenue from gov grants from 990"),
      .fns = ~ dplyr::case_when(is.na(GovernmentGrantsAmt) ~ "0%",
                                is.na(TotalRevenueCY) ~ "Did not file form 990",
                                .default = .x)
    ),
    dplyr::across(
      .cols = c("percentage of total revenue from gov from survey"),
      .fns = ~ dplyr::case_when(is.na(.x) ~ "Did not answer question",
                                .default = .x)
    )
  ) |>
  dplyr::select(
    EIN,
    `y/n gov grants on 990`,
    `y/n gov grants on survey`,
    `y/n any gov funding on survey`,
    `percentage of total revenue from gov grants from 990`,
    `percentage of total revenue from gov from survey`,
    Year
  ) |>
  unique() |>
  tidyr::pivot_wider(
    names_from = "Year",
    values_from = c(
      "y/n gov grants on 990",
      "y/n gov grants on survey",
      "y/n any gov funding on survey",
      "percentage of total revenue from gov grants from 990",
      "percentage of total revenue from gov from survey"
    ),
    names_sep = " - "
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = c(
        "y/n gov grants on 990 - 2023", 
        "y/n gov grants on survey - 2023", 
        "y/n any gov funding on survey - 2023", 
        "percentage of total revenue from gov grants from 990 - 2023", 
        "percentage of total revenue from gov from survey - 2023"
      ),
      .fns = ~ dplyr::case_when(is.na(.x) ~ "Respondent absent from year 4 survey",
                                .default = .x)
    ),
    `Org "name` = row_number()
  ) |>
  dplyr::select(!EIN)

# Save outputs
data.table::fwrite(
  nptrends_govgrnts_full_proc,
  "data_requests/nptrends_govgrants_082225/data/990 and survey gov funding comparison.csv"
)
