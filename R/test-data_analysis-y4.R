################### Script Header ########################
# Title: This script contains reproducible code to perform test data pulls for year 4 np trends survey data
# Description: Aggregate at national, state, region, size and subsector for the following variables:
# Revenue from individual donations
# Individuals
# Programmer: Thiyaghessan [tpoongundranar@urban.org] & Christina Prinvil [cprinvil@urban.org]
# Date created: 2025-06-13
# Date last modified: 2025-06-17
# Raw Data:
# Year 4 nptrends data: Y:/CNP/Generosity Commission/Year 4/Restricted Dataset/RESTRICTED_Y4.csv
# Details:
# (1) - Load Survey data
# (2) - Aggregate desired survey variables
##########################################################

# libraries
library(data.table)
library(data.table)
library(purrr)

# Helper scripts
source(here::here("R", "summarize_survey_results.R"))

# (1) - Extract Data

nptrends_y4 <- data.table::fread("Y:/CNP/Generosity Commission/Year 4/Restricted Dataset/RESTRICTED_Y4.csv")

# (2) - Transform

# Create national group for national aggregations
nptrends_y4$National = "National"

# Named list with groupbys and weighting variable to use
# use stateweight for weighted means by state
group_ls = list(
  "National" = "year4wt",
  "CensusRegion4" = "year4wt",
  "state" = "stateweight",
  "SizeStrata" = "year4wt",
  "ntmaj12" = "year4wt"
)

# (2.1) - Revenue from Indivudual Donations (average percentage of total revenue): Finance_Rev_IndvDon_Pct


Finance_Rev_IndvDon_Pct_df <- summarise_single_variable(group_ls, survey_df = nptrends_y4, survey_var = "Finance_Rev_IndvDon_Pct")

# (2.2) - Individual donors that give less than and at or above $250 (average percentages)
# First calculate percentages for Below and above 250
nptrends_y4 <- nptrends_y4 |>
  dplyr::mutate(Dnr_blw250_pct = FndRaise_DnrBlw250/(FndRaise_DnrBlw250 + FndRaise_DnrAbv250),
                Dnr_abv250_pct = FndRaise_DnrAbv250/(FndRaise_DnrBlw250 + FndRaise_DnrAbv250))

FndRaise_DnrBlw250_df <- summarise_single_variable(group_ls, survey_df = nptrends_y4, survey_var = "Dnr_blw250_pct")

FndRaise_DnrAbv250_df <- summarise_single_variable(group_ls, survey_df = nptrends_y4, survey_var = "Dnr_abv250_pct")

# (2.3) - Share of nonprofits receiving donor advised fund grants

FndRaise_DAF_df <- summarise_single_variable(group_ls, survey_df = nptrends_y4, survey_var = "FndRaise_DAF_Rcv")

#(2.4) - Number of full time staff
# create 5 categorical variables: 0, 1, 2-9, 10-49, and 50+
nptrends_y4 <- nptrends_y4 |>
  dplyr::mutate(
    Staff = dplyr::case_when(
      Staff_Fulltime_2023 == 0 ~ "0",
      Staff_Fulltime_2023 == 1 ~ "1",
      Staff_Fulltime_2023 >= 2 & Staff_Fulltime_2023 <= 9 ~ "2-9",
      Staff_Fulltime_2023 >= 10 & Staff_Fulltime_2023 <= 49 ~ "10-49",
      Staff_Fulltime_2023 >= 50 ~ "50+",
      TRUE ~ NA_character_
    )
  )
# Now we have to create a function so they each can be a variable
staff_categories <- c("0","1","2-9","10-49","50+")

for (cat in staff_categories) {
  var_name <- paste0("staff_cat_", gsub("-","_", cat))
  nptrends_y4[[var_name]] <- as.integer(nptrends_y4$Staff == cat)
}

Staff_Fulltime_2023_df <- purrr:: map_dfr(staff_categories, function(cat) {
  var_name <- paste0("staff_cat_",gsub("-","_", cat))
  
  summarise_single_variable(group_ls, survey_df = nptrends_y4, 
                            survey_var = var_name) %>%
    dplyr::mutate(Staff = cat)})
