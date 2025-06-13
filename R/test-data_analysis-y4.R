################### Script Header ########################
# Title: This script contains reproducible code to perform test data pulls for year 4 np trends survey data
# Description: Aggregate at national, state, region, size and subsector for the following variables:
# Revenue from individual donations
# Individuals
# Programmer: Thiyaghessan [tpoongundranar@urban.org] & Christina Prinvil [cprinvil@urban.org]
# Date created: 2025-06-13
# Date last modified: 2025-06-13
# Raw Data:
# Year 4 nptrends data: Y:/CNP/Generosity Commission/Year 4/Restricted Dataset/RESTRICTED_Y4.csv
# Details:
# (1) - Load Survey data
# (2) - Aggregate desired survey variables
##########################################################

# libraries
library(data.table)
library(data.table)

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

# Christina: Continue with the rest of the variables here. I  wrote a function for the above and I think it should work for the rest of the variables, but you can rewrite the code if needed.