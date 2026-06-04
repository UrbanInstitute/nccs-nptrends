# Load the Year 5, Year 6, and Year 6 "staffed" datasets and the tests template.
#
# Three source datasets are referenced by the template's `dataset` column:
#   nptrends_y5_clean         — Year 5 (2025) respondents,        weight = year5wt
#   nptrends_y6_clean         — Year 6 (2026) respondents,        weight = weight_year6plus
#   nptrends_y6_staffed_clean — Year 6 orgs with paid staff only, weight = weight_year6plus
#
# The two large *_clean.csv files and the staffed file are git-ignored
# (data/input/.gitignore) — they are restricted survey microdata.

library(data.table)
library(openxlsx)

here_root <- here::here("data_requests",
                        "government_funding_disruptions_prevalence_wide_060426")

y5      <- data.table::fread(file.path(here_root, "data/input/nptrends_y5_clean.csv"))
y6      <- data.table::fread(file.path(here_root, "data/input/nptrends_y6_clean.csv"))
y6staff <- data.table::fread(file.path(here_root, "data/input/nptrends_y6_staffed_clean.csv"))

template <- openxlsx::read.xlsx(
  file.path(here_root, "data/input/Government Funding Disruptions - CI and Stats Tests.xlsx"),
  sheet = 1
)
