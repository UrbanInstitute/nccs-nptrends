# Ensures the Year 6 source variables named in the data dictionary are
# requested from the raw Y6 file via survey_analysis_vars in R/config.R.
# This catches the case where a dictionary update introduces a new column
# the pipeline isn't reading.

library(testthat)
library(data.table)

root <- "../../"

# The dictionary's variableNameY6 column may list a single column or several
# comma/and-separated columns. Tokenize on commas, "and", and whitespace, then
# drop sentinel non-variable strings.
extract_y6_vars <- function(cell) {
  if (is.na(cell) || !nzchar(cell)) return(character(0))
  if (grepl("^same as Y5", cell, ignore.case = TRUE)) return(character(0))
  if (grepl("^NA", cell)) return(character(0))
  # Substitute the dictionary's [current year]/[previous year] placeholder
  # for staff variables (Y6 wave: previous year = 2025).
  cell <- gsub("\\[current year\\]", "2025", cell)
  cell <- gsub("\\[previous year\\]", "2025", cell)
  cell <- gsub("\\[prior year\\]", "2025", cell)
  parts <- unlist(strsplit(cell, "(,| and )", perl = TRUE))
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  # Drop residual bracket annotations.
  parts <- parts[!grepl("\\[", parts)]
  parts
}

testthat::test_that("All Y6 source variables in the dictionary are in survey_analysis_vars or derivable", {
  skip_if_not_installed("readxl")
  dd <- readxl::read_excel(paste0(root, data_dictionary_path))

  y6_var_lists <- lapply(dd$variableNameY6, extract_y6_vars)
  y6_vars <- unique(unlist(y6_var_lists))

  # Variables computed inside the pipeline rather than read from raw Y6.
  derived_in_pipeline <- c(
    "GeoAreas_Local", "GeoAreas_MultipleLocal", "GeoAreas_RegionalWithin",
    "GeoAreas_RegionalAcross", "GeoAreas_MultipleState",
    "Staff_RegVlntr_NA", "Staff_EpsdVltnr_NA", "Staff_Boardmmbr_NA",
    "Staff_Fulltime_NA", "Staff_Parttime_NA"
  )
  missing <- setdiff(y6_vars, c(survey_analysis_vars, derived_in_pipeline))
  testthat::expect_equal(missing, character(0),
                         info = paste("Y6 variables missing from config:",
                                      paste(missing, collapse = ", ")))
})

testthat::test_that("Per-metric Y6 weight assignments cover only known metrics", {
  unknown <- setdiff(y6_metric_weights$metricname, metricID_lookup$metricname)
  testthat::expect_equal(unknown, character(0),
                         info = paste("Unknown metricnames in y6_metric_weights:",
                                      paste(unknown, collapse = ", ")))
})

testthat::test_that("metrics_drop_y4_y5 references valid metric names", {
  unknown <- setdiff(metrics_drop_y4_y5, metricID_lookup$metricname)
  testthat::expect_equal(unknown, character(0),
                         info = paste("Unknown metricnames in metrics_drop_y4_y5:",
                                      paste(unknown, collapse = ", ")))
})

testthat::test_that("percentdem_4bucket_vars references known PercentDem variables", {
  unknown <- setdiff(percentdem_4bucket_vars, percentdem_vars)
  testthat::expect_equal(unknown, character(0))
})
