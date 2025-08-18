# Unit testing to ensure output data matches data template

# libraries
library(testthat)
library(data.table)
library(tidyverse)

# Datasets used across multiplte testthat functions
root <- "../../"
nptrends_full_postprocessed <- data.table::fread(
  paste0(root, nptrends_full_postproc_path)
)
template <- data.table::fread(paste0(root, template_path))

# Tests
testthat::test_that("All permutations in template present", {
  combinations_validate_df <- data.table::fread(
    paste0(root, combinations_validate_path)
  )
  
  postproc_combinations <- nptrends_full_postprocessed |>
    dplyr::mutate(responseOpt = tolower(responseOpt)) |>
    dplyr::select(filterType,
                  filterOpt,
                  splitByOpt,
                  splitByOpt_category,
                  responseOpt) |>
    unique()
  testthat::expect_true(data.table::fsetequal(
    data.table::setDT(postproc_combinations),
    data.table::setDT(combinations_validate_df)
  ))
})

testthat::test_that("Values were computed correctly", {
  # Values computed using Hannah's scripts in Y:\CNP\Generosity Commission\Year 5\Analyses\Dashboard Checking
  validation_df <- data.table::fread("../../data/validate/validate-value.csv", 
                                     na.strings = "")
  validation_df <- validation_df |>
    dplyr::mutate(key = paste0(metricID, 
                               responseOpt, 
                               splitByOpt_category, 
                               year)) |>
    dplyr::select(key, value) |>
    arrange(key)
  postproc_validate <- nptrends_full_postprocessed |>
    dplyr::filter(metricID %in% c(1, 4, 62),
                  filterType == "National",
                  filterOpt == "National") |>
    dplyr::select(
      metricID,
      responseOpt,
      splitByOpt_category,
      year,
      value
    ) |>
    unique() |>
    dplyr::mutate(
      year = as.character(year),
      metricID = as.integer(metricID)
    ) |>
    dplyr::mutate(key = paste0(metricID, 
                               responseOpt, 
                               splitByOpt_category, 
                               year)) |>
    dplyr::select(key, value) |>
    dplyr::arrange(key)
  testthat::expect_equal(validation_df, postproc_validate, tolerance = 0.02)
})

testthat::test_that("Metrics are numbered and ordered correctly", {
  templateids <- template |>
    dplyr::select(metricID, responseOpt) |>
    unique()
  myids <- nptrends_full_postprocessed |>
    dplyr::select(metricID, responseOpt) |>
    unique()
  testthat::expect_equal(templateids, myids)
})