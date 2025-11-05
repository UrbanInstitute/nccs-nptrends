# Unit testing to ensure output data matches data template

# libraries
library(testthat)
library(data.table)
library(tidyverse)

# Datasets used across multiplte testthat functions
root <- "../../"
nptrends_full_formatted <- data.table::fread(
  paste0(root, nptrends_full_formatted_path),
  colClasses = list(character = c("responseOpt")),
  na.strings = ""
)
nptrends_full_filtered <- data.table::fread(
  paste0(root, nptrends_full_filtered_path),
  colClasses = list(character = c("responseOpt")),
  na.strings = ""
)
template <- data.table::fread(paste0(root, template_path),
                              colClasses = list(character = c("responseOpt")))

# Tests
testthat::test_that("Correct states have been excluded", {
  
  extract_valid_states <- function(data, yr) {
    data |>
      dplyr::filter(year == yr) |>
      tidyr::pivot_longer(cols = c(filterOpt, splitByOpt_category),
                          values_to = "disaggregations") |>
      distinct(disaggregations) |>
      dplyr::filter(disaggregations %in% usdata::state_stats$abbr) |>
      dplyr::pull(disaggregations)
  }
  
  states_present_2024 <- extract_valid_states(nptrends_full_filtered, 2024)
  states_present_2025 <- extract_valid_states(nptrends_full_filtered, 2025)  
  
  excluded_2024 <- states_to_exclude[["2024"]]
  excluded_2025 <- states_to_exclude[["2025"]]
  
  testthat::expect_false(any(states_present_2024 %in% excluded_2024))
  testthat::expect_false(any(states_present_2025 %in% excluded_2025))
})

# We filter responseOpt because the final dataset does not have the binary == 0 values (e.g. has "Serving seniors" but not "Not serving seniors")
testthat::test_that("All permutations in template present", {
  combinations_validate_df <- data.table::fread(
    paste0(root, combinations_validate_path)
  )
  
  formatted_combinations <- nptrends_full_formatted |>
    dplyr::filter(responseOpt %in% combinations_validate_df$responseOpt) |>
    dplyr::select(filterType,
                  filterOpt,
                  splitByOpt,
                  splitByOpt_category,
                  responseOpt) |>
    unique()
  testthat::expect_true(data.table::fsetequal(
    data.table::setDT(formatted_combinations),
    data.table::setDT(combinations_validate_df)
  ))
})

testthat::test_that("Values were computed correctly", {
  # Values computed by Hannah
  validation_df <- data.table::fread("../../data/validate/validate-value.csv", 
                                     na.strings = "")
  validation_df <- validation_df |>
    dplyr::filter(metricID %in% c(1, 5, 36)) |>
    dplyr::mutate(
      splitByOpt = ifelse(is.na(splitByOpt), "", splitByOpt),
      splitByOpt_category = ifelse(is.na(splitByOpt_category), 
                                   "", 
                                   splitByOpt_category)
    ) |>
    dplyr::mutate(key = paste0(metricID,
                               year,
                               filterType,
                               filterOpt,
                               splitByOpt,
                               splitByOpt_category, 
                               responseOpt),
                  value = as.numeric(value)) |>
    dplyr::select(key, value) |>
    arrange(key)
  postproc_validate <- nptrends_full_filtered |>
    dplyr::mutate(key = paste0(metricID,
                               year,
                               filterType,
                               filterOpt,
                               splitByOpt,
                               splitByOpt_category, 
                               responseOpt),
                  value = as.numeric(value)) |>
    dplyr::select(key, value) |>
    dplyr::filter(key %in% validation_df$key) |>
    arrange(key) |>
    unique()
  testthat::expect_equal(validation_df, postproc_validate, tolerance = 0.02)
})

testthat::test_that("Metrics are numbered and ordered correctly", {
  templateids <- template |>
    dplyr::select(metricID, responseOpt) |>
    unique() |>
    dplyr::arrange(responseOpt)
  myids <- nptrends_full_filtered |>
    dplyr::select(metricID, responseOpt) |>
    dplyr::filter(responseOpt %in% templateids$responseOpt) |>
    unique()  |>
    dplyr::arrange(responseOpt)
  testthat::expect_equal(templateids, myids)
})

# We expect a subset because the final dataset does not have the binary == 0 values (e.g. has "Serving seniors" but not "Not serving seniors")
testthat::test_that("ResponseOpt values are formatted correctly", {
  myresponseOpt <- nptrends_full_filtered |>
    dplyr::pull(responseOpt)
  templateresponseOpt <- template |>
    dplyr::pull(responseOpt)
  testthat::expect_true(all(templateresponseOpt %in% myresponseOpt))
})

testthat::test_that("Correct column names are present", {
  myresponseOpt <- nptrends_full_filtered |>
    names() |>
    sort()
  templateresponseOpt <- template |>
    names() |>
    sort()
  testthat::expect_equal(myresponseOpt, templateresponseOpt)
})