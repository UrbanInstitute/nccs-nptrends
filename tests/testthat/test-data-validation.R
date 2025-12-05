# Unit testing to ensure output data matches data template

# libraries
library(testthat)
library(data.table)
library(tidyverse)

# Datasets used across multiplte testthat functions
root <- "../../"
nptrends_full_filtered <- data.table::fread(
  paste0(root, nptrends_full_filtered_path),
  colClasses = list(character = c("responseOpt"))
)
template <- data.table::fread(paste0(root, template_path),
                              colClasses = list(character = c("responseOpt")))

# Tests
testthat::test_that("All excluded states have NA values", {
  
  extract_valid_states <- function(data, yr) {
    data |>
      dplyr::filter(year == yr) |>
      tidyr::pivot_longer(cols = c(filterOpt, splitByOpt_category),
                          values_to = "disaggregations") |>
      dplyr::filter(disaggregations %in% states_to_exclude[[as.character(yr)]]) |>
      dplyr::pull(value)
  }
  
  excluded_state_values_2024 <- extract_valid_states(nptrends_full_filtered, 2024)
  excluded_state_values_2025 <- extract_valid_states(nptrends_full_filtered, 2025)  
  

  testthat::expect_true(all(is.na(excluded_state_values_2024)))
  testthat::expect_true(all(is.na(excluded_state_values_2025)))
})

testthat::test_that("Values were computed correctly", {
  # Values computed by Hannah
  validation_df <- data.table::fread("../../data/validate/validate-value.csv")
  
  validation_merge <- validation_df[
    nptrends_full_filtered,
    value := i.value,
    on = .(metricID = metricID,
           category = category,
           subcategory = subcategory,
           vizType = vizType,
           filterType = filterType,
           filterOpt = filterOpt,
           year = year,
           splitByOpt = splitByOpt,
           splitByOpt_category = splitByOpt_category,
           responseOpt = responseOpt)
  ]

  testthat::expect_equal(validation_merge$value, 
                         validation_merge$validate_value, 
                         tolerance = 0.02)
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