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
  excluded_state_values_2026 <- extract_valid_states(nptrends_full_filtered, 2026)


  testthat::expect_true(all(is.na(excluded_state_values_2024)))
  testthat::expect_true(all(is.na(excluded_state_values_2025)))
  testthat::expect_true(all(is.na(excluded_state_values_2026)))
})

testthat::test_that("Year 6 (2026) rows are produced and match the template keys", {
  yrs_out <- sort(unique(nptrends_full_filtered$year))
  testthat::expect_true(2026 %in% yrs_out)

  key_cols <- c("metricID", "category", "subcategory", "vizType",
                "filterType", "filterOpt", "splitByOpt",
                "splitByOpt_category", "responseOpt")
  tpl_2026_keys <- unique(template[year == 2026, ..key_cols])

  # Every output row for 2026 must correspond to a template key.
  out_2026 <- unique(nptrends_full_filtered[year == 2026, ..key_cols])
  joined <- data.table::merge.data.table(out_2026, tpl_2026_keys,
                                         by = key_cols, all.x = TRUE)
  testthat::expect_equal(nrow(joined), nrow(out_2026))
})

testthat::test_that("ProgDem metrics have no Y4/Y5 values (per 2026-05 dictionary)", {
  drop_ids <- metricID_lookup$metricID[metricID_lookup$metricname %in% metrics_drop_y4_y5]
  affected <- nptrends_full_filtered[metricID %in% drop_ids & year %in% c(2024, 2025)]
  testthat::expect_true(all(is.na(affected$value)))
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