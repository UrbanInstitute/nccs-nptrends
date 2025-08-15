# libraries
library(testthat)
library(data.table)
library(tidyverse)

testthat::test_that("All permutations in template present", {
  # Load in template
  root <- "../../"
  template <- data.table::fread(
    paste0(root, template_path)
  )
  nptrends_full_postprocessed <- data.table::fread(
    paste0(root, nptrends_full_postproc_path)
  )
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