# Unit test to ensure that all filterOpt, filterType, splitbyOpt, splitbyopt_category permutations are correct

# libraries
library(testthat)
library(data.table)
library(tidyverse)

# Datasets
root <- "../../"
nptrends_full_formatted <- data.table::fread(
  paste0(root, nptrends_full_formatted_path),
  colClasses = list(character = c("responseOpt"))
)
template <- data.table::fread(paste0(root, template_path),
                              colClasses = list(character = c("responseOpt")))

testthat::test_that("filterType values are correct", {
  
  myfilterType <- unique(nptrends_full_formatted$filterType)
  templatefilterType <- unique(template$filterType)

  testthat::expect_equal(myfilterType, templatefilterType)
})

testthat::test_that("filterOpt values are correct", {
  
  myfilterOpt <- unique(nptrends_full_formatted$filterOpt) |> sort()
  templatefilterOpt <- unique(template$filterOpt) |> sort()
  
  testthat::expect_equal(myfilterOpt, templatefilterOpt)
})

testthat::test_that("splitByOpt values are correct", {
  
  mysplitByOpt <- unique(nptrends_full_formatted$splitByOpt) |> sort()
  templatesplitByOpt <- unique(template$splitByOpt) |> sort()
  
  testthat::expect_equal(mysplitByOpt, templatesplitByOpt)
})

testthat::test_that("splitByOpt_category values are correct", {
  
  mysplitByOpt_category <- 
    unique(nptrends_full_formatted$splitByOpt_category) |> sort()
  templatesplitByOpt_category <- 
    unique(template$splitByOpt_category) |> sort()
  
  testthat::expect_equal(mysplitByOpt_category, 
                         templatesplitByOpt_category)
})

testthat::test_that("Responses are named correctly", {
  templateids <- template$responseOpt |> unique() |> sort()
  myids <- nptrends_full_formatted$responseOpt |> unique() |> sort()
  testthat::expect_contains(myids, templateids)
})

# We expect a subset because the final dataset does not have the binary == 0 values (e.g. has "Serving seniors" but not "Not serving seniors")
testthat::test_that("ResponseOpt values are formatted correctly", {
  myresponseOpt <- nptrends_full_formatted |>
    dplyr::pull(responseOpt)
  templateresponseOpt <- template |>
    dplyr::pull(responseOpt)
  testthat::expect_contains(myresponseOpt, templateresponseOpt)
})