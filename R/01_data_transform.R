# Data transformation script that performs computation of all summary statistics

# Libraries
library(tidyverse)
library(data.table)
library(purrr)
library(rlang)

# Helper scripts
source(here::here("R", "config.R"))
source(here::here("R", "summarize_survey_results.R"))

# Load preprocessed survey data
nptrends_full_preprocessed = data.table::fread(nptrends_full_preproc_path, na.strings = "")

# Transform survey data and calculate all the aggregates needed
nptrends_full_transformed <- purrr::imap(
  .x = metrics,
  .f = function(method, metric){
    rs <- purrr::imap(
      .x = groupby_ls,
      .f = svy_tfm,
      metric = metric,
      method = method,
      df = nptrends_full_preprocessed,
      .progress = TRUE
    ) |>
      purrr::list_rbind()
    return(rs)
  },
  .progress = "Computing weighted estimates"
) |> purrr::list_rbind()

# Save data
data.table::fwrite(nptrends_full_transformed, nptrends_full_transformed_path)
