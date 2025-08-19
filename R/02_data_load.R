# Postprocessing script, making transformed data adhere to Jeff's template

# Libraries
library(stringr)

# Helper scripts
source(here::here("R", "config.R"))
source(here::here("R", "metadata.R"))

# Load in transformed survey data
nptrends_full_transformed <- data.table::fread(nptrends_full_transformed_path, na.strings = "")

# Postprocess data
nptrends_full_postprocessed <- nptrends_full_transformed |>
  dplyr::filter(responseOpt != "",
                is.na(splitByOpt_category) |
                  splitByOpt_category != "Religion") |>
  dplyr::mutate(
    value = format(value, digits = 2, scientific = TRUE),
    dplyr::across(
      .cols = dplyr::all_of(c("filterType", "splitByOpt")),
      .fns = ~ dplyr::case_when(
        .x == "CensusRegion4" ~ "Region",
        .x == "state" ~ "State",
        .x == "SizeStrata" ~ "Size",
        .x == "census_urban_area" ~ "Rural/Urban",
        .x == "Subsector" ~ "Sector",
        .default = .x
      )
    ),
    splitByOpt = dplyr::case_when(
      filterType == "National" &
        filterOpt == "National" & splitByOpt == "National" ~ "",
      filterType == "Region" & splitByOpt == "Region" ~ "",
      filterType == "State" & splitByOpt == "State" ~ "",
      .default = splitByOpt
    ),
    splitByOpt_category = dplyr::case_when(is.na(splitByOpt_category) ~ "", .default = splitByOpt_category)
  ) |>
  dplyr::filter(!(splitByOpt ==  "Rural/Urban" &
                    splitByOpt_category == "")) |>
  tidylog::left_join(responseOpt_lookup, by = "metricname") |>
  dplyr::mutate(
    responseOpt_lookup = dplyr::coalesce(responseOpt_lookup, responseOpt),
    value = ifelse(num_responses < 25, NA, value)
  ) |>
  tidylog::left_join(template_metadata, by = "metricname") |>
  dplyr::select(
    metricID,
    theme,
    category,
    dataVizType,
    filterType,
    filterOpt,
    year,
    splitByOpt,
    splitByOpt_category,
    responseOpt_lookup,
    value
  ) |>
  dplyr::rename(responseOpt = responseOpt_lookup, vizType = dataVizType) |>
  tidyr::complete(
    tidyr::nesting(metricID, theme, category, vizType, year, responseOpt),
    tidyr::nesting(filterType, filterOpt),
    tidyr::nesting(splitByOpt, splitByOpt_category)
  ) |>
  dplyr::semi_join(
    combinations_validate_df,
    by = c(
      "filterType",
      "filterOpt",
      "splitByOpt",
      "splitByOpt_category"
    )
  )

# Save output dataset
data.table::fwrite(nptrends_full_postprocessed, nptrends_full_postproc_path)

# Perform validation checks
testthat::test_dir("tests/testthat")

# TODO:
# Check that values are correct
# mentiopn that some responseOpt are numbers
# mention that variables have been removed
# tell Hannah that I am not doing the filtering out. and tell Jeff about the recoding
# validation checks for some manual calculations of each type
# let jeff know I made the subgroups equal as well. e.g. AK, Size < $100,000
# ask about years