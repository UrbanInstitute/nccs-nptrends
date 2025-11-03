# Postprocessing script, making transformed data adhere to Jeff's template

# Libraries
library(stringr)

# Helper scripts
source(here::here("R", "config.R"))
source(here::here("R", "metadata.R"))

# Load in transformed survey data
nptrends_full_transformed <- data.table::fread(nptrends_full_transformed_path, na.strings = "")

# Postprocess data
nptrends_full_formatted <- nptrends_full_transformed |>
  dplyr::filter(responseOpt != "",
                is.na(splitByOpt_category) |
                  splitByOpt_category != "Religion",
                ! responseOpt %in% c(
                  "Did not serve local",
                  "Did not serve state-wide",
                  "Did not serve multi state",
                  "Did not serve national",
                  "Did not serve international",
                  "Did not serve families and individuals in poverty",
                  "Did not serve individuals with physical or cognitive disabilities",
                  "Did not serve veterans",
                  "Did not serve LGBTQ+",
                  "Did not serve foreign born populations",
                  "Did not serve Latinx/Hispanic/Hispanic Origin",
                  "Did not serve Black/African American",
                  "Did not serve indigenous/Native American/Native Alaskan",
                  "Did not serve Asian/Native Hawaiian/other Pacific Islander",
                  "Did not serve men/boys",
                  "Did not serve women/girls",
                  "Did not serve individuals of non-binary gender",
                  "Did not serve children and youth",
                  "Did not serve young adults",
                  "Did not serve adults",
                  "Did not serve seniors",
                  "Not experiencing",
                  "Did not meet demand",
                  "Did not receive",
                  "Not person of color",
                  "Not under 35 years old",
                  "Not disabled",
                  "Not individual of non-binary gender",
                  "Not LGBTQ+",
                  "Not a man",
                  "Not a woman",
                  "Did not draw on cash reserves"
                )) |>
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
    responseOpt_lookup = gsub("'", "’", responseOpt_lookup)
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
    value,
    num_responses
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

data.table::fwrite(nptrends_full_formatted, nptrends_full_formatted_path)

# Filtering to remove metrics where at least one subcategory has under 25 weighted responses
nptrends_minresponse <- nptrends_full_formatted |>
  dplyr::filter(splitByOpt %in% c("Rural/Urban", "Size", "Subsector")) |>
  dplyr::group_by(
    year, 
    metricID, 
    filterType,
    filterOpt,
    splitByOpt
  ) |>
  dplyr::mutate(
    min_sum = min(num_responses, na.rm = TRUE),
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(value = ifelse(min_sum < 25, NA, value)) |>
  dplyr::select(-min_sum)

nptrends_totresponse <- nptrends_full_formatted |>
  dplyr::filter(!splitByOpt %in% c("Rural/Urban", "Size", "Subsector")) |>
  dplyr::group_by(year,
                  metricID,
                  filterType,
                  filterOpt,
                  splitByOpt,
                  splitByOpt_category) |>
  dplyr::mutate(
    group_sum = sum(num_responses, na.rm = TRUE),
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(value = ifelse(group_sum < 25, NA, value)) |>
  dplyr::select(-group_sum)

nptrends_full_responseFiltered <- dplyr::bind_rows(nptrends_minresponse, nptrends_totresponse) |>
  dplyr::select(!num_responses) |>
  dplyr::filter(
    (
      year == 2024 &
        !filterOpt %in% states_to_exclude[["2024"]] &
        !splitByOpt_category %in% states_to_exclude[["2024"]]
    ) | (
      year == 2025 &
        !filterOpt %in% states_to_exclude[["2025"]] &
        !splitByOpt_category %in% states_to_exclude[["2025"]]
    )
  )


# Save output dataset
data.table::fwrite(nptrends_full_responseFiltered, nptrends_full_filtered_path)

# Perform validation checks
testthat::test_dir("tests/testthat")