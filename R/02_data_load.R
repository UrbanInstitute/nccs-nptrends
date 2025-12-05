# Postprocessing script, making transformed data adhere to Jeff's template

# Libraries
library(stringr)

# Helper scripts
source(here::here("R", "config.R"))
source(here::here("R", "metadata.R"))

# Load in transformed survey data
nptrends_full_transformed <- data.table::fread(nptrends_full_transformed_path)

# Postprocess data
nptrends_full_formatted <- nptrends_full_transformed |>
  dplyr::mutate(
    value = format(value, digits = 2, scientific = TRUE),
    filterType = dplyr::case_when(
      filterType == "National" ~ "National",
      filterType == "CensusRegion4" ~ "Region",
      filterType == "state" ~ "State",
      .default = filterType
    ),
    splitByOpt = dplyr::case_when(
      splitByOpt == "National" ~ "National",
      splitByOpt == "CensusRegion4" ~ "All regions",
      splitByOpt == "state" ~ "Available states",
      splitByOpt == "SizeStrata" ~ "Size of annual expenses",
      splitByOpt == "census_urban_area" ~ "Urban or rural designation",
      splitByOpt == "Subsector" ~ "Nonprofit subsector",
        .default = splitByOpt
      ),
    splitByOpt = dplyr::case_when(
      filterType == "National" &
        filterOpt == "National" & splitByOpt == "National" ~ "",
      filterType == "Region" & splitByOpt == "All regions" ~ "",
      filterType == "State" & splitByOpt == "Available states" ~ "",
      .default = splitByOpt
    )
  ) |>
  dplyr::filter(splitByOpt_category != "Religion") |>
  tidylog::left_join(responseOpt_lookup, by = "metricname") |>
  dplyr::mutate(
    responseOpt_lookup = dplyr::coalesce(responseOpt_lookup, responseOpt),
    responseOpt_lookup = gsub("'", "’", responseOpt_lookup)
  ) |>
  tidylog::left_join(template_metadata, by = "metricname") |>
  dplyr::select(
    metricID,
    category,
    subcategory,
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
  dplyr::rename(responseOpt = responseOpt_lookup, 
                vizType = dataVizType)

data.table::fwrite(nptrends_full_formatted, nptrends_full_formatted_path)
# Check that recoded groupby/disaggregation variables match template
testthat::test_file("tests/testthat/test-template_permutations.R")

# Exclude data with minimum number of responses

# Exclusion criteria 1: If one subcategory  has less than 25 responses, exclude the entire disaggregation. Only for Size or Urban/Rural designation.
nptrends_minresponse <- nptrends_full_formatted |>
  dplyr::filter(splitByOpt %in% groupbys_to_exclude) |>
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

# Exclusion criteria 2: Only exclude subcategories with less than 25 responses.
nptrends_totresponse <- nptrends_full_formatted |>
  dplyr::filter(!splitByOpt %in% groupbys_to_exclude) |>
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

# Combine and exclude data belonging to specific states in specific years
nptrends_full_responseFiltered <- data.table::rbindlist(
  list(nptrends_minresponse, nptrends_totresponse)
) |>
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

# Remove data belonging to negative responseOpts
nptrends_full_responseOptFiltered <- nptrends_full_responseFiltered[! responseOpt %in% negative_responseOpts, ]


# Merge with template
template_merge <- template[, value := NULL]

nptrends_full_formatted_merged <- template_merge[
  nptrends_full_responseOptFiltered,
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

# Save output dataset
data.table::fwrite(nptrends_full_formatted_merged, nptrends_full_filtered_path)

# Check that computed values align with HMartin's[HMartin@urban.org] values
testthat::test_file("tests/testthat/test-data-validation.R")