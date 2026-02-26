# Script to create data catalog entry

# Libraries
library(data.table)

# Helper Scripts

# Load survey dashboard data from Box
nptrends_tracker_data_url <- "https://urbanorg.box.com/shared/static/adozu2flmjfppsllyh7r7iwu9qqxl6ac.csv"
nptrends_tracker_data <- data.table::fread(nptrends_tracker_data_url)
# Load metrics master
metricsMaster_path <- "data/validate/metricsMaster_20260219.csv"
metricsMaster <- data.table::fread(metricsMaster_path)
# Filter metricsMaster to only include chart title
metricsMaster <- metricsMaster |>
  dplyr::select(metricID, chartTitle)
# Order of columns
catalog_column_order <- c(
  "metricID",
  "category",
  "chartTitle",
  "subcategory",
  "filterType",
  "filterOpt",
  "year",
  "splitByOpt",
  "splitByOpt_category",
  "responseOpt",
  "value"
)
# Vector containing all subsector categories in dashboard
subsectors <- c(
  "Arts, culture, and humanities",
  "Education",
  "Environment and animals",
  "Health",
  "Human services",
  "International and foreign affairs",
  "Public and societal benefit"
)

# Join survey data with metricsMaster
nptrends_tracker_data <- nptrends_tracker_data |>
  dplyr::select(
    metricID, 
    category, 
    subcategory, 
    filterType,
    filterOpt,
    year,
    splitByOpt,
    splitByOpt_category,
    responseOpt,
    value
  ) |>
  tidylog::left_join(metricsMaster)
setcolorder(nptrends_tracker_data,  catalog_column_order)

# Data catalog entry split by state
nptrends_tracker_data_state <- nptrends_tracker_data |>
  dplyr::filter(filterType == "State")
nptrends_tracker_data_state_ls <- split(nptrends_tracker_data_state, 
                                        by = "filterOpt")
openxlsx::write.xlsx(nptrends_tracker_data_state_ls, 
                     file = "data/State_Level_Survey_Workbook.xlsx")

# Data catalog entry split by subsector
nptrends_tracker_data_subsector <- nptrends_tracker_data |>
  dplyr::filter(splitByOpt  == "Nonprofit subsector") |>
  dplyr::mutate(
    splitByOpt_category = dplyr::case_match(
      # Renamed due xlsx tab character limits
      splitByOpt_category,
      "International and foreign affairs" ~ "International & foreign affairs",
      .default = splitByOpt_category
    )
  )
nptrends_tracker_data_subsector_ls <- split(nptrends_tracker_data_subsector, 
                                            by = "splitByOpt_category")
openxlsx::write.xlsx(nptrends_tracker_data_subsector_ls, 
                     file = "data/Subsector_Level_Survey_Workbook.xlsx")

# Split by category and save each subcategory to a separate workbook
nptrends_tracker_category_data <- split(
  nptrends_tracker_data,
  by = "category"
)
# Loop to save each cataegory as a separate work book with tabs for subcategories
purrr::imap(
  nptrends_tracker_category_data,
  .f = function(dt, dt_name) {
    dt_ls <- split(dt, by = "subcategory")
    for (idx in 1:length(dt_ls)){
      name <- names(dt_ls)[idx]
      newname <- gsub("Demographics: ", "", name)
      if (nchar(name) > 31){
        newname <- substr(newname, 1, 31)
      }
      names(dt_ls)[idx] <- newname
    }
    openxlsx::write.xlsx(dt_ls, 
                         file = sprintf("data/%s_Survey_Workbook.xlsx", dt_name))
  },
  .progress = TRUE
)
