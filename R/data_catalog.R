# Script to create data catalog entry

library(data.table)

nptrends_tracker_data_url <- "https://urbanorg.box.com/shared/static/2qnki38kinhkxvzsktgnbmz6f73a6gue.csv"

nptrends_tracker_data <- data.table::fread(nptrends_tracker_data_url)

metricsMaster <- metricsMaster |>
  dplyr::select(metricID, chartTitle)


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
  dplyr::left_join(metricsMaster)


nptrends_tracker_data_state <- nptrends_tracker_data |>
  dplyr::filter(filterType == "State")

setcolorder(nptrends_tracker_data_state, 
            c(
              "metricID",
              "category",
              "subcategory",
              "chartTitle",
              "year",
              "filterType",
              "filterOpt",
              "splitByOpt",
              "splitByOpt_category",
              "responseOpt",
              "value"
            ))

nptrends_tracker_data_state_ls <- split(nptrends_tracker_data_state, 
                                        by = "filterOpt")

openxlsx::write.xlsx(nptrends_tracker_data_state_ls, 
                     file = "data/State_Level_Survey_Workbook.xlsx")


nptrends_tracker_data_subsector <- nptrends_tracker_data |>
  dplyr::filter(splitByOpt == "Nonprofit subsector")


nptrends_tracker_data_subsector_ls <- split(nptrends_tracker_data_subsector, 
                                        by = "splitByOpt_category")

names(nptrends_tracker_data_subsector_ls)[6] <- "International & foreign affairs"


openxlsx::write.xlsx(nptrends_tracker_data_subsector_ls, 
                     file = "data/Subsector_Level_Survey_Workbook.xlsx")

setcolorder(
  nptrends_tracker_data,
  c(
    "metricID",
    "category",
    "subcategory",
    "chartTitle",
    "year",
    "filterType",
    "filterOpt",
    "splitByOpt",
    "splitByOpt_category",
    "responseOpt",
    "value"
  )
)

nptrends_tracker_category_data <- split(
  nptrends_tracker_data,
  by = "category"
)

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
  }
)
