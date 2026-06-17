# Script to create Urban data catalog entries.
#
# Reads the locally processed survey output (which already contains Year 4/5/6,
# i.e. 2024, 2025, and 2026) and writes a set of formatted Excel workbooks plus a
# Data Dictionary for manual upload to the Urban data catalog:
#   https://datacatalog.urban.org/dataset/nonprofit-trends-tracker
#
# Outputs (written to data/catalog/):
#   - State_Level_Survey_Workbook.xlsx      one sheet tab per state
#   - Subsector_Level_Survey_Workbook.xlsx  one sheet tab per nonprofit subsector
#   - <category>_Survey_Workbook.xlsx       one workbook per category, tabs = subcategories
#   - Data Dictionary.xlsx                  column definitions + workbook descriptions

# Libraries
library(data.table)

# ---- Inputs / outputs --------------------------------------------------------
# Source data: the local pipeline output. It carries the same schema the catalog
# needs (minus chartTitle, which is joined from metricsMaster, plus an extra
# vizType column that is dropped below) and already includes 2026 (Year 6).
input_data_path   <- "data/processed/nptrends_full_filtered.csv"
metricsMaster_path <- "data/validate/metricsMaster_20260526.csv"
logo_path         <- "data/assets/urban_institute_logo.jpeg"
catalog_dir       <- "data/catalog"
catalog_url       <- "https://datacatalog.urban.org/dataset/nonprofit-trends-tracker"
# Survey years present in this release (used in the data dictionary).
survey_years_label <- "2024, 2025, 2026"

if (!dir.exists(catalog_dir)) dir.create(catalog_dir, recursive = TRUE)

# Order of columns in every data workbook
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

# ---- Load and assemble the catalog table -------------------------------------
nptrends_tracker_data <- data.table::fread(input_data_path)

# The pipeline stores `value` as a reduced-precision scientific string (e.g.
# "4.7e+05", "7.9e-01") and writes suppressed/missing values as blanks. Convert
# back to numeric so the catalog shows clean numbers (470000, 0.79) and blanks
# become NA (rendered as empty cells in Excel).
nptrends_tracker_data[, value := suppressWarnings(as.numeric(value))]

# Drop the pipeline-only vizType column if present.
if ("vizType" %in% names(nptrends_tracker_data)) {
  nptrends_tracker_data[, vizType := NULL]
}

# Join the chart title for each metric from metricsMaster.
metricsMaster <- data.table::fread(metricsMaster_path)
metricsMaster <- unique(metricsMaster[, .(metricID, chartTitle)])
nptrends_tracker_data <- merge(
  nptrends_tracker_data,
  metricsMaster,
  by = "metricID",
  all.x = TRUE
)
data.table::setcolorder(nptrends_tracker_data, catalog_column_order)

# ---- Excel formatting helpers ------------------------------------------------
header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "left",
  valign = "bottom",
  border = "bottom",
  borderStyle = "medium"
)

# Logo dimensions (inches). Source image is 801x481 px (~1.665:1 aspect ratio).
logo_w_in <- 1.8
logo_h_in <- logo_w_in / (801 / 481)

# Write a named list of data.tables to one workbook, one sheet per list element,
# with a bold frozen header row and the Urban logo floated to the right of the
# table on every sheet.
write_catalog_workbook <- function(sheet_list, file) {
  wb <- openxlsx::createWorkbook()
  ncol_data <- length(catalog_column_order)
  # Readable fixed column widths (chartTitle / responseOpt / split columns are wide).
  col_widths <- c(9, 20, 42, 24, 11, 11, 7, 22, 26, 32, 13)
  for (i in seq_along(sheet_list)) {
    openxlsx::addWorksheet(wb, sheetName = names(sheet_list)[i])
    openxlsx::writeData(
      wb, sheet = i, x = sheet_list[[i]],
      startRow = 1, startCol = 1, headerStyle = header_style
    )
    openxlsx::freezePane(wb, sheet = i, firstActiveRow = 2)
    openxlsx::setColWidths(wb, sheet = i, cols = seq_len(ncol_data), widths = col_widths)
    if (file.exists(logo_path)) {
      openxlsx::insertImage(
        wb, sheet = i, file = logo_path,
        width = logo_w_in, height = logo_h_in, units = "in",
        startRow = 1, startCol = ncol_data + 2
      )
    }
  }
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}

# ---- 1. State-level workbook (one tab per state) -----------------------------
nptrends_tracker_data_state <- nptrends_tracker_data[filterType == "State"]
nptrends_tracker_data_state_ls <- split(nptrends_tracker_data_state, by = "filterOpt")
write_catalog_workbook(
  nptrends_tracker_data_state_ls,
  file = file.path(catalog_dir, "State_Level_Survey_Workbook.xlsx")
)

# ---- 2. Subsector-level workbook (one tab per subsector) ----------------------
nptrends_tracker_data_subsector <- nptrends_tracker_data[splitByOpt == "Nonprofit subsector"]
nptrends_tracker_data_subsector[, splitByOpt_category := fifelse(
  # Renamed due to xlsx tab character limits
  splitByOpt_category == "International and foreign affairs",
  "International & foreign affairs",
  splitByOpt_category
)]
nptrends_tracker_data_subsector_ls <- split(
  nptrends_tracker_data_subsector,
  by = "splitByOpt_category"
)
write_catalog_workbook(
  nptrends_tracker_data_subsector_ls,
  file = file.path(catalog_dir, "Subsector_Level_Survey_Workbook.xlsx")
)

# ---- 3. One workbook per category, tabs = subcategories ----------------------
nptrends_tracker_category_data <- split(nptrends_tracker_data, by = "category")
for (dt_name in names(nptrends_tracker_category_data)) {
  dt_ls <- split(nptrends_tracker_category_data[[dt_name]], by = "subcategory")
  # Clean subcategory names into valid, <=31-char sheet tab names.
  for (idx in seq_along(dt_ls)) {
    name <- names(dt_ls)[idx]
    newname <- gsub("Demographics: ", "", name)
    if (nchar(newname) > 31) {
      newname <- substr(newname, 1, 31)
    }
    names(dt_ls)[idx] <- newname
  }
  write_catalog_workbook(
    dt_ls,
    file = file.path(catalog_dir, sprintf("%s_Survey_Workbook.xlsx", dt_name))
  )
}

# ---- 4. Data Dictionary ------------------------------------------------------
write_data_dictionary <- function(file) {
  wb <- openxlsx::createWorkbook()

  wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  link_style <- openxlsx::createStyle(fontColour = "#0563C1", textDecoration = "underline")

  ## Sheet 1: Column Definitions
  column_definitions <- data.table(
    Variable = c(
      "Unique ID for metric",
      "Data category",
      "Data subcategory",
      "Chart title",
      "Level of geographic filtering applied to the data",
      "Specific geographic filter applied",
      "Survey year",
      "Demographic or organizational split applied to the data",
      "Specific split-by group",
      "Survey response option",
      "Metric value"
    ),
    Variable.Name = c(
      "metricID", "category", "subcategory", "chartTitle", "filterType",
      "filterOpt", "year", "splitByOpt", "splitByOpt_category", "responseOpt", "value"
    ),
    Unit = c(
      "Integer", "Text", "Text", "Text", "Text",
      "Text", "Integer", "Text", "Text", "Text", "Numeric"
    ),
    Allowed.Values = c(
      "1 to 90",
      "Fiscal health, Communities served, Programming, Sources of revenue, Workers and board members",
      "23 subcategories (e.g., Total revenue, Total expenses, Cash reserves, Geographic reach, Staff and volunteers, Individual donations, Demand for services)",
      "Descriptive titles of the charts found in the dashboard (e.g., Median total revenue nonprofits estimated receiving in)",
      "National, State, Region",
      "National; 50 US states + DC (two-letter abbreviations); 4 Census regions (Northeast, Midwest, South, West)",
      survey_years_label,
      "Blank (no split); All regions; Available states; Nonprofit subsector; Size of annual expenses; Urban or rural designation",
      "Depends on splitByOpt. Blank when no split is applied. State abbreviations for Available states; subsector names for Nonprofit subsector; expense ranges for Size of annual expenses; Rural or Urban; Census region names for All regions",
      "Survey response labels specific to each metric (e.g., Median total revenue, Increase, Decrease, Serving local areas, 0%, 1-20%)",
      "Decimal proportions (0 to 1) for percentage metrics; dollar amounts for financial metrics; counts for quantitative metrics. NA indicates suppressed or unavailable data"
    ),
    Description = c(
      "Each visualization in the dashboard displays data from a single survey metric, which is assigned a unique ID",
      "The metrics being visualized are split into 5 thematic categories",
      "Each data category can be further broken down into various subcategories. In category-level workbooks, each subcategory is a separate sheet tab",
      "The title of the dashboard chart associated with this metric. Describes what the metric measures",
      "Indicates whether data is filtered at the national, state, or regional level",
      "The specific geographic filter value. National for national-level data; a two-letter state abbreviation for state-level data; or a Census region name for region-level data",
      "The survey year in which the data was collected",
      "The demographic or organizational dimension used to disaggregate the data. Blank when data is not disaggregated",
      "The specific group within the split-by dimension. For example, if splitByOpt is Nonprofit subsector, this column contains the subsector name (e.g., Education, Health). Blank when no split is applied",
      "The label for the survey response option being measured. Each metric can have one or more response options (e.g., Increase, Decrease, No change)",
      "The numeric result for the given combination of metric, filter, split, and response option. Proportions range from 0 to 1; financial values are in US dollars; quantitative values are counts. NA indicates the data was suppressed or is unavailable"
    )
  )

  openxlsx::addWorksheet(wb, "Column Definitions")
  if (file.exists(logo_path)) {
    openxlsx::insertImage(
      wb, "Column Definitions", file = logo_path,
      width = logo_w_in, height = logo_h_in, units = "in",
      startRow = 1, startCol = 1
    )
  }
  # Link back to the dataset on the Urban data catalog.
  catalog_link <- catalog_url
  class(catalog_link) <- "hyperlink"
  openxlsx::writeData(wb, "Column Definitions", x = catalog_link, startRow = 7, startCol = 1)
  openxlsx::addStyle(wb, "Column Definitions", link_style, rows = 7, cols = 1)
  # Definitions table below the logo / link.
  defs_start <- 9
  openxlsx::writeData(
    wb, "Column Definitions", x = column_definitions,
    startRow = defs_start, startCol = 1, headerStyle = header_style
  )
  openxlsx::setColWidths(wb, "Column Definitions", cols = 1:5, widths = c(38, 20, 10, 55, 70))
  openxlsx::addStyle(
    wb, "Column Definitions", wrap_style,
    rows = (defs_start + 1):(defs_start + nrow(column_definitions)),
    cols = 1:5, gridExpand = TRUE
  )

  ## Sheet 2: Workbook Descriptions
  shared_columns <- paste(catalog_column_order, collapse = ", ")
  workbook_descriptions <- data.table(
    Workbook = c(
      "State_Level_Survey_Workbook.xlsx",
      "Subsector_Level_Survey_Workbook.xlsx",
      "Fiscal health_Survey_Workbook.xlsx",
      "Communities served_Survey_Workbook.xlsx",
      "Programming_Survey_Workbook.xlsx",
      "Sources of revenue_Survey_Workbook.xlsx",
      "Workers and board members_Survey_Workbook.xlsx"
    ),
    Description = c(
      "Survey results filtered by US state. Contains one sheet tab per state (51 tabs: 50 states + DC). Each tab contains all metrics for that state across all years, subcategories, and response options.",
      "Survey results disaggregated by nonprofit subsector. Contains one sheet tab per subsector (7 tabs: Arts, culture, and humanities; Education; Environment and animals; Health; Human services; International & foreign affairs; Public and societal benefit).",
      "Survey results for the Fiscal health category. Contains one sheet tab per subcategory: Total revenue, Total expenses, Cash reserves, Fundraising.",
      "Survey results for the Communities served category. Contains one sheet tab per subcategory: Geographic reach, Additional populations, Gender and sexuality, Race and ethnicity, Age.",
      "Survey results for the Programming category. Contains one sheet tab per subcategory: Demand for services, Service provision.",
      "Survey results for the Sources of revenue category. Contains one sheet tab per subcategory: Individual donations, Donor-advised funds, Private foundations, Corporate foundations and giving programs, Government, Earned revenue, Community foundations.",
      "Survey results for the Workers and board members category. Contains one sheet tab per subcategory: Paid staff demographics, Staff and volunteers, Board member demographics, CEO demographics, Board chair demographics."
    ),
    Sheet.Tabs = c(
      "AL, AK, AZ, AR, CA, CO, CT, DC, DE, FL, GA, HI, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY",
      "Arts, culture, and humanities; Education; Environment and animals; Health; Human services; International & foreign affairs; Public and societal benefit",
      "Total revenue; Total expenses; Cash reserves; Fundraising",
      "Geographic reach; Additional populations; Gender and sexuality; Race and ethnicity; Age",
      "Demand for services; Service provision",
      "Individual donations; Donor-advised funds; Private foundations; Corporate foundations and giving programs; Government; Earned revenue; Community foundations",
      "Paid staff demographics; Staff and volunteers; Board member demographics; CEO demographics; Board chair demographics"
    ),
    Columns = rep(shared_columns, 7)
  )

  openxlsx::addWorksheet(wb, "Workbook Descriptions")
  openxlsx::writeData(
    wb, "Workbook Descriptions", x = workbook_descriptions,
    startRow = 1, startCol = 1, headerStyle = header_style
  )
  openxlsx::setColWidths(wb, "Workbook Descriptions", cols = 1:4, widths = c(42, 80, 60, 60))
  openxlsx::addStyle(
    wb, "Workbook Descriptions", wrap_style,
    rows = 2:(nrow(workbook_descriptions) + 1), cols = 1:4, gridExpand = TRUE
  )
  openxlsx::freezePane(wb, "Workbook Descriptions", firstActiveRow = 2)

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}

write_data_dictionary(file.path(catalog_dir, "Data Dictionary.xlsx"))
