# Internal data objects used for data processing and validation

# metricsMaster table for validation
metricsMaster <- gsheet::gsheet2tbl(metricsMaster_gsheet_url)

# lookup table mapping variable names in the survey to metricID values
metricID_lookup <- tibble::tribble(
  ~metricID, ~metricname,
  1, "TotRev_clean",
  2, "TotExp",
  3, "Reserves_Est",
  4, "FinanceChng_Reserves",
  5, "FinanceChng_TotExp",
  6, "FinanceChng_Salaries",
  7, "FinanceChng_Benefits",
  8, "FinanceChng_TotRent",
  9, "FinanceChng_TotTech",
  10, "GeoAreas_ServedLocal",
  11, "GeoAreas_State",
  12, "GeoAreas_Servedmultistate",
  13, "GeoAreas_National",
  14, "GeoAreas_International",
  15, "ProgDem_BelowFPL",
  16, "ProgDem_Disabled",
  17, "ProgDem_Veterans",
  18, "ProgDem_LGBTQ",
  19, "ProgDem_Foreign",
  20, "ProgDem_Latinx",
  21, "ProgDem_Black",
  22, "ProgDem_Indigenous",
  23, "ProgDem_Asian",
  24, "ProgDem_Men",
  25, "ProgDem_Women",
  26, "ProgDem_Nonbinary",
  27, "ProgDem_Children",
  28, "ProgDem_YoungAdults",
  29, "ProgDem_Adults",
  30, "ProgDem_Elders",
  31, "PplSrv_MeetDemand",
  32, "PrgSrvc_Suspend",
  33, "PrgSrvc_Amt_Srvc",
  34, "PrgSrvc_Amt_Num",
  35, "Dmnd_NxtYear",
  36, "PercentDon_Tot",
  37, "FndRaise_DnrBlw250_ratio",
  38, "FndRaise_DnrAbv250_ratio",
  39, "PercentDAF_Tot",
  40, "PercentPriv_Tot",
  41, "PercentCorp_Tot",
  42, "PercentGov",
  43, "PercentEarned",
  44, "FndRaise_DAF_Rcv",
  45, "FndRaise_PFGrnt_Rcv",
  46, "FndRaise_Corp_Found_Grnt_Rcv",
  47, "FndRaise_CFGrnt_Rcv",
  50, "FndRaise_DAF_Grnt_Chng",
  51, "FndRaise_Priv_Grnt_Chng",
  52, "FndRaise_Corp_Grnt_Chng",
  53, "PrgSrvc_Amt_Fee",
  54, "FndRaise_TotExp",
  55, "FndRaise_TotDigAppeal",
  56, "FndRaise_TotDirMail",
  57, "FndRaise_TotInPersEvent",
  58, "FndRaise_TotVirtEvent",
  59, "PercentDem_Women_Staff",
  60, "Staff_RegVlntr",
  61, "Staff_EpsdVlntr",
  62, "PercentDem_Women_Board",
  63, "PercentDem_LGBTQ_Board",
  64, "PercentDem_Disabled_Board",
  65, "PercentDem_Young_Board",
  66, "PercentDem_ReceivedServices_Board",
  67, "PercentDem_POC_Staff",
  68, "PercentDem_LGBTQ_Staff",
  69, "PercentDem_Disabled_Staff",
  70, "PercentDem_Young_Staff",
  71, "PercentDem_ReceivedServices_Staff",
  72, "Staff_Boardmmbr",
  73, "PercentDem_POC_Board",
  74, "CEOrace_POC",
  75, "Dem_CEO_LGBTQ",
  76, "Dem_CEO_Disabled",
  77, "Dem_CEO_Under35",
  78, "BChair_POC",
  79, "Dem_BChair_LGBTQ",
  80, "Dem_BChair_Disabled",
  81, "Dem_BChair_Under35",
  82, "Staff_Fulltime",
  83, "Staff_Parttime",
  84, "CEOgender_Man",
  85, "CEOgender_Woman",
  86, "CEOgender_NB",
  87, "BChairgender_Man",
  88, "BChairgender_Woman",
  89, "BChairgender_NB"
)

# lookup table to recode responseOpt for continuous variables
responseOpt_lookup <- tibble::tribble(
  ~responseOpt_lookup, ~metricname,
  "Total revenue", "TotRev_clean",
  "Total expenses", "TotExp",
  "Cash reserves", "Reserves_Est",
  "Individual donations", "PercentDon_Tot",
  "<$250", "FndRaise_DnrBlw250_ratio",
  ">=$250", "FndRaise_DnrAbv250_ratio",
  "Donor advised funds", "PercentDAF_Tot",
  "Private foundations", "PercentPriv_Tot",
  "Corporate foundations or giving programs", "PercentCorp_Tot",
  "Government", "PercentGov",
  "Earned Revenue", "PercentEarned",
  "Regular volunteers", "Staff_RegVlntr",
  "Episodic volunteers", "Staff_EpsdVlntr",
  "Number of board members", "Staff_Boardmmbr"
)

# Template containing desired outputs, excluding values for survey metrics used for validation
# Provided by data-visualization developer Jeff MacIness [jeff@decimalpointstudio.com]
template <- data.table::fread(template_path)

# Dataset containing all desired permutations of filters in dashboard for validation
combinations_validate_df <- template |>
  dplyr::mutate(
    splitByOpt_category = dplyr::case_when(
      splitByOpt_category == "<$100,000" ~ "< $100,000",
      splitByOpt_category == "$100,000-$499,999" ~ "$100,000 - $499,999",
      splitByOpt_category == "$500,000-$999,999" ~ "$500,000 - $999,999",
      splitByOpt_category == "$1 million-$9,999,999" ~ "$1 million - $9,999,999",
      .default = splitByOpt_category
    )
  ) |>
  dplyr::select(filterType,
                filterOpt,
                splitByOpt,
                splitByOpt_category,
                responseOpt,) |>
  unique()
data.table::fwrite(combinations_validate_df, combinations_validate_path)

# Metadata to merge with postprocessed data to resemble data template
template_metadata <- metricsMaster |>
  tidylog::left_join(metricID_lookup, by = "metricID") |>
  dplyr::mutate(metricID = as.integer(metricID)) |>
  dplyr::select(metricID,
                theme,
                category,
                dataVizType,
                metricname) |>
  as.data.frame()