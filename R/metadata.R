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
  "Gov't", "PercentGov",
  "Earned Revenue", "PercentEarned",
  "Women", "PercentDem_Women_Board",
  "Regular volunteers", "Staff_RegVlntr",
  "Episodic volunteers", "Staff_EpsdVlntr",
  "Women", "PercentDem_Women_Staff",
  "LGBTQ+", "PercentDem_LGBTQ_Board",
  "Disabled", "PercentDem_Disabled_Board",
  "Under 35 years old", "PercentDem_Young_Board",
  "Past or current recipient of nonprofits' services", "PercentDem_ReceivedServices_Board",
  "Person of color", "PercentDem_POC_Staff",
  "LGBTQ+", "PercentDem_LGBTQ_Staff",
  "Disabled", "PercentDem_Disabled_Staff",
  "Under 35 years old", "PercentDem_Young_Staff",
  "Past or current recipient of nonprofits' services", "PercentDem_ReceivedServices_Staff",
  "Number of board members", "Staff_Boardmmbr",
  "Person of color", "PercentDem_POC_Board"
)

# Template containing desired outputs, excluding values for survey metrics
# Provided by data-visualization developer Jeff MacIness [jeff@decimalpointstudio.com]
template <- data.table::fread(template_path)

# Dataset containing all desired permutations of filters in dashboard
combinations_validate_df <- template |>
  dplyr::mutate(
    splitByOpt_category = dplyr::case_when(
      splitByOpt_category == "<$100,000" ~ "< $100,000",
      splitByOpt_category == "$100,000-$499,999" ~ "$100,000 - $499,999",
      splitByOpt_category == "$500,000-$999,999" ~ "$500,000 - $999,999",
      splitByOpt_category == "$1 million-$9,999,999" ~ "$1 million - $9,999,999",
      .default = splitByOpt_category
    ),
    responseOpt = tolower(gsub("’", "'", responseOpt))
  ) |>
  dplyr::select(filterType,
                filterOpt,
                splitByOpt,
                splitByOpt_category,
                responseOpt,) |>
  unique()
data.table::fwrite(combinations_validate_df, combinations_validate_path)

# Metadata to merge with postprocessed data to resemble data template
template_metadata <- data.table::fread(template_metadata_path) |>
  dplyr::select(metricID,
                theme,
                category,
                dataVizType,
                metricname)