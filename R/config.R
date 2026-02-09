# Params for NP Trends survey analysis
library(tibble)

# URLS
# Template URL: https://observablehq.com/@jeffmacinnes/urban-nonprofit-donor-trends

# Path to files
nptrends_y5_id_path <-   "Y:/CNP/Generosity Commission/Year 5/Restricted Dataset/Identifiable/RESTRICTED_Y5_Identifiable_Vars.csv"
nptrends_y4_id_path <-   "Y:/CNP/Generosity Commission/Year 4/Restricted Dataset/Identifiable/RESTRICTED_Y4_Identifiable_Vars.csv"
nptrends_y4_raw_path <- "Y:/CNP/Generosity Commission/Year 4/Restricted Dataset/RESTRICTED_Y4_w_DK_NA.csv"
nptrends_y5_raw_path <- "Y:/CNP/Generosity Commission/Year 5/Restricted Dataset/RESTRICTED_Y5.csv"
nptrends_full_preproc_path <- "data/intermediate/nptrends_full_preprocessed.csv"
nptrends_full_transformed_path <- "data/intermediate/nptrends_full_transformed.csv"
nptrends_full_formatted_path <- "data/processed/nptrends_full_formatted.csv"
nptrends_full_filtered_path <- "data/processed/nptrends_full_filtered.csv"

template_path <- "data/validate/dataTemplate_20251205.csv"
metricsMaster_gsheet_url <- "https://docs.google.com/spreadsheets/d/1HMoQymn4F6q0qOqTX9njTLujYwHFXXtSGnhgR0_Ohjk/edit?gid=0#gid=0"

# Variable names needed for analysis
survey_analysis_vars <- c(
  "SizeStrata",
  "ntmaj12",
  "census_urban_area",
  "CensusRegion4",
  "state",
  "State",
  "year4wt",
  "stateweight",
  "year5wt",
  "TotRev_clean",
  "FinanceChng_Reserves",
  "FinanceChng_TotExp",
  "FinanceChng_Salaries",
  "FinanceChng_Benefits",
  "FinanceChng_TotRent",
  "FinanceChng_TotTech",
  "GeoAreas_Local",
  "GeoAreas_MultipleLocal",
  "GeoAreas_RegionalWithin",
  "GeoAreas_State",
  "GeoAreas_RegionalAcross",
  "GeoAreas_MultipleState",
  "GeoAreas_National",
  "GeoAreas_International",
  "ProgDem_BelowFPL",
  "ProgDem_Disabled",
  "ProgDem_Veterans",
  "ProgDem_LGBTQ",
  "ProgDem_Foreign",
  "ProgDem_Latinx",
  "ProgDem_Black",
  "ProgDem_Indigenous",
  "ProgDem_Asian",
  "ProgDem_Men",
  "ProgDem_Women",
  "ProgDem_Nonbinary",
  "ProgDem_Children",
  "ProgDem_YoungAdults",
  "ProgDem_Adults",
  "ProgDem_Elders",
  "PplSrv_NumServed",
  "PplSrv_NumServed_NA_X",
  "PplSrv_NumWait",
  "PplSrv_NumWait_NA_X",
  "PrgSrvc_Suspend",
  "PrgSrvc_Amt_Srvc",
  "PrgSrvc_Amt_Num",
  "Dmnd_NxtYear",
  "FndRaise_DnrBlw250",
  "FndRaise_DnrAbv250",
  "FndRaise_DAF_Rcv",
  "FndRaise_PFGrnt_Rcv",
  "FndRaise_Corp_Found_Grnt_Rcv",
  "FndRaise_CFGrnt_Rcv",
  "PrgSrvc_Amt_Fee",
  "FndRaise_TotExp",
  "FndRaise_TotDigAppeal",
  "FndRaise_TotDirMail",
  "FndRaise_TotVirtEvent",
  "FndRaise_TotInPersEvent",
  "PercentDem_Women_Staff",
  "Staff_RegVlntr_2023",
  "Staff_RegVlntr_NA",
  "Staff_EpsdVltnr_2023",
  "Staff_EpsdVltnr_NA",
  "PercentDem_Women_Board",
  "PercentDem_LGBTQ_Board",
  "PercentDem_Disabled_Board",
  "PercentDem_Young_Board",
  "PercentDem_ReceivedServices_Board",
  "PercentDem_POC_Staff",
  "PercentDem_LGBTQ_Staff",
  "PercentDem_Disabled_Staff",
  "PercentDem_Young_Staff",
  "PercentDem_ReceivedServices_Staff",
  "Staff_Boardmmbr_2023",
  "Staff_Boardmmbr_NA",
  "PercentDem_POC_Board",
  "CEOrace",
  "Dem_CEO_LGBTQ",
  "Dem_CEO_Disabled",
  "Dem_CEO_Age",
  "BChairrace",
  "Dem_BChair_LGBTQ",
  "Dem_BChair_Disabled",
  "Dem_BChair_Age",
  "Staff_Fulltime_NA",
  "Staff_Parttime_2023",
  "Staff_Parttime_NA",
  "CEOgender_Man",
  "CEOgender_Woman",
  "CEOgender_NB",
  "BChairgender_Man",
  "BChairgender_Woman",
  "Staff_Fulltime_2023",
  "BChairgender_NB",
  "TotExp",
  "Reserves_Est",
  "Reserves_NA_X",
  "PercentDon_Tot",
  "PercentDAF_Tot",
  "PercentPriv_Tot",
  "PercentCorp_Tot",
  "PercentGov",
  "PercentEarned",
  "FndRaise_LocGvtGrnt_Rcv",
  "FndRaise_StateGvtGrnt_Rcv",
  "FndRaise_FedGvtGrnt_Rcv",
  "FndRaise_LocGvtLn_Rcv",
  "FndRaise_StateGvtLn_Rcv",
  "FndRaise_FedGvtLn_Rcv",
  "FndRaise_LocGvtCntrct_Rcv",
  "FndRaise_StateGvtCntrct_Rcv",
  "FndRaise_FedGvtCntrct_Rcv",
  "FndRaise_FeeOth_Rcv",
  "FndRaise_FeeMed_Rcv",
  "FndRaise_Tribal_Rcv",
  "FndRaise_GovOth_Rcv",
  "FndRaise_DAF_Grnt_Chng",
  "FndRaise_Priv_Grnt_Chng",
  "FndRaise_Corp_Grnt_Chng",
  "Staff_RegVlntr_2024",
  "Staff_EpsdVltnr_2024",
  "Staff_Boardmmbr_2024",
  "Staff_Fulltime_2024",
  "Staff_Parttime_2024",
  "FndRaise_Cashbelow250_Chng",
  "FndRaise_Cashabove250_Chng"
)

# Questions with 5 possible answers: Significant decrease, decrease, stay the same, increase, significant increase
multi_select_cols <- c(
  "FinanceChng_TotExp",
  "FinanceChng_Salaries",
  "FinanceChng_Benefits",
  "FinanceChng_TotRent",
  "FinanceChng_TotTech",
  "PrgSrvc_Amt_Srvc",
  "PrgSrvc_Amt_Num",
  "PrgSrvc_Amt_Fee",
  "FndRaise_TotExp",
  "FndRaise_TotDigAppeal",
  "FndRaise_TotDirMail",
  "FndRaise_TotVirtEvent",
  "FndRaise_TotInPersEvent",
  "FndRaise_DAF_Grnt_Chng",
  "FndRaise_Priv_Grnt_Chng",
  "FndRaise_Corp_Grnt_Chng",
  "FndRaise_Cashbelow250_Chng",
  "FndRaise_Cashabove250_Chng"
)

# Questions with 2 possible answers: Received, did not receive
binary_rcv_cols_ls <- list(
  "FndRaise_DAF_Rcv" = "Received funding from donor-advised funds",
  "FndRaise_PFGrnt_Rcv" = "Received grants from private foundations",
  "FndRaise_Corp_Found_Grnt_Rcv" = "Received grants from corporate foundations or giving programs",
  "FndRaise_CFGrnt_Rcv" = "Received grants from community foundations"
)

# CEO and Board chair binary questions
ceo_bchair_binary_ls <- list(
  "Dem_CEO_Disabled" = list("CEO has a disability", 
                            "CEO does not have a disability"),
  "Dem_BChair_Disabled" = list("Board chair has a disability",
                               "Board chair does not have a disability"),
  "CEOgender_NB" = list("CEO is nonbinary",
                        "CEO is not nonbinary"), 
  "BChairgender_NB" = list("Board chair is nonbinary",
                           "Board chair is not nonbinary"),
  "Dem_CEO_LGBTQ" = list("CEO is LGBTQ",
                         "CEO is not LGBTQ"), 
  "Dem_BChair_LGBTQ" = list("Board chair is LGBTQ",
                            "Board chair is not LGBTQ"),
  "CEOgender_Woman" = list("CEO is a woman",
                           "CEO is not a woman"), 
  "BChairgender_Woman" = list("Board chair is a woman",
                              "Board chair is not a woman"),
  "CEOgender_Man" = list("CEO is a man",
                          "CEO is not a man"), 
  "BChairgender_Man" = list("Board chair is a man",
                            "Board chair is not a man")
)

# Government funding questions
gvt_fndraise_vars <- c(
  "FndRaise_LocGvtGrnt_Rcv",
  "FndRaise_StateGvtGrnt_Rcv",
  "FndRaise_FedGvtGrnt_Rcv",
  "FndRaise_LocGvtLn_Rcv",
  "FndRaise_StateGvtLn_Rcv",
  "FndRaise_FedGvtLn_Rcv",
  "FndRaise_LocGvtCntrct_Rcv",
  "FndRaise_StateGvtCntrct_Rcv",
  "FndRaise_FedGvtCntrct_Rcv",
  "FndRaise_FeeOth_Rcv",
  "FndRaise_FeeMed_Rcv",
  "FndRaise_Tribal_Rcv",
  "FndRaise_GovOth_Rcv",
  "FndRaise_DAF_Grnt_Chng",
  "FndRaise_Priv_Grnt_Chng",
  "FndRaise_Corp_Grnt_Chng"
)

# Questions valid for 2024 but not 2025
qns_invalid_2024 <- c(
  "PercentDon_Tot",
  "PercentGov",
  "PercentEarned"
)

# Demographic percentage questions
percentdem_vars <- c(
  "PercentDem_Women_Board",
  "PercentDem_LGBTQ_Board",
  "PercentDem_Disabled_Board",
  "PercentDem_Young_Board",
  "PercentDem_ReceivedServices_Board",
  "PercentDem_Women_Staff",
  "PercentDem_POC_Staff",
  "PercentDem_LGBTQ_Staff",
  "PercentDem_Disabled_Staff",
  "PercentDem_Young_Staff",
  "PercentDem_ReceivedServices_Staff",
  "PercentDem_POC_Board"
)

# Number of staff questions
numstaff_vars <- c(
  "Staff_Fulltime_2023",
  "Staff_Fulltime_2024",
  "Staff_Parttime_2023",
  "Staff_Parttime_2024" 
)

# Variables with percentages
percent_vars <- c(
  "PercentDon_Tot",
  "PercentDAF_Tot",
  "PercentPriv_Tot",
  "PercentCorp_Tot",
  "PercentGov",
  "PercentEarned"
)

# List containing the summary methods for each survey variable, variable names are mapped to their corresponding summaryVariable in the output dataset. Except "% of respondents, by type", are left as "% of respondents" since the methods used are the same.
metrics <- list(
  TotRev_clean = "median",
  TotExp = "median",
  Reserves_Est = "median",
  FinanceChng_Reserves = "% of respondents",
  FinanceChng_TotExp = "% of respondents",
  FinanceChng_Salaries = "% of respondents",
  FinanceChng_Benefits = "% of respondents",
  FinanceChng_TotRent = "% of respondents",
  FinanceChng_TotTech = "% of respondents",
  GeoAreas_ServedLocal = "% of respondents",
  GeoAreas_State = "% of respondents",
  GeoAreas_Servedmultistate = "% of respondents",
  GeoAreas_National = "% of respondents",
  GeoAreas_International = "% of respondents",
  ProgDem_BelowFPL = "% of respondents",
  ProgDem_Disabled = "% of respondents",
  ProgDem_Veterans = "% of respondents",
  ProgDem_LGBTQ = "% of respondents",
  ProgDem_Foreign = "% of respondents",
  ProgDem_Latinx = "% of respondents",
  ProgDem_Black = "% of respondents",
  ProgDem_Indigenous = "% of respondents",
  ProgDem_Asian = "% of respondents",
  ProgDem_Men = "% of respondents",
  ProgDem_Women = "% of respondents",
  ProgDem_Nonbinary = "% of respondents",
  ProgDem_Children = "% of respondents",
  ProgDem_YoungAdults = "% of respondents",
  ProgDem_Adults = "% of respondents",
  ProgDem_Elders = "% of respondents",
  PplSrv_MeetDemand = "% of respondents",
  PrgSrvc_Suspend = "% of respondents",
  PrgSrvc_Amt_Srvc = "% of respondents",
  PrgSrvc_Amt_Num = "% of respondents",
  Dmnd_NxtYear = "% of respondents",
  PercentDon_Tot = "average of %",
  FndRaise_DnrBlw250_ratio = "average of %",
  FndRaise_DnrAbv250_ratio = "average of %",
  PercentDAF_Tot = "average of %",
  PercentPriv_Tot = "average of %",
  PercentCorp_Tot = "average of %",
  PercentGov = "average of %",
  PercentEarned = "average of %",
  FndRaise_DAF_Rcv = "% of respondents",
  FndRaise_PFGrnt_Rcv = "% of respondents",
  FndRaise_Corp_Found_Grnt_Rcv = "% of respondents",
  FndRaise_CFGrnt_Rcv = "% of respondents",
  FndRaise_DAF_Grnt_Chng = "% of respondents",
  FndRaise_Priv_Grnt_Chng  = "% of respondents",
  FndRaise_Corp_Grnt_Chng  = "% of respondents",
  PrgSrvc_Amt_Fee = "% of respondents",
  FndRaise_TotExp = "% of respondents",
  FndRaise_TotDigAppeal = "% of respondents",
  FndRaise_TotDirMail = "% of respondents",
  FndRaise_TotVirtEvent = "% of respondents",
  FndRaise_TotInPersEvent = "% of respondents",
  PercentDem_Women_Staff = "% of respondents",
  Staff_RegVlntr = "median",
  Staff_EpsdVlntr = "median",
  PercentDem_Women_Board = "% of respondents",
  PercentDem_LGBTQ_Board = "% of respondents",
  PercentDem_Disabled_Board = "% of respondents",
  PercentDem_Young_Board = "% of respondents",
  PercentDem_ReceivedServices_Board = "% of respondents",
  PercentDem_POC_Staff = "% of respondents",
  PercentDem_LGBTQ_Staff = "% of respondents",
  PercentDem_Disabled_Staff = "% of respondents",
  PercentDem_Young_Staff = "% of respondents",
  PercentDem_ReceivedServices_Staff = "% of respondents",
  Staff_Boardmmbr = "median",
  PercentDem_POC_Board = "% of respondents",
  CEOrace_POC = "% of respondents",
  Dem_CEO_LGBTQ = "% of respondents",
  Dem_CEO_Disabled = "% of respondents",
  Dem_CEO_Under35 = "% of respondents",
  BChair_POC = "% of respondents",
  Dem_BChair_LGBTQ = "% of respondents",
  Dem_BChair_Disabled = "% of respondents",
  Dem_BChair_Under35 = "% of respondents",
  Staff_Fulltime = "% of respondents",
  Staff_Parttime = "% of respondents",
  CEOgender_Man = "% of respondents",
  CEOgender_Woman = "% of respondents",
  CEOgender_NB = "% of respondents",
  BChairgender_Man = "% of respondents",
  BChairgender_Woman = "% of respondents",
  BChairgender_NB = "% of respondents",
  FndRaise_Cashbelow250_Chng = "% of respondents",
  FndRaise_Cashabove250_Chng = "% of respondents",
  Cash_Reserves = "% of respondents"
)

# Groupby combinations
groupby_ls <- list(
  "National" = c(
    "National",
    "CensusRegion4", 
    "state", 
    "SizeStrata", 
    "Subsector",
    "census_urban_area"
  ), 
  "CensusRegion4" = c("CensusRegion4", 
                      "state", 
                      "SizeStrata",  
                      "Subsector",
                      "census_urban_area"), 
  "state" = c("state", 
              "SizeStrata", 
              "Subsector",
              "census_urban_area") 
)

groupbys_to_exclude <- c("Urban or rural designation",
                         "Size of annual expenses")

# There are several observations with NA values for Urban/Rural that we want to exclude
rows_to_exclude <- tibble(
  filterType = c("National", "Region", "State"),
  filterOpt = c("National", "Midwest", "MI"),
  splitByOpt = c("Rural/Urban", "Rural/Urban", "Rural/Urban"),
  splitByOpt_category = c("", "", "")
)

# States to exclude due to insufficient responses
states_to_exclude <- list(
  "2025" = c(
    "AK",
    "CT",
    "DC",
    "DE",
    "GA",
    "HI",
    "IA",
    "IN",
    "KS",
    "LA",
    "MD",
    "ME",
    "MO",
    "MS",
    "ND",
    "NE",
    "NH",
    "NJ",
    "NV",
    "OR",
    "RI",
    "SC",
    "SD",
    "VT",
    "WI",
    "WV",
    "WY"
  ),
  "2024" = c(
    "AK",
    "CT",
    "DC",
    "DE",
    "HI",
    "IA",
    "KS",
    "ME",
    "ND",
    "NE",
    "NH",
    "NJ",
    "RI",
    "SC",
    "SD",
    "VT",
    "WV",
    "WY"
  )
)