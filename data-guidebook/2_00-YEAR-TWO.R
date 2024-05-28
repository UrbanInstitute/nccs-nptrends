## ----echo=T-------------------------------------------------------------------
library( haven )
library( dplyr )
library( tidyr )
library( epoxy )
library( memisc )
library( labelled )


## ----cache=T------------------------------------------------------------------
# LOAD DATA DICTIONARY 
dd <- 
  readxl::read_xlsx( 
   "../data-dictionaries/dd-nptrends-wave-02.xlsx", 
   sheet = "data dictionary" )

# USE RAW VNAME IF VNAME IS EMPTY:
dd$vname[ is.na(dd$vname) ] <- 
   dd$vname_raw[ is.na(dd$vname) ]

# LOAD QUALTRICS SURVEY DATA 
fpath <- "DATA-PREP/02-year-two/01-data-raw/"
fname <- "wave-02-qualtrics-download-29mar23.csv"
survey_df  <- readr::read_csv( paste0( fpath, fname ) )
survey_df  <- survey_df[ -(1:2), ]                      # drop qualtrics headers  

fname <- "YEAR-02-COMPLETE-CASE-CODES.csv" 
cases <- read.csv( paste0(fpath,fname) )

cases <- dplyr::select( cases, EIN, Completion_Status )
survey_df <- merge( survey_df, cases, by="EIN", all.x=T )

survey_df <- 
  survey_df %>% 
  dplyr::filter( Completion_Status %in% c("Complete","Partial_keep") ) %>%
  dplyr::select( - Completion_Status )


## -----------------------------------------------------------------------------
survey_df[ 1:6, 51:55 ] %>% pander::pander()   # data peek


## -----------------------------------------------------------------------------
torename <- 
  dd %>% 
  dplyr::select( vname, vname_raw ) %>% 
  tidyr::drop_na()


## ----echo=F-------------------------------------------------------------------
(names(survey_df))[ 52:55 ] 


## ----cache=T------------------------------------------------------------------
survey_df <- 
  survey_df %>% 
  dplyr::rename_at(
     vars( torename$vname_raw ), 
     ~ torename$vname )


## ----echo=F-------------------------------------------------------------------
(names(survey_df))[ 52:55 ] 


## ----eval=F, echo=F-----------------------------------------------------------
# Add survey questions as an attribute in the data frame: 

fpath <- "DATA-PREP/02-year-two/01-data-raw/"
fname <- "wave-02-qualtrics-download-29mar23.csv"
temp          <- readr::read_csv( paste0( fpath, fname ) )
question.txt  <- as.character( temp[ 1, ] )
rm( temp )

names( question.txt ) <- names( survey_df )
attr( survey_df, "question_txt" ) <- question.txt


## ----cache=TRUE---------------------------------------------------------------
# SELECT COLUMNS TO DROP: 
DROP_THESE <- dd$vname[ dd$group == "DROP" ] |> na.omit()

survey_df <- 
  survey_df %>% 
  dplyr::select( -any_of( DROP_THESE ) )


## -----------------------------------------------------------------------------
# ADD SURVEY WEIGHTS 
fpath <- "DATA-PREP/00-sample-framework/"
fname <- "year2wt.csv"
wt2  <- readr::read_csv( paste0( fpath, fname ) )

survey_df <- merge( survey_df, wt2, by.x="EIN", by.y="ein", all.x=TRUE )


## ----echo=F-------------------------------------------------------------------
# Function to get survey questions 
# by group and data type from data dictionary

get_survey_questions <- function( data_dict, qgroup, qtype ){

    data_dict %>% 
    dplyr::filter( group == qgroup, type  == qtype ) %>% 
    dplyr::pull( "vname" )

}

print_qns <- function( qns, data_dict ){
  
  data_dict %>% 
    dplyr::select( vname, type, vlabel ) %>%
    dplyr::filter( vname %in% qns) %>% 
    pander::pander()
  
}


## ----cache=T, echo=F----------------------------------------------------------
## Changes to Programs and Services

program_change_qns_bool <- get_survey_questions( dd, "ProgChanges", "boolean" )
program_change_qns_txt  <- get_survey_questions( dd, "ProgChanges", "text" )

## People Served

people_served_qns_int  <- 
  get_survey_questions( dd, "PeopleServed#1", "integer" )
people_served_qns_bool <- 
  get_survey_questions( dd, "PeopleServed#2", "boolean" )

## Demand for future programs and services

demand_fct_qns <- get_survey_questions( dd, "Demand", "factor" ) 

## Staff and Volunteers

staff_qns_int <- 
  c( get_survey_questions( dd, "Staff#1", "integer" ),
     get_survey_questions( dd, "Staff#2", "integer" ) )

staff_qns_bool <- get_survey_questions( dd, "Staff#3", "boolean" )

staff_qns_text <- 
  c( get_survey_questions( dd, "Staff#1", "text" ), 
     get_survey_questions( dd, "Staff#2", "text" ),
     get_survey_questions( dd, "Staff#3", "text" )  )

## Importance of Volunteers and Donors

volimportance_qns_fct <- get_survey_questions( dd, "VolImportance", "factor" )
donimportance_qns_fct <- get_survey_questions( dd, "DonImportance", "factor" )

## Fundraising

fundraise_qns_bool <- get_survey_questions( dd, "FRchanges", "boolean" )
fundraise_qns_text <- get_survey_questions( dd, "FRchanges", "text" )

## Amount for major gifts

majorgift_qn_num <- get_survey_questions( dd, "Frmajgift", "numeric" )

## Fundraising Changes

fundraise_change_qns_fct <- get_survey_questions( dd, "FRchanges#1", "factor" )

## Number of donors

fundraise_donor_qns_int <- 
  get_survey_questions( dd, "FRnumberdonors", "integer" )

## Fundraising Sought and Received

fundraise_skrcv_qns_bool <- 
  c( get_survey_questions( dd, "Funding1#1", "boolean" ),
     get_survey_questions( dd, "Funding1#2", "boolean" ) )

## Revenue breakdown

finance_revenue_qns_num  <- get_survey_questions( dd, "Finances", "numeric")
finance_revenue_qns_text <- get_survey_questions( dd, "Finances", "text")

## Financial reserves

reserve_qns_num  <- get_survey_questions( dd, "Reserves", "numeric")
reserve_qns_bool <- get_survey_questions( dd, "Reserves", "boolean")

## CARES Funding

cares_qns_num  <- get_survey_questions( dd, "CARES", "numeric")
cares_qns_bool <- get_survey_questions( dd, "CARES", "boolean")

## Changes to finances

finance_chng_qns_bool <- get_survey_questions( dd, "FinanceChanges", "boolean")
finance_chng_qns_text <- get_survey_questions( dd, "FinanceChanges", "text")

## Leadership Changes

leadership_chng_qns_bool <- 
  get_survey_questions( dd, "LeadershipChanges", "boolean")

leadership_chng_qns_text <- 
  get_survey_questions( dd, "LeadershipChanges", "text")

## Race and Gender

race_gender_qns_bool <- 
  c( get_survey_questions( dd, "CEOrace",   "boolean" ),
     get_survey_questions( dd, "CEOgender", "boolean" ),
     get_survey_questions( dd, "BCrace",    "boolean" ),
     get_survey_questions( dd, "BCgender",  "boolean" ) )

race_gender_qns_text <- 
  c( get_survey_questions( dd, "CEOrace",   "text" ),
     get_survey_questions( dd, "CEOgender", "text" ),
     get_survey_questions( dd, "BCrace",    "text" ),
     get_survey_questions( dd, "BCgender",  "text" ) )

## External affairs

extaffairs_qns_fct <- get_survey_questions( dd, "ExternalAffairs", "factor")

## Primary concerns

primary_cncrn_qn_text <- get_survey_questions( dd, "PrimaryConcern", "text")


## ----eval=F, echo=F-----------------------------------------------------------

# CHECK FOR MISSED VARIABLES NOT INCLUDED ABOVE 

all.questions <- c(
  program_change_qns_bool, program_change_qns_txt,
  people_served_qns_bool, people_served_qns_int,
  demand_fct_qns,
  staff_qns_bool, staff_qns_int, staff_qns_text,
  volimportance_qns_fct, donimportance_qns_fct,
  leadership_chng_qns_bool, leadership_chng_qns_text,
  race_gender_qns_bool, race_gender_qns_text,
  finance_chng_qns_bool, finance_chng_qns_text,
  cares_qns_bool, cares_qns_num,
  reserve_qns_bool, reserve_qns_num,
  finance_revenue_qns_num, finance_revenue_qns_text,
  fundraise_skrcv_qns_bool,
  fundraise_donor_qns_int,
  fundraise_change_qns_fct,
  fundraise_qns_bool,fundraise_qns_text,
  majorgift_qn_num,
  extaffairs_qns_fct,
  primary_cncrn_qn_text )

omit.these.groups <- c("id","DROP","MainAddress")
all.vars <- dd$vname[ ! dd$group %in% omit.these.groups ] |> na.omit()
setdiff( all.vars, all.questions )


## ----echo=F, eval=F-----------------------------------------------------------

# Response Types 

# Preview of the types of raw responses in the qualtrics files prior to recoding: 

  
  
id <- dd$vname[ dd$group == "id" ] |> na.omit()
df_preview <- survey_df %>% dplyr::select( - any_of(id) )

ff <- function( x ){
  x <- x[ ! is.na(x) ]
  x <- unique(x) |> sort() |> head(10)
  if( length(x) == 10 ){ x <- c("{FIRST 10 VALUES ONLY}",x) }
  paste0( x, collapse=" ;; ")
}

L   <- sapply( df_preview, ff )
T   <- table( L ) |> sort( d=TRUE) 
T   <- T[ T > 1 ]
tdf <- T |> 
  as.data.frame() %>% 
  dplyr::rename( RESPONSE_VALUES = L )

print_qns <- function( val, L ){
  nm <- names( L[ L == val ] )
  paste0( nm, collapse=" ;; " )
}

tdf$QUESTIONS <- purrr::map_chr( tdf$RESPONSE_VALUES, print_qns, L )

tdf[c("QUESTIONS","RESPONSE_VALUES")] %>% pander::pander()


## ----echo=F-------------------------------------------------------------------
#### 
####   CLEANUP
#### 

# temp file for use in next chapter 

fpath <- "DATA-PREP/02-year-two/02-data-intermediate/"
fname <- "SURVEYDF.csv"
write.csv( survey_df, paste0( fpath, fname ), row.names=F )

rdsname <- "SURVEYDF.rds"
saveRDS( survey_df, paste0( fpath, rdsname ) )

