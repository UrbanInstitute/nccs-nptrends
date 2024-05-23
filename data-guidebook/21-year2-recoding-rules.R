## ----include=FALSE, echo=FALSE------------------------------------------------
library( haven )
library( dplyr )
library( tidyr )
library( epoxy )
library( memisc )
library( labelled )

# SOURCE PREVIOUS STEPS 

knitr::purl( "20-YEAR-TWO.qmd" )   # convert QMD to R script
source( "20-YEAR-TWO.R" )          # run all chunks in prior step



## ----eval=F, echo=F-----------------------------------------------------------
# LOAD SURVEY DATA FROM SAVED FILE AFTER LAST CHAPTER

fpath <- "DATA-PREP/02-year-two/02-data-intermediate/"
fname <- "SURVEYDF.csv"
survey_df <- read.csv( paste0( fpath, fname ) )

# LOAD DATA DICTIONARY 

dd <- readxl::read_xlsx( 
        "../data-dictionaries/dd-nptrends-wave-02.xlsx", 
        sheet = "data dictionary" )


## ----echo=F-------------------------------------------------------------------
# Functions to convert survey variable to survey
# item with missingness and variable labels

recode_x <- function( x, pattern, replace ){
  stringi::stri_replace_all_fixed( x, pattern, replace, vectorize_all=F  )
}


as_memisc_item <- function( x, val.x, lab.x, missing.x=NULL ){
  
  # add unnamed values 
  diff <- setdiff( x, val.x ) |> na.omit()
  val.x <- c( val.x, diff )
  lab.x <- c( lab.x, diff )
  
  # assign labels 
  names( val.x ) <- lab.x
  
  # convert to memisc survey item
  x <- x |> 
    memisc::as.item(
      labels = val.x,
      missing.values = missing.x ) 
  
  return(x)  
}

# wrapper for both steps

f <- function( x, pattern, replace, val.x, lab.x, missing.x=NULL ){
  x <- recode_x( x, pattern, replace )
  x <- as_memisc_item( x, val.x, lab.x, missing.x )
  return(x)
}

# add question labels for columns k

add_q_details <- function( x, df ){
  
  v <- df[[x]]
  
  # add question label attribute
  q.label <- dd$vlabel[ dd$vname == x ] |> 
    na.omit() |>
    paste0( collapse=" ;; " )
 
  # add variable type attribute
  q.type <- dd$type[ dd$vname == x ] |> 
    na.omit() |>
    paste0( collapse=" ;; " )
  
  # add question text
  q.text <- dd$description[ dd$vname == x ] |> na.omit()
    
  wording( v )     <- q.label
  description( v ) <- paste0( "TYPE: ", q.type )
  annotation( v )["QUESTION TXT"] <- q.text 
  
  return( v )
}

# apply rules to specific columns K

recode_columns <- function( df, k, pattern, replace, val.x, lab.x, mis.x=NULL ){
  df[ k ] <- lapply( df[k], f, pattern, replace, val.x, lab.x, mis.x )
  df[ k ] <- purrr::map( k, add_q_details, df )
  return(df)
}


# ALTERNATIVE REPRESENTATION OF RECODING RULES

parse_rules <- function(x) {
  L <- strsplit( x, "=>>|~" )
  L <- lapply( L, trimws )
  m <- matrix( unlist(L), byrow=T, ncol=2 )
  d <- as.data.frame( m )
  names( d ) <- c("pattern","replace")
  return( d )
}


# CLEANING NON-NUMERIC VALUES FROM NUMERIC RESPONSES

keep_numbers <- function(x){
  x <- gsub( "[$,]", "", x ) %>% as.numeric()
  x[ x == -99 ] <- Inf
  return(x)
}


## ----eval=F, echo=F-----------------------------------------------------------

#####
#####   FUNDRAISING EXAMPLE
#####

# APPLY TO COLUMNS K:
COLUMNS <-  fundraise_skrcv_qns_bool

# VALUES FOR RECODING 
pattern <- c( "(select all that apply)", "-99" )
replace <- c( "1", "0" )

# MEMISC LABELS AND MISSING VALUE CODES  
values  <- c("0", "1")
labels  <- c( "No", "Yes" )
missing <- "UNSURE"

d2 <- dplyr::select( survey_df, all_of( COLUMNS ) )
d3 <- recode_columns( d2, k=COLUMNS, pattern, replace, values, labels, missing )
codebook( d3 )


## ----eval=F, echo=F-----------------------------------------------------------

# ALTERNATIVE REPRESENTATION OF RECODING RULES

RULES <- c(     ### -----------------------------
       
  "   (select all that apply)   =>>   1   ", 
  "                       -99   =>>   0   "    

)               ### ------------------------------


# VALUES FOR RECODING 
rules <- parse_rules( RULES )
pattern <- rules[[ "pattern" ]]
replace <- rules[[ "replace" ]]

# MEMISC LABELS AND MISSING VALUE CODES  
values  <- c("0", "1")
labels  <- c( "No", "Yes" )
missing <- "UNSURE"

d3 <- recode_columns( d2, k=COLUMNS, pattern, replace, values, labels, missing )
codebook( d3 )



## ----eval=F, echo=F-----------------------------------------------------------
# NUMERIC ENCODING WITH MEMISC TO PRESERVE MISSING CASES
#
#   We want to use a number to preserve the numeric data,
#   but we don't want to number to impact computations.
#   Ideally the user has to remove missing values before 
#   any computations so that the missing values are not
#   polluting the statistics.

#   Options: -n (n being any value), NaN, Inf

survey_df <- read.csv( paste0( fpath, fname ) )
x <- survey_df[[ "FndRaise_MajGift_Amt" ]]
sum( x == -99, na.rm=T )  # 18 missing values


x[ x == -99 ] <- -1   # user might calculate without realizing
x[ x == -99 ] <- NaN  # can't calculate until dropping and distinct from NA;
                      #   - memisc doesn't recognize as missing value though
x[ x == -99 ] <- Inf  # this one works 


keep_numbers <- function(x){
  x <- gsub( "[$,]", "", x ) %>% as.numeric()
  x[ x == -99 ] <- Inf
  return(x)
}

x <- survey_df[[ "FndRaise_MajGift_Amt" ]]
x <- keep_numbers( x )
x <- memisc::as.item( x, missing.values=Inf )

codebook( x )

   #   Storage mode: double
   #   Measurement: interval
   #   Missing values: Inf
   # 
   #   Values                  N Percent
   #                                  
   #      M (unlab.mss.)      18     2.2
   #   NA M                  156    19.0
   #                                  
   #        Min:       0.010            
   #        Max: 1000000.000            
   #       Mean:    5935.894            
   #   Std.Dev.:   44801.714

# missing values preserved in memisc object
x %>% print()
x %>% include.missings() %>% as.integer()


# converted to NA when saving to file
x %>% as.numeric() %>% summary()


## -----------------------------------------------------------------------------
# APPLY TO COLUMNS K:
bool_qns <- 
  c( program_change_qns_bool, 
     fundraise_qns_bool, 
     cares_qns_bool, 
     finance_chng_qns_bool, 
     leadership_chng_qns_bool )

COLUMNS <-  bool_qns

# VALUES THAT NEED RECODING
RULES <- c(
  
           "  Yes    =>>    1    ", 
           "  No     =>>    0    ", 
           "  Unsure =>>    97   ", 
           "  N/A    =>>    98   ", 
           "  -99    =>>    99   "   )

rules <- parse_rules( RULES )          
pattern <- rules[[ "pattern" ]]
replace <- rules[[ "replace" ]]


# MEMISC LABELS AND MISSING VALUE CODES
values  <- c( 0, 1, 97, 98, 99 )
labels  <- c( "No", "Yes", "Unsure", "Not Applicable", "N/A" )
missing <- c( 97, 98, 99 )

# RECODE VARIABLES 
survey_df <- 
  survey_df %>% 
  recode_columns( k=COLUMNS, pattern, replace, values, labels, missing )


## ----eval=F, echo=F-----------------------------------------------------------
# codebook( survey_df[ COLUMNS[1] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html(codebook( survey_df[ COLUMNS[1] ] ))


## -----------------------------------------------------------------------------
# APPLY TO COLUMNS K:
COLUMNS <-  fundraise_skrcv_qns_bool

# VALUES THAT NEED RECODING

RULES <- c(    
  
  "  (select all that apply)    =>>    1    ", 
  "                      -99    =>>    0    "    )


rules <- parse_rules( RULES )          
pattern <- rules[[ "pattern" ]]
replace <- rules[[ "replace" ]]

# MEMISC LABELS AND MISSING VALUE CODES 
values  <- c( "0", "1" )
labels  <- c( "No", "Yes" )
missing <- "UNSURE"

# RECODE VARIABLES 
survey_df <- 
  survey_df %>% 
  recode_columns( k=COLUMNS, pattern, replace, values, labels, missing )


## ----eval=F, echo=F-----------------------------------------------------------
# codebook( survey_df[ COLUMNS[1] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html(codebook( survey_df[ COLUMNS[1] ] ))


## -----------------------------------------------------------------------------
# APPLY TO COLUMNS K:
COLUMNS <-  race_gender_qns_bool

# VALUES THAT NEED RECODING

RULES <- c(    
  
          "          Asian/Pacific Islander    =>>     1   ",
          "          Black/African American    =>>     1   ",
          "                 Latinx/Hispanic    =>>     1   ",
          " Native American/American Indian    =>>     1   ",
          "                           White    =>>     1   ",
          "                             Man    =>>     1   ",
          "                           Woman    =>>     1   ",
          "                           Trans    =>>     1   ",
          "Gender non-conforming/Non-Binary    =>>     1   ",
          "         Other (please specify):    =>>     1   ",
          "                             -99    =>>     x   ",
          "                               0    =>>     0   "     )


rules <- parse_rules( RULES )          
pattern <- rules[[ "pattern" ]]
replace <- rules[[ "replace" ]]

# MEMISC LABELS AND MISSING VALUE CODES 
values  <- c( "0", "1", "x" )
labels  <- c( "No", "Yes", "Incomplete" )
missing <- "x"

# RECODE VARIABLES 
survey_df <- 
  survey_df %>% 
  recode_columns( k=COLUMNS, pattern, replace, values, labels, missing )


## ----eval=F, echo=F-----------------------------------------------------------
codebook( survey_df[ COLUMNS[1] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html(codebook( survey_df[ COLUMNS[1] ] ))


## -----------------------------------------------------------------------------
na_bool_qns <- 
  c( staff_qns_bool, 
     reserve_qns_bool, 
     people_served_qns_bool )


# APPLY TO COLUMNS K:
COLUMNS <-  na_bool_qns

# VALUES THAT NEED RECODING

RULES <- c(    
          
             "    C    =>>     1   ", 
             "  -99    =>>     0   ", 
             "  N/A    =>>     1   "     )


rules <- parse_rules( RULES )          
pattern <- rules[[ "pattern" ]]
replace <- rules[[ "replace" ]]

# MEMISC LABELS AND MISSING VALUE CODES 
values  <- c( "0", "1" )
labels  <- c( "No", "Yes" )
missing <- NULL

# RECODE VARIABLES 
survey_df <- 
  survey_df %>% 
  recode_columns( k=COLUMNS, pattern, replace, values, labels, missing )


## ----eval=F, echo=F-----------------------------------------------------------
codebook( survey_df[ COLUMNS[1] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html(codebook( survey_df[ COLUMNS[1] ] ))


## -----------------------------------------------------------------------------
# APPLY TO COLUMNS K:
COLUMNS <-  demand_fct_qns

# VALUES THAT NEED RECODING

RULES <- c(    
          
             "        Decrease    =>>     0   ",
             "   Stay the same    =>>     1   ", 
             "        Increase    =>>     2   ", 
             "             -99    =>>     x   "    )


rules <- parse_rules( RULES )          
pattern <- rules[[ "pattern" ]]
replace <- rules[[ "replace" ]]

# MEMISC LABELS AND MISSING VALUE CODES 
values  <- c( 2, 1, 0, "x" )
labels  <- c( "Increase", "Unchanged", "Decrease", "Incomplete" )
missing <- "x"

# RECODE VARIABLES 
survey_df <- 
  survey_df %>% 
  recode_columns( k=COLUMNS, pattern, replace, values, labels, missing )


## ----eval=F, echo=F-----------------------------------------------------------
codebook( survey_df[ COLUMNS[1] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html(codebook( survey_df[ COLUMNS[1] ] ))


## -----------------------------------------------------------------------------
# APPLY TO COLUMNS K:
COLUMNS <-  fundraise_change_qns_fct

# VALUES THAT NEED RECODING

RULES <- c(    
          
     "   Increased significantly (by more than 10%)    =>>     5   ",
     "      Increased moderately (by less than 10%)    =>>     4   ",
     "                 Stayed more or less the same    =>>     3   ",
     "      Decreased moderately (by less than 10%)    =>>     2   ",
     "   Decreased significantly (by more than 10%)    =>>     1   ",
     "                                       Unsure    =>>     0   ",
     "                                          -99    =>>     X   ",
     "                                          N/A    =>>   N/A   "  )
 
rules <- parse_rules( RULES )          
pattern <- rules[[ "pattern" ]]
replace <- rules[[ "replace" ]]

# MEMISC LABELS AND MISSING VALUE CODES 
values  <- c( 5, 4, 3, 2, 1, 0, "X", "N/A" )
labels  <- c( "Increase Significantly", "Increase Moderately", 
              "Unchanged", "Decrease Moderately", "Decrease Significantly", 
              "Unsure", "Incomplete", "Not Applicable" )
missing <- c( "X" )

# RECODE VARIABLES 
survey_df <- 
  survey_df %>% 
  recode_columns( k=COLUMNS, pattern, replace, values, labels, missing )


## ----eval=F, echo=F-----------------------------------------------------------
codebook( survey_df[ COLUMNS[1] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html(codebook( survey_df[ COLUMNS[1] ] ))


## -----------------------------------------------------------------------------
# APPLY TO COLUMNS K:
COLUMNS <-  volimportance_qns_fct

# VALUES THAT NEED RECODING

RULES <- c(    

"           Essential - we depend entirely on volunteers to carry out our mission and goals    =>>     5   ",
"           Very important - we depend on volunteers for a wide range of tasks, but not all    =>>     4   ",
"                        Somewhat important - we depend on volunteers for several key tasks    =>>     3   ",
"                 Not very important - we depend on volunteers for only non-essential tasks    =>>     2   ",
"  Not at all important - we could carry out our mission and goals without using volunteers    =>>     1   ",
"                                                                  We do not use volunteers    =>>     0   ",
"                                                                                       -99    =>>     X   "    )

rules <- parse_rules( RULES )          
pattern <- rules[[ "pattern" ]]
replace <- rules[[ "replace" ]]

# MEMISC LABELS AND MISSING VALUE CODES 
values  <- c( 5, 4, 3, 2, 1, 0, "X" )
labels  <- c( "Essential", "Very Important", "Somewhat Important", 
              "Not Very Important", "Not At All Important", "Not Used", 
              "Incomplete" )
missing <- c( "X" )

# RECODE VARIABLES 
survey_df <- 
  survey_df %>% 
  recode_columns( k=COLUMNS, pattern, replace, values, labels, missing )


## ----eval=F, echo=F-----------------------------------------------------------
codebook( survey_df[ COLUMNS[1] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html(codebook( survey_df[ COLUMNS[1] ] ))


## -----------------------------------------------------------------------------
# APPLY TO COLUMNS K:
COLUMNS <-  donimportance_qns_fct

# VALUES THAT NEED RECODING

RULES <- c(    

  "          Essential, we depend entirely on individual donations to carry out our mission and goals  =>>  5  ",
  "     Very important, we depend on individual donations for a wide range of activities, but not all  =>>  4  ",
  "                           Important, we depend on individual donations for several key activities  =>>  3  ",
  "           Not very important, we depend on individual donations for only non-essential activities  =>>  2  ",
  " Not at all important, we could carry out our mission and goals without donations from individuals  =>>  1  ",
  "                                                      We do not receive donations from individuals  =>>  0  ",
  "                                                                                               -99  =>>  X  "  )


rules <- parse_rules( RULES )          
pattern <- rules[[ "pattern" ]]
replace <- rules[[ "replace" ]]

# MEMISC LABELS AND MISSING VALUE CODES 
values  <- c(5, 4, 3, 2, 1, 0, "X" )
labels  <- c( "Essential", "Very Important", 
              "Somewhat Important", "Not Very Important", 
              "Not At All Important", "Not Used", "Incomplete")
missing <- c( "X" )

# RECODE VARIABLES 
survey_df <- 
  survey_df %>% 
  recode_columns( k=COLUMNS, pattern, replace, values, labels, missing )


## ----eval=F, echo=F-----------------------------------------------------------
codebook( survey_df[ COLUMNS[1] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html(codebook( survey_df[ COLUMNS[1] ] ))


## -----------------------------------------------------------------------------
# APPLY TO COLUMNS K:
COLUMNS <-  extaffairs_qns_fct

# VALUES THAT NEED RECODING

RULES <- c(    

     "             Frequently    =>>     4   ",
     "    Almost all the time    =>>     3   ",
     "           Occasionally    =>>     2   ",
     "                 Rarely    =>>     1   ",
     "                  Never    =>>     0   ",
     "                    -99    =>>     X   "   )
     
rules <- parse_rules( RULES )          
pattern <- rules[[ "pattern" ]]
replace <- rules[[ "replace" ]]

# MEMISC LABELS AND MISSING VALUE CODES 
values  <- c( 4, 3, 2, 1, 0, "X" )
labels  <- c( "Frequently", "More Often than Not", 
              "Occasionally", "Rarely", "Never", "Incomplete" )
missing <- c( "X" )

# RECODE VARIABLES 
survey_df <- 
  survey_df %>% 
  recode_columns( k=COLUMNS, pattern, replace, values, labels, missing )


## ----eval=F, echo=F-----------------------------------------------------------
codebook( survey_df[ COLUMNS[1] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html(codebook( survey_df[ COLUMNS[1] ] ))


## -----------------------------------------------------------------------------
int_qns <- 
  c( staff_qns_int, 
     people_served_qns_int, 
     fundraise_donor_qns_int )

COLUMNS <- int_qns

survey_df[ COLUMNS ] <- 
  survey_df[ COLUMNS ] %>%
  lapply( recode_x, pattern=c("N/A","-99"), replace=c("Inf","Inf") )

survey_df[ COLUMNS ] <- 
  survey_df[ COLUMNS ] %>%
  lapply( as.numeric )

survey_df[ COLUMNS ] <- 
  survey_df[ COLUMNS ] %>%
  lapply( memisc::as.item, missing.values=Inf )

survey_df[ COLUMNS ] <- purrr::map( COLUMNS, add_q_details, survey_df )


## ----eval=F, echo=F-----------------------------------------------------------
codebook( survey_df[ COLUMNS[1] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html(codebook( survey_df[ COLUMNS[1] ] ))


## ----warning=FALSE------------------------------------------------------------
numeric_qns <- 
  c( majorgift_qn_num, 
     reserve_qns_num, 
     cares_qns_num, 
     finance_revenue_qns_num )

COLUMNS <- numeric_qns

survey_df[ COLUMNS ] <- 
  survey_df[ COLUMNS ] %>% 
  lapply( keep_numbers )

survey_df[ COLUMNS ] <- 
  survey_df[ COLUMNS ] %>%
  lapply( memisc::as.item, missing.values=Inf )

survey_df[ COLUMNS ] <- purrr::map( COLUMNS, add_q_details, survey_df )


## ----eval=F, echo=F-----------------------------------------------------------
codebook( survey_df[ COLUMNS[2] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html( codebook( survey_df[ COLUMNS[1] ] ))


## -----------------------------------------------------------------------------
text_qns <- 
  c( staff_qns_text, 
     finance_chng_qns_text, 
     finance_revenue_qns_text, 
     fundraise_qns_text, 
     leadership_chng_qns_text, 
     primary_cncrn_qn_text, 
     program_change_qns_txt, 
     race_gender_qns_text)

survey_df[ text_qns ] <- 
  survey_df[ text_qns ] %>%
  lapply( recode_x, pattern="-99", replace=NA )

survey_df[ text_qns ] <- 
  survey_df[ text_qns ] %>%
  lapply( memisc::as.item )

survey_df[ text_qns ] <- purrr::map( text_qns, add_q_details, survey_df )

codebook( survey_df[ text_qns[1] ] )


## ----eval=F, echo=F-----------------------------------------------------------
# codebook( survey_df[ COLUMNS[1] ] )


## ----results="asis", echo=F---------------------------------------------------
show_html( codebook( survey_df[ text_qns[1] ] ))


## ----echo=F-------------------------------------------------------------------
#### 
####   CLEANUP
#### 

# temp file for use in next chapter 

fpath <- "DATA-PREP/02-year-two/02-data-intermediate/"
fname <- "SURVEYDF-step21.csv"
write.csv( survey_df, paste0( fpath, fname ), row.names=F )

rdsname <- "SURVEYDF-step21.rds"
saveRDS( survey_df, paste0( fpath, rdsname ) )

