## ----include=FALSE, echo=FALSE----------------------------------------------------------------------------------------------------------------
library( haven )
library( dplyr )
library( tidyr )
library( epoxy )
library( memisc )
library( labelled )

# SOURCE PREVIOUS STEPS
# knitr::purl( "31-year3-recoding-rules.qmd" )   # convert QMD to R script
source(      "31-year3-recoding-rules.R"   )   # run all chunks in prior step


## ----echo=F-----------------------------------------------------------------------------------------------------------------------------------
# Function to convert survey variable to survey.item with missingness and variable labels
create_survey_item <- function(survey_data, qns, recode_vals, recode_labs, missing_vals){
  survey_data <- survey_data |>
    purrr::modify_at(
      .at = qns,
      .f = memisc::as.item,
      labels = structure(
        .Data = recode_vals,
        names = recode_labs
      ),
      missing.values = missing_vals
    )
  return(survey_data)
}


## ---------------------------------------------------------------------------------------------------------------------------------------------
fpath     <- "DATA-PREP/01-year-one/02-data-intermediate/"
fname     <- "wave-01-data-intermediate.csv"
year1_raw <- readr::read_csv( paste0( fpath, fname ) )


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Wrangle Data for Year 1 Responses on CEO race and Gender
year1_ceo <- year1_raw %>% 
  dplyr::select(EIN, CEOrace, CEOgender ) %>% 
  dplyr::filter(  ! CEOrace %in% c(-99, NA),
                  ! CEOgender %in% c(-99, NA)  ) %>% 
  dplyr::mutate(
    CEOrace = dplyr::case_match(
      CEOrace,
      1 ~ "AAPI", 2 ~ "Black", 3 ~ "Hisp", 4 ~ "NativeAm", 5 ~ "White", 6 ~ "Bi", 7 ~ "Oth",
      .default = "Oth"
    ),
    CEOgender = dplyr::case_match(
      CEOgender,
      1 ~ "Man", 2 ~ "Woman", 3 ~ "Trans", 4 ~ "NB", 5 ~ "Oth",
      .default = "Oth"
    ),
  race_check = "Yes",
  gender_check = "Yes"
  ) %>% 
  tidyr::pivot_wider(
    names_from = CEOrace,
    names_glue = "CEOrace_{CEOrace}",
    values_from = race_check, 
    values_fill = "No"
  ) %>% 
  tidyr::pivot_wider(
    names_from = CEOgender,
    names_glue = "CEOgender_{CEOgender}",
    values_from = gender_check, 
    values_fill = "No"
  )

# Wrangle Data for Year 1 Responses on Board Chair Race and Gender
year1_board <- year1_raw %>% 
  dplyr::select(EIN, BCrace, BCgender) %>% 
  dplyr::filter(  ! BCrace %in% c(-99, NA),
                  ! BCgender %in% c(-99, NA)) %>% 
  dplyr::mutate(
    BCrace = dplyr::case_match(
      BCrace,
      1 ~ "AAPI", 2 ~ "Black", 3 ~ "Hisp", 4 ~ "NativeAm", 5 ~ "White", 6 ~ "Bi", 7 ~ "Oth",
      .default = "Oth"
    ),
    BCgender = dplyr::case_match(
      BCgender,
      1 ~ "Man", 2 ~ "Woman", 3 ~ "Trans", 4 ~ "NB", 5 ~ "Oth",
      .default = "Oth"
    ),
  race_check = "Yes",
  gender_check = "Yes"
  ) %>% 
  tidyr::pivot_wider(
    names_from = BCrace,
    names_glue = "BChairrace_{BCrace}",
    values_from = race_check, 
    values_fill = "No"
  ) %>% 
  tidyr::pivot_wider(
    names_from = BCgender,
    names_glue = "BChairgender_{BCgender}",
    values_from = gender_check, 
    values_fill = "No"
  )

df.race2021 <- merge( year1_ceo, year1_board, by="EIN", all=TRUE )


## ---------------------------------------------------------------------------------------------------------------------------------------------
fpath <- "DATA-PREP/02-year-two/03-data-final/"
fname <- "YEAR-02-DATA-RESTRICTED-V02.csv"
year2_final <- read.csv( paste0( fpath, fname ) )

# ALL RACE AND GENDER CATEGORIES 
race.vars <- 
c("CEOrace_AAPI", "CEOrace_Black", "CEOrace_Hisp", "CEOrace_NativeAm", 
"CEOrace_White", "CEOrace_Oth", "CEOrace_Text", "CEOgender_Man", 
"CEOgender_Woman", "CEOgender_Trans", "CEOgender_NB", "CEOgender_Oth", 
"CEOgender_specify", "BChairrace_AAPI", "BChairrace_Black", "BChairrace_Hisp", 
"BChairrace_NativeAm", "BChairrace_White", "BChairrace_Oth", 
"BChairrace_Text", "BChairgender_Man", "BChairgender_Woman", 
"BChairgender_Trans", "BChairgender_NB", "BChairgender_Oth", 
"BChairgender_specify")


# select all race variables plus EIN for merge 
df.race2022 <- year2_final[ c("EIN",race.vars) ]


## ---------------------------------------------------------------------------------------------------------------------------------------------
# drop 2021 values if there are 2022 versions
drop <- intersect( df.race2021$EIN, df.race2022$EIN )

# drop %>% length()  506
# nrow( df.race2021 )  1656
# nrow( df.race2022 )  691
#
# total nrow:  1656 + 691 - 506 = 1841

df.race2021 <- 
  df.race2021 %>% 
  dplyr::filter( ! EIN %in% drop )

df.race2021$EIN <- as.numeric( df.race2021$EIN )
df.race2022$EIN <- as.numeric( df.race2022$EIN )
df.race2022 <- dplyr::bind_rows( df.race2022, df.race2021 )

# nrow( df.race2022 )  1841


## ---------------------------------------------------------------------------------------------------------------------------------------------
# creates race.vars.y versions:
# CEOrace_AAPI & CEOrace_AAPI.y 

survey_df <- 
  merge( survey_df, df.race2022, 
         by.x="ExternalReference", by.y="EIN", 
         all.x=TRUE )

# remove .x from varname.x versions:

nm <- names( survey_df )
nm <- gsub( "\\.x$", "", nm )
names( survey_df ) <- nm 


###
###   CEO UPDATES
###

# APPLY TO COLUMNS K:
COLUMNS <-  
  c("CEOrace_AAPI", "CEOrace_Black", 
    "CEOrace_Hisp", "CEOrace_NativeAm", 
    "CEOrace_White", 
    "CEOrace_Oth", "CEOrace_Text",
    "CEOgender_Man", "CEOgender_Woman", 
    "CEOgender_Trans", "CEOgender_NB", 
    "CEOgender_Oth", "CEOgender_specify" )

# ALL RESPONSES NA IN GROUP:
# indication that the question
# was skipped by the respondent

all.na <- apply( survey_df[ COLUMNS ], 
                 MARGIN = 1, 
                 FUN = function(x) all(is.na(x)) ) 

CHANGE_2023_X <- ! all.na

# NA means "no" if change occurred

na_to_no <- function(x){
  x[ is.na(x) ] <- "No"
  return(x)
}

survey_df[ COLUMNS ] <- 
  survey_df[ COLUMNS ] %>% 
  lapply( na_to_no )

# if they do not respond "1/yes" 
# to at least one question then
# assume they skipped them all

no_to_na <- function( x, rowid ){
  x[ rowid ] <- NA
  return(x)
}

survey_df[ COLUMNS ] <- 
  survey_df[ COLUMNS ] %>% 
  lapply( no_to_na, rowid=all.na )



# update 2022 data with 2023 responses: 
#   has.change is T/F indicator of any CEO
#   change in 2023

impute_race <- function( x.txt, df, has.change ){
  x.txt.y <- paste0( x.txt, ".y" )
  
  x1 <- df[[ x.txt   ]] # race indicator 2023 1/NA
  x2 <- df[[ x.txt.y ]] # race indicator 2022 Yes/No
  
  # update 2022 values with 2023 values
  x1[ is.na(x1) ] <- ""
  x2[ has.change ] <- x1[ has.change ]
  
  # return updated vector
  return( x2 )
}

survey_df[ COLUMNS ] <- 
  purrr::map( COLUMNS, impute_race, survey_df, CHANGE_2023_X )



###
###   BOARD UPDATES
###

# APPLY TO COLUMNS K:
COLUMNS <-  
  c("BChairrace_AAPI", "BChairrace_Black", 
    "BChairrace_Hisp", "BChairrace_NativeAm", 
    "BChairrace_White", 
    "BChairrace_Oth", "BChairrace_Text", 
    "BChairgender_Man", 
    "BChairgender_Woman", "BChairgender_Trans", 
    "BChairgender_NB", 
    "BChairgender_Oth", "BChairgender_specify" )

# ALL RESPONSES NA IN GROUP:
# indication that the question
# was skipped by the respondent

all.na <- apply( survey_df[ COLUMNS ], 
                 MARGIN = 1, 
                 FUN = function(x) all(is.na(x)) ) 

# any change in 2023
CHANGE_2023_X <- ! all.na

# NA means "no"

na_to_no <- function(x){
  x[ is.na(x) ] <- "No"
  return(x)
}

survey_df[ COLUMNS ] <- 
  survey_df[ COLUMNS ] %>% 
  lapply( na_to_no )

# if they do not respond "1/yes" 
# to at least one question then
# assume they skipped them all

no_to_na <- function( x, rowid ){
  x[ rowid ] <- NA
  return(x)
}

survey_df[ COLUMNS ] <- 
  survey_df[ COLUMNS ] %>% 
  lapply( no_to_na, rowid=all.na )



# update 2022 data with 2023 responses 

impute_race <- function( x.txt, df, has.change ){
  x.txt.y <- paste0( x.txt, ".y" )
  
  x1 <- df[[ x.txt   ]] # race indicator 2023 1/NA
  x2 <- df[[ x.txt.y ]] # race indicator 2022 Yes/No
  
  # update 2022 values with 2023 values
  x1[ is.na(x1) ] <- ""
  x2[ has.change ] <- x1[ has.change ]
  
  # return updated vector
  return( x2 )
}

survey_df[ COLUMNS ] <- 
  purrr::map( COLUMNS, impute_race, survey_df, CHANGE_2023_X )



# change varname.y to varname.2022

nm <- names( survey_df )
nm <- gsub( "\\.y$", "_v2022", nm )
names( survey_df ) <- nm 


## ---------------------------------------------------------------------------------------------------------------------------------------------

# CEOs

ceo.race.qns <- grep( "CEOrace", race_gender_qns_bool, value=T  )

# COUNT YES'S
num.races.ceo <-  
  apply( survey_df[ ceo.race.qns ], 
         MARGIN = 1, 
         FUN = function(x){ sum( x %in% c("1","Yes"), na.rm=T ) } ) 

survey_df$CEOrace_Bi <- ifelse( num.races.ceo > 1, "Yes","No" )

# OMIT ROWS WITH NO RESPONSE 

is_missing <- function(x){
  # can be NA or ""
  x[ is.na(x) ] <- ""
  return( x == "" )
}

all.missing <- 
  apply( survey_df[ ceo.race.qns ], 
         MARGIN = 1, 
         FUN = function(x){ all( is_missing(x) ) } ) 

survey_df$CEOrace_Bi[ all.missing ] <- ""

# BOARD CHAIRs

board.race.qns <- grep( "BChairrace", race_gender_qns_bool, value=T )

# COUNT YES
num.races.chair <-  
  apply( survey_df[ board.race.qns ], 
          MARGIN = 1, 
          FUN = function(x){ sum( x %in% c("1","Yes"), na.rm=T ) } ) 

survey_df$BChairrace_Bi <- ifelse( num.races.chair > 1, "Yes","No" )

# OMIT ROWS WITH NO RESPONSE 
all.missing <- 
  apply( survey_df[ board.race.qns  ], 
         MARGIN = 1, 
         FUN = function(x){ all( is_missing(x) ) } )  

survey_df$BChairrace_Bi[ all.missing ] <- ""


# MAKE MEMISC ITEM 

biracial_qns <- c( "CEOrace_Bi","BChairrace_Bi" )

# Create Survey Item
survey_df <- create_survey_item(
  survey_df, 
  biracial_qns, 
  recode_vals = c("Yes","No", "" ), 
  recode_labs = c("Yes","No", "Missing" ), 
  missing_vals = ""
)


## ----eval=F, echo=F---------------------------------------------------------------------------------------------------------------------------
## codebook( survey_df[ biracial_qns[2] ] )


## ----results="asis", echo=F-------------------------------------------------------------------------------------------------------------------
show_html(codebook( survey_df[ biracial_qns[2] ] ))


## ---------------------------------------------------------------------------------------------------------------------------------------------
# APPLY TO COLUMNS K:
COLUMNS <-  race_gender_qns_bool


# VALUES THAT NEED RECODING

RULES <- c(     "    1    =>>    Yes    ", 
                "  Yes    =>>    Yes    ", 
                "   No    =>>     No    "     )


rules <- parse_rules( RULES )
pattern <- rules[[ "pattern" ]]
replace <- rules[[ "replace" ]]

# MEMISC LABELS AND MISSING VALUE CODES
values  <- c( "No", "Yes", "", NA )
labels  <- c( "No", "Yes", "Missing in 2022", "No Response" )
missing <- c( "", NA )

# RECODE VARIABLES
survey_df <-
  survey_df %>%
  recode_columns( k=COLUMNS, pattern, replace, values, labels, missing )


## ----eval=F, echo=F---------------------------------------------------------------------------------------------------------------------------
## codebook( survey_df[ COLUMNS[5] ] )


## ----results="asis", echo=F-------------------------------------------------------------------------------------------------------------------
show_html(codebook( survey_df[ COLUMNS[5] ] ))


## ---------------------------------------------------------------------------------------------------------------------------------------------
# Create New Race variables
survey_df <- survey_df %>%
  dplyr::mutate(
    CEOrace = dplyr::case_when(
      CEOrace_AAPI == 'Yes' ~ 1,
      CEOrace_Black == 'Yes' ~ 2,
      CEOrace_Hisp == 'Yes' ~ 3,
      CEOrace_NativeAm == 'Yes' ~ 4,
      CEOrace_White == 'Yes' ~ 5,
      CEOrace_Oth == 'Yes' ~ 6,
      CEOrace_Bi == 'Yes' ~ 7,
      .default = NA
    ),
    BChairrace = dplyr::case_when(
      BChairrace_AAPI == 'Yes' ~ 1,
      BChairrace_Black == 'Yes' ~ 2,
      BChairrace_Hisp == 'Yes' ~ 3,
      BChairrace_NativeAm == 'Yes' ~ 4,
      BChairrace_White == 'Yes' ~ 5,
      BChairrace_Oth == 'Yes' ~ 6,
      BChairrace_Bi == 'Yes' ~ 7,
      .default = NA
  ))

race.categories <- c("CEOrace", "BChairrace")

# Create Survey Item
survey_df <- create_survey_item(
  survey_df, 
  race.categories, 
  recode_vals = c(1, 2, 3, 4, 5, 6, 7), 
  recode_labs = c("AAPI", "Black", "Hispanic", "NativeAm", "White", "Other","Biracial"), 
  missing_vals = NULL
)


## ----eval=F, echo=F---------------------------------------------------------------------------------------------------------------------------
## codebook( survey_df[ race.categories[1] ] )


## ----results="asis", echo=F-------------------------------------------------------------------------------------------------------------------
show_html(codebook( survey_df[ c("CEOrace", "BChairrace")[1] ] ))


## ----eval=F, echo=F---------------------------------------------------------------------------------------------------------------------------
## 
## df03 <- survey_df[ c(ceo.race.qns,board.race.qns,"CEOrace","BChairrace") ]
## 
## fpath <- "DATA-PREP/03-year-three/03-data-final/"
## 
## fname <- "YEAR-03-TEMP3.csv"
## write.csv( df03, paste0( fpath, fname ), row.names=F, na="" )


## ---------------------------------------------------------------------------------------------------------------------------------------------
survey_df <- survey_df %>%
  dplyr::mutate(
    CEOgender = dplyr::case_when(
      CEOgender_Man == 'Yes' ~ 1,
      CEOgender_Woman == 'Yes' ~ 2,
      CEOgender_Trans == 'Yes' ~ 3,
      CEOgender_NB == 'Yes' ~ 4,
      CEOgender_Oth == 'Yes' ~ 5,
      .default = NA
    ),
    BChairgender = dplyr::case_when(
      BChairgender_Man == 'Yes' ~ 1,
      BChairgender_Woman == 'Yes' ~ 2,
      BChairgender_Trans == 'Yes' ~ 3,
      BChairgender_NB == 'Yes' ~ 4,
      BChairgender_Oth == 'Yes' ~ 5,
      .default = NA
  ))

# Create Survey Item
survey_df <- create_survey_item(
  survey_df, 
  c("CEOgender", "BChairgender"), 
  recode_vals = c( 1, 2, 3, 4, 5 ), 
  recode_labs = c( "Man", "Woman", "Trans", "NB", "Other" ), 
  missing_vals = NULL
)


## ----eval=F, echo=F---------------------------------------------------------------------------------------------------------------------------
## 
## df01 <-
##   survey_df[ c("CEOgender_Man", "CEOgender_Woman", "CEOgender_Trans",
##                "CEOgender_NB", "CEOgender_Oth","CEOgender") ]
## 
## df02 <-
##   survey_df[ c("BChairgender_Man", "BChairgender_Woman", "BChairgender_Trans",
##                "BChairgender_NB", "BChairgender_Oth","BChairgender") ]
## 
## fpath <- "DATA-PREP/03-year-three/03-data-final/"
## 
## fname <- "YEAR-03-TEMP2.csv"
## write.csv( df01, paste0( fpath, fname ), row.names=F, na="" )
## 
## fname <- "YEAR-03-TEMP3.csv"
## write.csv( df02, paste0( fpath, fname ), row.names=F, na="" )


## ----eval=F, echo=F---------------------------------------------------------------------------------------------------------------------------
## codebook( survey_df[ c("CEOgender", "BChairgender")[1] ] )


## ----results="asis", echo=F-------------------------------------------------------------------------------------------------------------------
show_html(codebook( survey_df[ c("CEOgender", "BChairgender")[1] ] ))


## ---------------------------------------------------------------------------------------------------------------------------------------------
fpath <- "DATA-PREP/03-year-three/03-data-final/"
fname <- "YEAR-03-TEMP.csv"
write.csv( survey_df, paste0( fpath, fname ), row.names=F, na="" )

