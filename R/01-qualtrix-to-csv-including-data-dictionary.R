library( qualtRics )
library( dplyr )

# setwd( "..." )
source( "00-data-processing-utils.R" )




###########
###########   DATA DICTIONARY
###########

# NEED TO USE LEGACY = TRUE
# FOR THE DATA DICTIONARY
# TO WORK PROPERLY 

d <- 
  read_survey( 
    "data-raw/wave-02-qualtrics-download-29mar23.csv", 
    legacy = T )

head( as.data.frame(d) )

# exports data dictionary 

dd <- extract_colmap( d )  

# add group variables
# and factor labels 

dd <- 
  dd %>% 
  mutate( group_name = append_groups(qname) ) %>%
  group_by( group_name ) %>% 
  mutate( group_n = n(),
          group_name = ifelse( group_n > 1, group_name, "" ),
          is_group = ifelse( group_n > 1, "1", "0" ),
          group_levels = ifelse( group_n > 1, get_categories(description), "" ) ) %>%
  ungroup() %>% 
  mutate( type = ifelse( group_n > 1, "factor", "" ) ) %>% 
  select( qname, is_group, group_name, group_n, group_levels, 
          description, main, sub )


  
write.csv( dd, "../dd-nptrends-wave-02.csv" )

# FIX UTF8 PROBLEM: 
# open empty excel file;
# import csv by (data>>get data) to fix UTF8 encoding problem 






###########
###########   DATA EXPORT AND CLEAN-UP
###########


# USE LEGACY = TRUE
# TO DROP THE VAR LABEL
# FROM THE FIRST LINE  

d <- 
  read_survey( 
    "wave-02-qualtrics-download-29mar23.csv", 
    legacy = F )

head( as.data.frame(d) )


### Convert -99 to missing

d[ d == "-99" ] <- NA



### Combine Atomized Factors 

# columns are not binary - see below 
#
# f.cols <- 
#   c("CEOrace_1", "CEOrace_2", "CEOrace_3", 
#     "CEOrace_4", "CEOrace_5" )
# 
# flevels <- 
#   c("Asian/Pacific Islander","Black/African American",
#     "Latinx/Hispanic","Native American/American Indian",
#     "White" )
# 
# dd$CEOrace_f <- 
#   factorize( dd, 
#              cols=f.cols,
#              labels=flevels )

table( dd$CEOrace_1 )
table( dd$CEOrace_2 )
table( dd$CEOrace_3 )
table( dd$CEOrace_4 )
table( dd$CEOrace_5 )
table( dd$CEOrace_7 )

ceo_race <- rep( NA, nrow(dd) )
ceo_race[ dd$CEOrace_1 == "Asian/Pacific Islander" ] <- "Asian/Pacific Islander"
ceo_race[ dd$CEOrace_2 == "Black/African American" ] <- "Black/African American"
ceo_race[ dd$CEOrace_3 == "Latinx/Hispanic" ] <- "Latinx/Hispanic"
ceo_race[ dd$CEOrace_4 == "Native American/American Indian" ] <- "Native American/American Indian"
ceo_race[ dd$CEOrace_5 == "White" ] <- "White"

d$ceo_race <- ceo_race

write.csv( d, "../nptrends-wave-02-data-raw.csv" )

# find_all_groups( d$qname )
# 
# [1] "Q125"              "Name"              "Q126"             
# [4] "MainAddress"       "ProgChanges"       "PeopleServed#1"   
# [7] "PeopleServed#2"    "Staff#1"           "Staff#2"          
# [10] "Staff#3"           "FRchanges"         "FRmajgift"        
# [13] "FRchanges#1"       "FRnumberdonors"    "Funding1#1"       
# [16] "Funding1#2"        "Finances"          "FinancesTimer"    
# [19] "Reserves#1"        "Reserves#2"        "CARESfunding2"    
# [22] "FinanceChanges"    "LeadershipChanges" "CEOrace"          
# [25] "CEOgender"         "BCrace"            "BCgender"         
# [28] "ExternalAffairs"   "ContactInfoUpdate" "FirstName"        
# [31] "LastName"





# SECOND YEAR FILE FROM ROB
# IMPORT FILE FROM STATA:

library( haven )
d <- haven::read_dta( "nptrends_year2_13jan23_with_y2_weight.dta" )
