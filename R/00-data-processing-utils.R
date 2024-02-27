####### 
#######   ADD VARIABLE GROUPS TO DATA DICTIONARY:
#######
#######     Many variables belong to groups on the survey, such as types of revenue. 
#######     some of these are atomized categorical variables that need to be combined 
#######     into single factors. Others need to be better documented. These function 
#######     help label groups and extract group levels to help convert raw qualtrics 
#######     files into a format that is more intuitive for the end users of the dataset.
#######


# Same details as find_all_groups() below except it returns a vector
# of group names congruent with the input vector of variable names 
# (instead of the smaller list of unique groups), so that it can
# be appended as a column in the data dictionary. 

append_groups <- function( vname ) {

  get_prefix <- function(x) {
    L <- strsplit( x, "_" )
    s <- (unlist(L))[1]
    return( s )
  }

  f <- purrr::map_chr( vname, get_prefix )
  return( f )
}

# d$group <- append_groups( vname=d$qname )



# Find all unique question groups by parsing question strings and identifying 
# those with the same prefix and different endings (ceo_1, ceo_2, etc.).

# Arguments:
#   q: A vector of the variable name strings exported from qualtrix.

# Returns:
#   A set of the unique variable groups in the dataset returned as a character vector.


find_all_groups <- function( q ) {

  f <- function(x) {
    L <- strsplit( x, "_" )
    s <- (unlist(L))[1]
    return( s )
  }

  v <- purrr::map_chr( q, f )
  these <- grepl( "_", x )
  g <- unique( v[ these ] )
  return( g )
}

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
# [31] "LastName"          "Title"







####### 
#######   PARSE RESPONSE CATEGORIES FROM FIELD DESCRIPTION 
#######

# Extract response categories from the qualtrics questions embedded in the data exports. 
# The questions are first grouped by the question family then the get_categories() function
# is applied. It works by comparing strings to identify the base question component, then
# returning the difference. 
#
# Input strings (x's are common across all questions): 
#   xxxxx aaa xxx
#   xxxxx bb xxx
#   xxxxx ccccc xxx
#
# Output strings: 
#   aaa
#   bb
#   ccccc
#
# Examples: 
# q1: "Did you SEEK or APPLY for this funding? - Local government grants - (select all that apply)"
# q2: "Did you SEEK or APPLY for this funding? - State government grants - (select all that apply)"
# q3: "Did you SEEK or APPLY for this funding? - Federal government grants - (select all that apply)"
# q4: "Did you SEEK or APPLY for this funding? - Local government contracts or fee-for-service payments (other than Medicare/Medicaid) - (select all that apply)"
#
# get_categories(v)
# [1] "Local government grants"                                                              
# [2] "State government grants"                                                              
# [3] "Federal government grants"                                                            
# [4] "Local government contracts or fee-for-service payments (other than Medicare/Medicaid)"
# 
# Arguments:
#   v: The survey questions extracted from a qualtrics data exportj (pre-grouped by question family).

# Returns:
#   A vector of the extracted categories (e.g. response categories for factor variables).

get_categories <- function( v ) {

  min.char <- min( nchar(v) )
  v2 <- sapply( v, function(x){ substr(x,1,min.char) }, USE.NAMES=F )

  dds <- 
    sapply( 
       v2, 
       function(x){ strsplit(x,"") |> unlist() }, 
       USE.NAMES=F ) |> 
    as.data.frame()

  here <- apply( dds, MARGIN=1, function(x){ length(unique(x)) > 1 } ) |> which(TRUE) |> min()
  vv <- sapply( v, function(x){ substring( x, first=here ) }, USE.NAMES=F )
  
  backwards <- function(x) {
    s <- strsplit(x,"") |> unlist()
    b <- paste0( rev(s), collapse="" )
    return(b)
  }

  rv <- sapply( vv, backwards, USE.NAMES=F )

  min.char <- min( nchar(rv) )
  rv2 <- sapply( rv, function(x){ substr(x,1,min.char) }, USE.NAMES=F )

  ddr <- 
    sapply( 
       rv2, 
       function(x){ strsplit(x,"") |> unlist() }, 
       USE.NAMES=F ) |> 
    as.data.frame()

  here <- apply( ddr, MARGIN=1, function(x){ length(unique(x)) > 1 } ) |> which(TRUE) |> min()
  vv2 <- sapply( vv, function(x){ substring( x, first=1, last=nchar(x)-here+1 ) }, USE.NAMES=F )

  return(vv2)

}



# x1 <- "Did you SEEK or APPLY for this funding? - Local government grants - (select all that apply)"
# x2 <- "Did you SEEK or APPLY for this funding? - State government grants - (select all that apply)"
# x3 <- "Did you SEEK or APPLY for this funding? - Federal government grants - (select all that apply)"
# x4 <- "Did you SEEK or APPLY for this funding? - Local government contracts or fee-for-service payments (other than Medicare/Medicaid) - (select all that apply)"
# 
# v <- c(x1,x2,x3,x4)
# 
# get_categories(v)
# 
# 
# [1] "Local government grants"                                                              
# [2] "State government grants"                                                              
# [3] "Federal government grants"                                                            
# [4] "Local government contracts or fee-for-service payments (other than Medicare/Medicaid)"





####### 
#######   GATHER ATOMIZED CATEGORICALS INTO FACTOR
#######


# Arguments:
#   d: A data frame.
#   cols: Columns to factorize. Can be specified by column names (character) or column positions (numeric).
#   labels: Labels for the factor levels. If NULL, column names are used.
#   exhaustive: Logical indicating whether factorization is exhaustive (all rows sum to 1 or NA) or not (all rows sum to 0, 1, or NA).

# Returns:
#   A factor vector.

factorize <- function( d, cols, labels=NULL, exhaustive=TRUE ) {

  # select by column position (numeric)
  if( is.character(cols) )
  { dsub <- d[cols] }

  # select by column names (character)
  if( is.numeric(cols) )
  { dsub <- d[,cols] }

  # use column names as labels?
  if( is.null(labels) )
  { labels <- names(dsub) }

  if( ! ncol(dsub) == length(labels) )
  { stop("Each column needs a label") }

  # all columns are boolean or NA 
  cols.binary <- sapply( dsub, function(x){ all(x%in%c(0,1,NA)) }, simplify=T )
  if( ! all( cols.binary ) )
  { stop("Selected columns not Boolean; values must be in {0,1,NA}") }

  # responses are mutually exclusive and exhaustive; all rows sum to 1 or NA
  if( exhaustive )
  {
    checksum <- apply( dsub, MARGIN=1, FUN=function(x){ sum(as.numeric(x)) } )
    checksum <- checksum |> na.omit() |> as.numeric()
    if( ! all( checksum == 1 ) )
    { stop("Categories are not mutually exclusive and exhaustive") }
  }

  # responses are mutually exclusive; all rows sum to 0, 1 or NA
  if( ! exhaustive )
  {
    checksum <- apply( dsub, MARGIN=1, FUN=function(x){ sum(as.numeric(x)) } )
    checksum <- checksum |> na.omit() |> as.numeric()
    if( ! all( checksum %in% c(0,1) ) )
    { stop("Categories are not mutually exclusive") }
  }
 
  check.all.zero <- apply( dsub, MARGIN=1, FUN=function(x){ all( x == 0 ) } )
  if( any( na.omit(check.all.zero) ) )
  { labels <- c(labels,"NONE") }

  sum_rows <- function(x){ 
    y <- which( x == 1 )
    y[ all( x == 0 ) ] <- "NONE" # all zeros
    y[ length(y) < 1 ] <- NA     # no ones 
    return(y)
  }

  items <- apply( dsub, MARGIN=1, FUN=sum_rows )
  # items <- apply( dsub, MARGIN=1, FUN=function(x){which(x == 1)} )

  f <- factor( items, labels=labels )

  return(f)
}


  
# d <- 
#   data.frame( 
#     id=c(1,2,3,4,5,6,7,8,9), 
#     red=c("1","0","0","0","1","0","0","0","0"),
#     blue=c("0","1",NA,"1","0","1","1","1","0"),
#     yellow=c("0","0","0","0","0","0","0","0","1") )
# 
# # select columns by position
# factorize( d, cols=2:4 )
# 
# # select columns by name
# f.cols <- c("red","blue","yellow")
# factorize( d, cols=f.cols )
# 
# # provide category labels instead of column names
# factorize( d, cols=f.cols, labels=c("Lab1","Lab2","Lab3") )
# 
# # number of labels must match number of columns 
# factorize( d, cols=f.cols, labels=c("Lab1","Lab2") )
# 
# 
# 
# d <-             # test NAs
#   data.frame( 
#     id=c(1,2,3,4,5,6,7,8,9), 
#     red=c("1","0","0","0","1","0","0","0","0"),
#     blue=c("0","1",NA,"1","0","1","1","1","0"),
#     yellow=c("1","0","0","0","0","0","0","0","1") )
# 
# factorize( d, cols=f.cols )
# 
# 
# 
# 
# d <-            # not all rows sum to 1
#   data.frame( 
#     id=c(1,2,3,4,5,6,7,8,9), 
#     red=c("0","0","0","0","1","0","0","0","0"),
#     blue=c("0","1",NA,"1","0","1","1","1","0"),
#     yellow=c("0","0","0","0","0","0","0","0","1") )
# 
# factorize( d, cols=f.cols )
# factorize( d, cols=f.cols, exhaustive=F )
# 
# 
# 
# 
# d <-            # value outside {0,1}
#   data.frame( 
#     id=c(1,2,3,4,5,6,7,8,9), 
#     red=c("1","0","0","0","1","0","0","0","0"),
#     blue=c("0","1",NA,"1","0","1","1","1","0"),
#     yellow=c("2","0","0","0","0","0","0","0","1") )
# 
# factorize( d, cols=f.cols )
# 


