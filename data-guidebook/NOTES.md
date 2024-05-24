### READABILITY OF RESEARCH GUIDEBOOK

I tried to find ways to make the guidebook accessible to non-R programmers. To me that means being able to read the code, and even if you aren't familiar with the specific functions you can make sense of the operations. 

One thing I have find confusing about case_match is flips the direction of assignment and uses an ambiguous operator:

```r
# RIGHT TO LEFT ASSIGNMENT

x <- val

fx( y=3 )

df %>% rename( new = old )

# LEFT TO RIGHT ASSIGNMENT

df %>% 
  case_match(  "C"   ~ 1, 
               "N/A" ~ 0    )
```

It's not clear if you are replacing "C" with "1", or "1" with "C". 

I wrote some helper functions so that the rules for recoding variables and adding labels to make it easier to leverage analysts.  

```r
# RECODING RULES 

RULES <- c(   "     C    =>>     1   ",
              "   -99    =>>     0   ",
              "   N/A    =>>     1   "     )

# APPLY TO COLUMNS K:

COLUMNS <- na_bool_qns

# SURVEY LABELS AND MISSING VALUE CODES

values  <-  c( "0", "1" )
labels  <-  c( "No", "Yes" )
missing <-  NULL

# RECODE VARIABLES

survey_df <-  recode_columns( df, COLUMNS, RULES, values, labels, missing  )
```


### CODE READABILITY

There is a long German word meaning "law delegating beef label monitoring":

Rindfleischetikettierungsueberwachungsaufgabenuebertragungsgesetz 

The code equivalent would be many nested functions:

```r
mean(na.omit(sample(rep(1:5,each=10),100,replace=T)))
```

When working with your code I found that:

(1) Per usual, it is technically excellent and you are thoughtful about edge cases. Thank you. 

(2) You have to read a lot of these functions from the middle out instead of top-down, which means they can be tricky to debug when you don't understand which value is being passed to which level of nested functions. 

```r
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
```

This is less efficient but more readable (I believe): 

```r
# FUNCTIONS TO CONVERT SURVEY VARIABLE TO 
# SURVEY.ITEM WITH MISSINGNESS AND VARIABLE LABELS
              
as_item <- function( v, labeled_vals, missing_vals ){

   v <- memisc::as.item( v,
          labels = labeled_vals,
          missing.values = missing_vals  )
          
   return(v)
}

create_survey_item <- function(survey_data, qns, recode_vals, recode_labs, missing_vals){

  labeled_vals <- 
    structure(  .Data  =  recode_vals,
                 names =  recode_labs    )
                             
  survey_data <- 
    survey_data |>
    purrr::modify_at(
      .at =  qns,
      .f  =  as_item, labeled_vals, missing_vals   )
    
  return( survey_data )
}               
```

Not sure why dplyr::select now requires tidyselect::any_of/all_of to select columns? Don't forget about base R:

```r
codebook(
  df %>% 
    dplyr::select(
      tidyselect::all_of( qns )
    )
)

# VS

codebook( df[ qns ] )
```

Similarly, when manipulating columns: 

```r
# PLUCK X

x <- df[[ "EIN" ]]

# APPLY FUNCTION TO COLUMNS: 

fx <- function(x){ ... }
df    <- purrr::modify_at( df, k, fx )
df[k] <- lapply( df[k], fx )

# LIST OF COLUMN NAMES: 

f <- function( x.txt, df ){
  x <- df[[x.txt]]
  some_fx( x )
}

df[k] <- purrr:map( k, f, df )
df[k] <- lapply( k, f, df )
```



### CREATING PUBLIC USE FILES 

When creating files to share we might consider replacing NAs with empty cells, which makes the CSV files more universal. 

```r
write.csv( survey_df, "file.csv", na="" )  
```

I also updated the GROUP column in the data dictionary file so that it includes DROP and ID categories. All non-necessary qualtrics variables are labeled DROP and all variables that can be used to identify respondents are labeled ID. 

It might be helpful to create IDs from EINs so that if we need to merge public use files with other files it would be fairly straight-forward. 

```r
id <- paste0( "ORG-", hash::hash( EIN ) )
```



### PASSING OBJECTS AND DATA BETWEEN CHAPTERS

One challenge in working with the qualtrics book format is that knitting a single chapter launches a new environment, which causes problems when trying to use the same objects across chapters (files).

There are a few solutions such as writing results to the intermediate files library at the end of a chapter, then starting the next chapter by reading those files: 

```
+- data-raw
¦  +-- qualtrics_export.csv
¦
+- data-intermediate
¦  +-- step-01-output.csv
¦  +-- step-02-output.csv
¦
+-- data-final
   +-- restricted.csv
   +-- puf.csv
```

One challenge is that the objects we care about are not always datasets. For example, the groups of survey questions used in many of the steps. These can be easily written to file using dump(): 

```r
dump( list=c("fundraise_skrcv_qns_bool","na_bool_qns"), 
      file = "temp/var_groups.R" )
```

And loaded as needed: 

```r
source( "temp/var_groups.R" )
```

Another solution would be to preserve the entire environment: 

```	
save.image( file='temp/step-01-output.RData' )
```

Which all work fine, but they can cause problems in knowing whether all of the data dependencies have been updated when working on a chapter. One solution is to source a previous chapter when the current chapter requires the same resources, which can be done fairly easily with the purl() function. It converts an RMD or QMD file into an R script by dropping regular text (you can also keep it as comments if desired) and only keeping code from code chunks.

For example, to source the chapter 20-YEAR-TWO.qmd file: 

```r
knitr::purl( "20-YEAR-TWO.qmd" )   # convert QMD to R script
source(      "20-YEAR-TWO.R" )     # run all chunks in prior step
```

The only issue I encountered is that it keeps ALL code within chunks, not just R chunks. As a result, I had to convert your epoxy chunk to regular markdown (there was nothing wrong with it otherwise, if you were wondering why I bothered): 

````
```{epoxy}
- **{length(c(program_change_qns_bool, program_change_qns_txt))}** questions about changes to programs and services
```

- **`r length(c(program_change_qns_bool, program_change_qns_txt))`** questions about changes to programs and services
````


### FULL BOOK RENDER

The big limitation is duplication of tasks, especially if we are knitting the entire book. And more so if the input files are not small survey files. 

For example, if CH2 sources CH1, and CH3 sources CH2, then knitting the full book would require CH1 to execute 3 times and CH2 to execute 2 times: 

```
render CH1
render CH2 ->> source CH1
render CH3 ->> source CH2 ->> source CH1
```

The workaround is to add a parameter to the chapter header, which can be ignored by passing a new version when rendering the full book: 

```r
---
  params:
    chain: TRUE
---

# RUNS WHEN YOU RENDER FROM RSTUDIO:
if( chain ){
  knitr::purl( "20-YEAR-TWO.qmd" ) 
  source(      "20-YEAR-TWO.R" )    
}

# DOESN'T RUN WHEN YOU RENDER THE FULL BOOK: 
quarto::quarto_render(
  input = here::here("quarto"), 
  output_format = "html", 
  execute_params = list( chain=FALSE) )
```

I have not implemented this yet, though. 




### SAVING MEMISC FILES 

I noticed that these two functions produced different results: 

```r
write.csv( survey_df, "file.csv", na="" )  # preserves all of the labels, replaces missing value categories with NA
write_csv( survey_df, "file.csv" )         # drops labels, keeps missing value categories
```

I'm assuming it's because write.csv has an memisc class, whereas write_csv does not? 

They both drop all of the useful metadata captured by memisc files. 

I poked arond a bit and could not find a way to write memisc files to save them, but they can be preserved as R objects in the usual RDS structure. I experimented with preserving both in case we want to use the memisc object for documentation in subsequent steps. 

```
+- data-intermediate
¦  +-- step-01.csv    # CSV file
¦  +-- step-01.rds    # MEMISC object
```









 

