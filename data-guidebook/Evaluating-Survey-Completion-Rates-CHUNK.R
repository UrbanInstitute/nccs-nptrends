
## Evaluating Survey Completion Rates and Summing Section Completion

# We next evaluate survey completion rates for each category of questions. We separate our variables into question categories and compute the proportion of valid responses. 
# 
# ```{r, warning=FALSE}

fpath <- "DATA-PREP/01-year-one/02-data-intermediate/"
fname <- "wave-01-data-intermediate.dta"
weighted_survey_df <- haven::read_dta( paste0(fpath,fname) )


# Create named list of variables

survey_completion_qns_ls <- list(
  "Program Changes" = program_change_qns_bool,
  "People Served" = people_served_qns_int,
  "Next Year's Demand" = demand_fct_qns,
  "Staff & Volunteers" = staff_qns_int,
  "Volunteer Importance" = volimportance_qns_fct,
  "Donor Importance" = donimportance_qns_fct,
  "Fundraising" = fundraise_qns_bool,
  "Major Gift Amount" = majorgift_qn_num,
  "Fundraising Changes" = fundraise_change_qns_fct,
  "Donor Changes" = fundraise_donor_qns_int,
  "Fundraising Sought/Received" = fundraise_skrcv_qns_bool,
  "Revenue Breakdown" = finance_revenue_qns_num,
  "Reserves" = reserve_qns_num,
  "CARES Funding" = cares_qns_num,
  "Finance Changes" = finance_chng_qns_bool,
  "Leadership Changes" = leadership_chng_qns_bool,
  "Race and Gender" = c(race_gender_qns_bool, "BChairrace_Bi", "CEOrace_Bi"),
  "External Affairs" = extaffairs_qns_fct,
  "Primary Concern" = primary_cncrn_qn_text)
# Include Response IDs into questions
survey_completion_qns <- c("ResponseID", unname(unlist(survey_completion_qns_ls)))
# Subset Survey to only include questions to calculate response rates
survey_subset <- survey_df %>% 
  dplyr::select(
    tidyselect::all_of(survey_completion_qns)
  )
# Compute response rate
response_rate_ls <- purrr::imap(
  .x = survey_completion_qns_ls,
  .f = function(qns, qn_group) {
    # Compute responses for each question belonging to a group
    sum_ls <- purrr::map(qns,
                         .f = function(qn){
                           rs <- as.integer(survey_subset[[qn]])
                           rs <- ifelse(! is.na(rs), 1, 0)
                           return(rs)
                         })
    # Perform rowwise sum for questions belonging to a group to get number of valid responses
    sum_qn <- purrr::pmap(sum_ls, sum, na.rm = TRUE)
    survey_subset[[qn_group]] <- unlist(sum_qn)
    # Compute response rate based on number of questions in that group
    output_df <- survey_subset %>% 
      dplyr::mutate(!!qn_group := .data[[qn_group]] / length(qns)) %>% 
      dplyr::select(tidyselect::all_of(c("ResponseID", qn_group)))
    return(output_df)
  },
  .progress = TRUE
)

response_rate_df <- purrr::reduce(response_rate_ls,
                                  dplyr::left_join,
                                  by = "ResponseID")

# Exclude Qualtrics Test Responses
testcases = c(
  "R_1lB6u4BoFZWFeXI",
  "R_3e3RyQttJeNzqLu",
  "R_3kzeIxsE73IIoq9",
  "R_2BxNQBmrv4Dr9Dn",
  "R_Q4WjxQdBfeZbn4B",
  "R_xFNLuOjPbg6iL6N",
  "R_3McyCOS2Gv7FNcd",
  "R_9YWW00fssFUBEYh",
  "R_PNiTwNoQSfvuRkB",
  "R_111PYLkD7mpeFui",
  "R_a59rvIufWBZhAhr",
  "R_3MGb8by31yzxYW7",
  "R_1exbPd4NNpfWrFt",
  "R_2PwfUW9idAwPE1w",
  "R_3EunbLVD5Ce8b93",
  "R_2uO55NoBulnQTvx",
  "R_Wl2yqsO0HXhhaTv",
  "R_1kSRidkXneLKy4u",
  "R_3j3DCg6oVhQWRe0"
)

response_rate_df <- 
  response_rate_df %>% 
  dplyr::filter(! ResponseID %in% weighted_survey_df$ResponseId )

response_rate_df %>% knitr::kable()
# ```
