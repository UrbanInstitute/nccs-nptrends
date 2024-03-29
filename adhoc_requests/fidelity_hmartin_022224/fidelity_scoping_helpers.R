get_multi_variable_count <- function( variables, data, year, region ){
  varcounts_df <- purrr::map(
    .x = variables,
    .f = get_single_variable_count,
    dataset = data
  ) |> purrr::list_rbind()
  varcounts_df$`Year` <- as.character(year)
  varcounts_df$`Region` <- as.character(region)
  return(varcounts_df)
}


get_single_variable_count <- function(var_name, dataset, missing_vals){
  df <- dataset %>% 
    dplyr::select(tidyselect::all_of(var_name)) %>% 
    dplyr::filter(! .data[[var_name]] %in% missing_vals) %>% 
    tidyr::drop_na() %>% 
    dplyr::summarize("Number of Responses" = sum(! is.na(.data[[var_name]])),
                     "Variable Name" = var_name)
  return(df)
}
