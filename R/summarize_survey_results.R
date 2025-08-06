# Helper functions for nptrends year 4 and 5 survey analysis

#' @title Main survey analysis function
#' 
#' @description This function performs the filter, groupby and summary functions for each metric based on user parameters. It nests the groupbys based on conditional logic and computes either weighted median, weighted mean or proportion
#' 
#' @param groupby_2_vec character vector. additional groupby variables for nesting
#' @param groupby_1 character scalar. first groupby for aggregation
#' @param metric character scalar. Name of survey variable to analyse
#' @param method character scalar. Name of summary statistic to compute
#' @param df data.frame. Data.frame containing survey data
#' 
#' @return data.frame of summary statistic for each subgroup
svy_tfm <- function(groupby_2_vec,
                    groupby_1,
                    metric,
                    method,
                    df) {
  df_filtered <- dplyr::filter(df, !is.na(.data[[metric]]))
  df_grouped <- svy_grpby(groupby_1, "filterOpt", df_filtered)
  df_summarised <- purrr::map(
    .x = groupby_2_vec,
    .f = function(groupby_2) {
      wt <- svy_wt(groupby_2)
      weighted_count <- sum(df_grouped[[wt]], na.rm = TRUE)
      # Optional nesting for subgroups
      if (groupby_2 != groupby_1) {
        df_nested <- svy_grpby(groupby_2, "splitByOpt_category", df_grouped)
      } else {
        df_nested <- df_grouped
      }
      # Optional nesting for categorical variables
      if (method == "% of respondents") {
        df_nested <- svy_grpby(metric, "responseOpt", df_nested)
        current_metric <- "responseOpt"
      } else {
        df_nested <- df_nested
        current_metric <- metric
      }
      # Summary steps
      df_summarised <- df_nested |>
        dplyr::summarise(
          num_responses = sum(!!sym(wt), na.rm = TRUE),
          value = {
            # Determine if there are enough non-NA records
            x_values <- .data[[current_metric]]
            w_values <- .data[[wt]]
            valid_rows <- validate_metric(x_values, w_values)
            if (valid_rows == 0) {
              NA_real_
            }
            else {
              if (method == "median") {
                spatstat.univar::weighted.median(
                  x = !!sym(current_metric),
                  w = !!sym(wt),
                  na.rm = TRUE
                )
              } else if (method == "average of %") {
                stats::weighted.mean(
                  x = !!sym(current_metric),
                  w = !!sym(wt),
                  na.rm = TRUE
                )
              } else if (method == "% of respondents") {
                sum(!!sym(wt), na.rm = TRUE)
              }
            }
            
          }   ,
          .groups = 'drop'
        ) %>% 
        # Add remaining columns
        dplyr::mutate(
          filterType = {{groupby_1}},
          splitByOpt = {{groupby_2}},
          weight = {{wt}},
          metricname = {{metric}})
      if (! "splitByOpt_category" %in% names(df_summarised)){
        df_summarised$splitByOpt_category <- "None"
      }
      # Get weighted counts of the number of survey respondents
      df_summarised <- df_summarised |>
        dplyr::group_by(splitByOpt_category) |>
        dplyr::mutate(num_responses = sum(num_responses, na.rm = TRUE)) |>
        dplyr::ungroup()
      # Get weighted proportions for each category for categorical variables
      if (method == "% of respondents") {
        df_summarised <- df_summarised |>
          dplyr::group_by(splitByOpt_category) |>
          dplyr::mutate(value = value / sum(value, na.rm = TRUE)) |>
          dplyr::ungroup()
      }
      df_final <- recode_metric(current_metric, df_summarised)
      return(df_final)
    }
  ) |> purrr::list_rbind()
  return(df_summarised)
}

#' @title Validate survey variable before computing summary statistics
#' 
#' @description This function checks if there are enough non-NA values to compute summary statistics
#' 
#' @param x_values numeric vector. Values of the survey variable
#' @param w_values numeric vector. Weights for the survey variable
#' 
#' @return integer. Number of valid rows that can be used for summary statistics
validate_metric <- function(x_values, w_values) {
  sum(!is.na(x_values) &
        !is.na(w_values) & is.finite(w_values) & w_values >= 0)
}

#' @title survey-specific groupby function
#' 
#' @description This function groups the data frame by a specified variable and renames it
#' 
#' @param grpby_var character scalar. The variable to group by
#' @param newname character scalar. The new name for the grouped variable
#' @param df data.frame. The data frame to group
#' 
#' @return data.frame. The grouped data frame with the renamed variable
svy_grpby <- function(grpby_var, newname, df) {
  df <- df |>
    dplyr::group_by(!!sym(grpby_var), .add = TRUE) |>
    dplyr::rename({{newname}} := !!sym(grpby_var))
  return(df)
}

#' @title survey-specific weight function
#' 
#' @description This function returns the appropriate weight variable based on the groupby variable
#' 
#' @param grpby character scalar. The groupby variable
#' 
#' @return character scalar. The name of the weight variable to use
svy_wt <- function(grpby) {
  if (grpby == "state") {
    wt <- "stateweight"
  }
  else {
    wt <- "year4wt"
  }
  return(wt)
}

#' @title Recode survey variable being analysed
#' 
#' @description This function recodes the survey variable being analysed to a standard name based on whether it is a categorical response
#' 
#' @param metric character scalar. The survey variable being analysed
#' @param df data.frame. The data frame containing the survey data
#' 
#' @return data.frame. The data frame with the recoded survey variable
recode_metric <- function(metric, df) {
  if (metric != "responseOpt") {
    df <- dplyr::mutate(responseOpt = {{metric}}, df)
  } else {
    df <- dplyr::mutate(responseOpt = as.character(responseOpt), df)
  }
  return(df)
}

#' Calculate a combined binary flag vector-wise for specific columns.
#'
#' @param .data The input data frame.
#' @param ... Quosures of column names (e.g., `c(col1, col2, col3)`).
#'   These should be columns containing 0s, 1s, or NAs.
#' @return A numeric vector (0, 1, or NA_real_).
binary_flag <- function(data, ...) {
  # Capture the column names as symbols
  # Select the relevant columns
  cols <- rlang::enquos(...)
  selected_data <- data |> dplyr::select(!!!cols)
  
  # Convert to a matrix for easier row-wise processing in a vectorized way
  # Or iterate row by row using apply if memory is an issue for large matrices
  # For 0/1/NA data, a matrix is often efficient.
  mat <- as.matrix(selected_data)
  
  # Apply the logic row-wise using apply
  apply(mat, 1, function(row_values) {
    any_is_one <- any(row_values == 1, na.rm = TRUE)
    all_are_zero <- all(row_values == 0, na.rm = FALSE)
    has_non_na <- any(!is.na(row_values))
    
    if (any_is_one) {
      1
    } else if (all_are_zero && has_non_na) {
      0
    } else {
      NA_real_
    }
  })
}