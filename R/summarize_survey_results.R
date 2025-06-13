#' Summarize Survey Variable Across Multiple Groups
#'
#' This function applies weighted mean calculations to a survey variable across
#' multiple grouping variables. It processes each group-weight combination and
#' returns combined results.
#'
#' @param group_ls A named list where names are grouping variable names and
#'   values are weight variable names to use for each group
#' @param survey_df A data frame containing the survey data
#' @param survey_var Character string specifying the name of the survey variable
#'   to summarize
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{group}{Character vector of group values}
#'     \item{[survey_var]}{Numeric vector of weighted means (NA if group has < 25 observations)}
#'   }
#'
#' @details
#' The function calculates weighted means only for groups with 25 or more observations.
#' Groups with fewer observations receive NA values.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' group_weights <- list(
#'   "region" = "survey_weight",
#'   "age_group" = "age_weight"
#' )
#' results <- summarise_single_variable(
#'   group_ls = group_weights,
#'   survey_df = my_survey_data,
#'   survey_var = "satisfaction_score"
#' )
#' }
#'
#' @seealso \code{\link{summarise_single_group}}
#' @export
summarise_single_variable <- function(group_ls, survey_df, survey_var) {
  # Input validation
  if (!is.list(group_ls) || is.null(names(group_ls))) {
    stop("group_ls must be a named list")
  }
  
  if (!is.data.frame(survey_df)) {
    stop("survey_df must be a data frame")
  }
  
  if (!is.character(survey_var) || length(survey_var) != 1) {
    stop("survey_var must be a single character string")
  }
  
  # Process each group-weight combination
  single_var_results <- purrr::imap_dfr(
    group_ls,
    function(weight_var, group_var) {
      summarise_single_group(
        group = group_var,
        wt = weight_var,
        survey_df = survey_df,
        survey_var = survey_var
      )
    }
  )
  
  return(single_var_results)
}

#' Summarize Survey Variable for a Single Group
#'
#' This function calculates weighted means for a survey variable within groups
#' defined by a grouping variable, applying a minimum sample size threshold.
#'
#' @param group Character string specifying the name of the grouping variable
#' @param wt Character string specifying the name of the weight variable
#' @param survey_df A data frame containing the survey data
#' @param survey_var Character string specifying the name of the survey variable
#'   to summarize
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{group}{Character vector of group values}
#'     \item{[survey_var]}{Numeric vector of weighted means (NA if group has < 25 observations)}
#'   }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Groups data by the specified grouping variable
#'   \item Calculates weighted means only for groups with 25+ observations
#'   \item Returns NA for groups with fewer than 25 observations
#'   \item Converts group values to character for consistency
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- summarise_single_group(
#'   group = "region",
#'   wt = "survey_weight",
#'   survey_df = my_survey_data,
#'   survey_var = "satisfaction_score"
#' )
#' }
#'
#' @importFrom dplyr group_by summarise ungroup rename mutate n
#' @importFrom rlang sym
#' @export
summarise_single_group <- function(group, wt, survey_df, survey_var) {
  # Input validation
  if (!is.character(group) || length(group) != 1) {
    stop("group must be a single character string")
  }
  
  if (!is.character(wt) || length(wt) != 1) {
    stop("wt must be a single character string")
  }
  
  if (!is.data.frame(survey_df)) {
    stop("survey_df must be a data frame")
  }
  
  if (!is.character(survey_var) || length(survey_var) != 1) {
    stop("survey_var must be a single character string")
  }
  
  # Check if variables exist in the data frame
  missing_vars <- setdiff(c(group, wt, survey_var), names(survey_df))
  if (length(missing_vars) > 0) {
    stop(paste(
      "The following variables are not found in survey_df:",
      paste(missing_vars, collapse = ", ")
    ))
  }
  
  # Calculate weighted means with minimum sample size threshold
  summary_results <- survey_df |>
    dplyr::group_by(!!rlang::sym(group)) |>
    dplyr::summarise(
      !!survey_var := ifelse(
        dplyr::n() >= 25,
        stats::weighted.mean(!!rlang::sym(survey_var), !!rlang::sym(wt), na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    dplyr::rename(group = !!rlang::sym(group)) |>
    dplyr::mutate(group = as.character(group))
  
  return(summary_results)
}