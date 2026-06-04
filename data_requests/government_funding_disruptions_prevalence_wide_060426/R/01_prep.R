# Build survey designs and the test runners for the wider government-funding-
# disruption request. Inputs come from 00_load.R: y5, y6, y6staff, template.
#
# This request asks for only two test families:
#   * confidence interval — design-based weighted proportion CI (logit method),
#     either overall (no disaggregation) or per level of a disaggregator.
#   * chi-square          — design-based Rao-Scott chi-square of a 0/1 indicator
#     against a disaggregator.
#
# Three single-population designs (all weighted, ids = ~1 — no PSU/strata are
# available in the cleaned files; see critique.md §"Survey design"):
#   des_y5      — Year 5,            weight year5wt
#   des_y6      — Year 6,            weight weight_year6plus
#   des_staffed — Year 6 staffed,    weight weight_year6plus
#
# YEAR-COMPARISON ROWS (template rows 35, 36, 49, 50, 63, 64) are special. The
# template labels them "chi-square" with disaggregation
#   "year 5 vs [year 6 where LLAnyDisruption == 0] vs [year 6 where LLAnyDisruption == 1]".
# Per the client's decision we DO NOT run a pooled between-year chi-square on
# these (the Y5/Y6 samples share a large panel — 1,859 orgs recur across waves,
# 518 in all three — which violates the independence the chi-square assumes, and
# the two waves carry separately-calibrated weights). Instead we report each
# group's weighted proportion with its own confidence interval and let the
# reader compare — the "descriptive" option. See README.md and critique.md.
#
# PrgSrvc_Amt_* DENOMINATOR RULE (from data_dictionary_20260527.xlsx): response
# codes are 1 = significant decrease, 2 = moderate decrease, 3 = no change,
# 4 = moderate increase, 5 = significant increase, 97 = N/A, 98 = unsure. The
# dictionary instruction is "Keep 97 in the denominator but remove 98 from the
# denominator." So the analysis universe for any PrgSrvc_Amt_* variable is
# {1,2,3,4,5,97}; 98 and NA are dropped. All other variables use their full
# non-missing universe.

library(survey)

# --- survey designs ----------------------------------------------------------

des_y5 <- survey::svydesign(ids = ~1, weights = ~year5wt,          data = y5)
des_y6 <- survey::svydesign(ids = ~1, weights = ~weight_year6plus, data = y6)
des_staffed <- survey::svydesign(ids = ~1, weights = ~weight_year6plus,
                                 data = y6staff)

#' Map the template `dataset` cell to one of the single-population designs.
pick_design <- function(dataset) {
  ds <- tolower(trimws(dataset))
  if (ds == "nptrends_y5_clean")          return(des_y5)
  if (ds == "nptrends_y6_clean")          return(des_y6)
  if (ds == "nptrends_y6_staffed_clean")  return(des_staffed)
  stop("Unrecognized dataset: ", dataset)
}

# --- helpers -----------------------------------------------------------------

#' Parse a `response` cell into the integer codes that count as a "hit".
#' Handles "1", "97", and "1 and 2 combined" / "4 and 5 combined".
parse_responses <- function(response) {
  as.integer(unlist(regmatches(response, gregexpr("[0-9]+", response))))
}

#' Logical universe for a variable: PrgSrvc_Amt_* keep {1,2,3,4,5,97} (drop 98
#' and NA per the dictionary); everything else keeps all non-missing values.
in_universe <- function(vals, variable) {
  if (startsWith(variable, "PrgSrvc_Amt")) vals %in% c(1, 2, 3, 4, 5, 97)
  else !is.na(vals)
}

#' Attach a 0/1 `.indicator` (= variable code is one of `responses`) restricted
#' to the variable's universe, and return the universe-subset design.
prep_indicator <- function(design, variable, responses) {
  v   <- design$variables[[variable]]
  uni <- in_universe(v, variable)
  design$variables$.indicator <- as.integer(v %in% responses)
  subset(design, uni)
}

#' Format one CI line: "label: est [lo, hi] (n=k)".
fmt_ci <- function(label, est, lo, hi, n) {
  sprintf("%s: %.3f [%.3f, %.3f] (n=%d)", label, est, lo, hi, as.integer(n))
}

#' Format a p-value: scientific notation for very small values so significance
#' is not lost to rounding (e.g. 3.96e-18 instead of 0.0000), 4 decimals else.
fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 1e-4) sprintf("%.2e", p) else sprintf("%.4f", p)
}

# Each runner returns list(result = <string for the xlsx>, tidy = <data.frame>).
# The tidy frame is the machine-readable evidence (one row per estimate/test)
# assembled into the long-format CSV by 03_report.R.

# --- confidence interval -----------------------------------------------------

run_ci <- function(design, variable, responses, disagg) {
  des <- prep_indicator(design, variable, responses)

  # Overall (no disaggregation) ----------------------------------------------
  if (is.na(disagg) || disagg == "") {
    ci  <- tryCatch(survey::svyciprop(~.indicator, des, method = "logit"),
                    error = function(e) e)
    if (inherits(ci, "error"))
      return(list(result = paste("ERROR:", conditionMessage(ci)), tidy = NULL))
    est <- as.numeric(ci); lh <- as.numeric(attr(ci, "ci"))
    se  <- as.numeric(survey::SE(ci))
    n   <- nrow(des$variables)
    res <- fmt_ci("overall", est, lh[1], lh[2], n)
    tidy <- data.frame(test_type = "confidence interval", group_var = NA_character_,
                       group_level = "overall", estimate = est, se = se,
                       ci_l = lh[1], ci_u = lh[2], n = n,
                       statistic = NA_real_, df = NA_real_, p_value = NA_real_,
                       stringsAsFactors = FALSE)
    return(list(result = res, tidy = tidy))
  }

  # Per-disaggregator-level ---------------------------------------------------
  des <- subset(des, !is.na(des$variables[[disagg]]))
  fml <- as.formula(sprintf("~%s", disagg))
  by  <- tryCatch(
    survey::svyby(~.indicator, fml, des, survey::svyciprop,
                  vartype = c("se", "ci"), method = "logit"),
    error = function(e) e
  )
  if (inherits(by, "error"))
    return(list(result = paste("ERROR:", conditionMessage(by)), tidy = NULL))
  ns  <- table(des$variables[[disagg]])
  lvl <- as.character(by[[1]])
  nv  <- as.integer(ns[lvl])
  res <- paste(
    mapply(fmt_ci, sprintf("%s=%s", disagg, lvl), by$.indicator, by$ci_l, by$ci_u, nv,
           USE.NAMES = FALSE),
    collapse = " | "
  )
  tidy <- data.frame(test_type = "confidence interval", group_var = disagg,
                     group_level = lvl, estimate = by$.indicator, se = by$se,
                     ci_l = by$ci_l, ci_u = by$ci_u, n = nv,
                     statistic = NA_real_, df = NA_real_, p_value = NA_real_,
                     stringsAsFactors = FALSE)
  list(result = res, tidy = tidy)
}

# --- chi-square --------------------------------------------------------------

run_chisq <- function(design, variable, responses, disagg) {
  des <- prep_indicator(design, variable, responses)
  des <- subset(des, !is.na(des$variables[[disagg]]))
  fml <- as.formula(sprintf("~.indicator + %s", disagg))
  res <- tryCatch(survey::svychisq(fml, design = des, statistic = "Chisq"),
                  error = function(e) e)
  if (inherits(res, "error"))
    return(list(result = paste("ERROR:", conditionMessage(res)), tidy = NULL))
  stat <- as.numeric(res$statistic); df <- as.numeric(res$parameter[1])
  p    <- as.numeric(res$p.value);   n  <- nrow(des$variables)
  out  <- sprintf("X^2(%.0f) = %.3f, p = %s, n = %d", df, stat, fmt_p(p), n)
  tidy <- data.frame(test_type = "chi-square", group_var = disagg,
                     group_level = NA_character_, estimate = NA_real_, se = NA_real_,
                     ci_l = NA_real_, ci_u = NA_real_, n = n,
                     statistic = stat, df = df, p_value = p,
                     stringsAsFactors = FALSE)
  list(result = out, tidy = tidy)
}

# --- year comparison (descriptive: one weighted-proportion CI per group) ------

run_year_compare <- function(variable, responses) {
  groups <- list(
    list(label = "year5",            design = des_y5, filt = NULL),
    list(label = "year6_LLAny0",     design = des_y6, filt = 0),
    list(label = "year6_LLAny1",     design = des_y6, filt = 1)
  )
  parts <- character(0); tidies <- list()
  for (g in groups) {
    des <- prep_indicator(g$design, variable, responses)
    if (!is.null(g$filt)) {
      keep <- des$variables[["LLAnyDisruption"]] == g$filt
      keep[is.na(keep)] <- FALSE
      des <- subset(des, keep)
    }
    ci  <- tryCatch(survey::svyciprop(~.indicator, des, method = "logit"),
                    error = function(e) e)
    if (inherits(ci, "error")) {
      parts <- c(parts, paste0(g$label, ": ERROR ", conditionMessage(ci)))
      next
    }
    est <- as.numeric(ci); lh <- as.numeric(attr(ci, "ci"))
    se  <- as.numeric(survey::SE(ci))
    n   <- nrow(des$variables)
    parts <- c(parts, fmt_ci(g$label, est, lh[1], lh[2], n))
    tidies[[length(tidies) + 1]] <- data.frame(
      test_type = "confidence interval (year comparison)",
      group_var = "year_group", group_level = g$label, estimate = est, se = se,
      ci_l = lh[1], ci_u = lh[2], n = n,
      statistic = NA_real_, df = NA_real_, p_value = NA_real_,
      stringsAsFactors = FALSE)
  }
  list(result = paste(parts, collapse = " | "),
       tidy   = if (length(tidies)) do.call(rbind, tidies) else NULL)
}

# --- dispatch ----------------------------------------------------------------

#' Route one template row to the right runner. Year-comparison rows (combined
#' Y5+Y6 dataset) are always handled descriptively, regardless of the row's
#' "chi-square" label — see the header note.
dispatch <- function(variable, dataset, response, disagg, stats_test) {
  responses <- parse_responses(response)
  is_year   <- grepl("and", dataset) && grepl("year", tolower(disagg))
  if (is_year) return(run_year_compare(variable, responses))

  design <- pick_design(dataset)
  tag    <- tolower(trimws(stats_test))
  if (startsWith(tag, "confidence interval"))
    return(run_ci(design, variable, responses, disagg))
  if (startsWith(tag, "chi"))
    return(run_chisq(design, variable, responses, disagg))
  list(result = paste("UNRECOGNIZED stats test:", stats_test), tidy = NULL)
}
