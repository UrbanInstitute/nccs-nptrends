# PROTOTYPE — paired (panel) tests of year-over-year change.
#
# The template's six "year 5 vs year 6" rows are reported descriptively in the
# main pipeline (per-group CIs) because the cross-sectional Y5/Y6 samples share
# a panel and carry separately-calibrated weights, so a pooled between-year
# chi-square is not valid (see README §"Year-comparison rows are descriptive").
#
# This script is the *inferential alternative* for whoever wants a formal test
# of change: it links the same organizations across waves on EIN and applies the
# PAIRED members of the chi-square family —
#   * McNemar's test        — binary marginal homogeneity (e.g. "share reporting
#                             a decrease in program-service NUMBERS shifted Y5->Y6")
#   * Stuart-Maxwell test   — marginal homogeneity for the 3-category collapse
#                             (decrease / no change / increase)
# both of which account for the within-org correlation that the independence
# chi-square ignores.
#
# Reads: y5, y6 (00_load.R), in_universe() (01_prep.R).
# Writes: data/output/panel_tests.csv
#
# CAVEATS (carry these into any write-up):
#   1. UNWEIGHTED. There is no panel weight in the files (only cross-sectional
#      year5wt / weight_year6plus). These tests are unweighted counts on the
#      panel. A design-based weighted version needs a panel-weight decision from
#      whoever built the weights.
#   2. ATTRITION / SELECTION. The panel is the orgs that responded in BOTH waves
#      (n = 823 here), not a random subset — results generalize to repeat
#      responders, not the full population.
#   3. PERIOD-REFERENCED ITEMS. PrgSrvc_Amt_* ask about change vs the prior year,
#      so this compares the 2025 reference window to the 2026 reference window,
#      not a stable trait measured twice.

stopifnot(exists("y5"), exists("y6"), exists("in_universe"))

prg_vars <- c("PrgSrvc_Amt_Num", "PrgSrvc_Amt_Srvc", "PrgSrvc_Amt_Area")

# --- link the panel on EIN ---------------------------------------------------

y5p <- y5[, c("EIN", prg_vars, "year5wt"), with = FALSE]
y6p <- y6[, c("EIN", prg_vars, "weight_year6plus"), with = FALSE]
panel <- merge(y5p, y6p, by = "EIN", suffixes = c("_y5", "_y6"))
message(sprintf("Panel (orgs in both Y5 & Y6, linked on EIN): %d", nrow(panel)))

# --- Stuart-Maxwell test of marginal homogeneity for a k x k table -----------
# Statistic = d' V^-1 d on the first k-1 marginal differences, ~ chi-square_(k-1).
#   d_i  = row_i - col_i        (difference in i-th category's marginals)
#   V_ii = row_i + col_i - 2 n_ii
#   V_ij = -(n_ij + n_ji),  i != j
stuart_maxwell <- function(tab) {
  k <- nrow(tab)
  rs <- rowSums(tab); cs <- colSums(tab)
  d  <- (rs - cs)[1:(k - 1)]
  V  <- matrix(0, k - 1, k - 1)
  for (i in 1:(k - 1)) for (j in 1:(k - 1)) {
    V[i, j] <- if (i == j) rs[i] + cs[i] - 2 * tab[i, i] else -(tab[i, j] + tab[j, i])
  }
  stat <- tryCatch(as.numeric(t(d) %*% solve(V) %*% d), error = function(e) NA_real_)
  df   <- k - 1
  list(statistic = stat, df = df,
       p_value = if (is.na(stat)) NA_real_ else pchisq(stat, df, lower.tail = FALSE))
}

# --- run paired tests per PrgSrvc variable -----------------------------------

rows <- list()
for (v in prg_vars) {
  v5 <- panel[[paste0(v, "_y5")]]; v6 <- panel[[paste0(v, "_y6")]]

  # McNemar on each binary collapse (mirrors the template's "1 and 2 combined"
  # and "4 and 5 combined" rows). Universe = dictionary {1,2,3,4,5,97} in BOTH
  # years (97 kept, 98 / NA dropped).
  uni_bin <- in_universe(v5, v) & in_universe(v6, v)
  for (lab in c("decrease", "increase")) {
    codes <- if (lab == "decrease") c(1, 2) else c(4, 5)
    a5 <- as.integer(v5[uni_bin] %in% codes)
    a6 <- as.integer(v6[uni_bin] %in% codes)
    tb <- table(y5 = factor(a5, 0:1), y6 = factor(a6, 0:1))
    mc <- suppressWarnings(stats::mcnemar.test(tb, correct = TRUE))
    rows[[length(rows) + 1]] <- data.frame(
      variable = v, test = "McNemar", contrast = paste0(lab, " (vs not)"),
      n_pairs = sum(uni_bin), b_discordant = tb[1, 2], c_discordant = tb[2, 1],
      prop_y5 = mean(a5), prop_y6 = mean(a6),
      statistic = as.numeric(mc$statistic), df = as.numeric(mc$parameter),
      p_value = as.numeric(mc$p.value), stringsAsFactors = FALSE)
  }

  # Stuart-Maxwell on the 3-category collapse decrease/no change/increase.
  # 97 (N/A) is not a decrease/no-change/increase state, so the 3-category test
  # restricts to a 1-5 response in BOTH years.
  uni3 <- v5 %in% 1:5 & v6 %in% 1:5
  cat3 <- function(x) factor(ifelse(x %in% c(1, 2), "decrease",
                              ifelse(x == 3, "no change", "increase")),
                             levels = c("decrease", "no change", "increase"))
  tb3 <- table(cat3(v5[uni3]), cat3(v6[uni3]))
  sm  <- stuart_maxwell(tb3)
  rows[[length(rows) + 1]] <- data.frame(
    variable = v, test = "Stuart-Maxwell", contrast = "3-cat marginal homogeneity",
    n_pairs = sum(uni3), b_discordant = NA_integer_, c_discordant = NA_integer_,
    prop_y5 = NA_real_, prop_y6 = NA_real_,
    statistic = sm$statistic, df = sm$df, p_value = sm$p_value,
    stringsAsFactors = FALSE)
}

panel_results <- do.call(rbind, rows)

out_path <- file.path(here_root, "data/output/panel_tests.csv")
data.table::fwrite(panel_results, out_path)
message("Wrote: ", out_path)
