# Build the long-format companion CSV from the tidy frame assembled in
# 02_tests.R, and add multiple-comparison adjustments to the chi-square family.
#
# Reads: tidy_all (02_tests.R). Writes: data/output/stats_tests_pvalues.csv
#
# In this request only the chi-square rows carry a p-value (the confidence-
# interval and year-comparison rows are estimation, not testing). The chi-square
# family here is small — one omnibus test per (variable x disaggregator) — so we
# report both Holm (family-wise error rate) and Benjamini-Hochberg (false
# discovery rate) adjusted values across that family. See README.md for how to
# read these columns.

long <- tidy_all
# `se` is the design-based SE of the proportion on the LINEAR scale. The CIs are
# logit-method (asymmetric), so est +/- 1.96*se will NOT reproduce ci_l/ci_u —
# the SE is provided for forming group-difference contrasts, not for rebuilding
# these intervals.
long <- long[, c("template_row", "variable", "response", "disaggregation",
                 "test_type", "group_var", "group_level",
                 "estimate", "se", "ci_l", "ci_u", "n",
                 "statistic", "df", "p_value")]

fam <- !is.na(long$p_value)
long$p_holm <- NA_real_
long$p_bh   <- NA_real_
long$p_holm[fam] <- p.adjust(long$p_value[fam], method = "holm")
long$p_bh[fam]   <- p.adjust(long$p_value[fam], method = "BH")

message(sprintf("Chi-square family size (p-values adjusted): %d", sum(fam)))

csv_path <- file.path(here_root, "data/output/stats_tests_pvalues.csv")
data.table::fwrite(long, csv_path)
message("Wrote: ", csv_path)

# Attach the raw + adjusted p-values back onto the results template by template
# row, and re-write the xlsx so the spreadsheet deliverable carries the multiple-
# comparison context itself (not just the raw p in the result string). Columns
# are populated for chi-square rows and left NA for CI / year-comparison rows.
chi <- long[long$test_type == "chi-square", ]
results$p_raw  <- NA_real_
results$p_holm <- NA_real_
results$p_bh   <- NA_real_
results$p_raw[chi$template_row]  <- chi$p_value
results$p_holm[chi$template_row] <- chi$p_holm
results$p_bh[chi$template_row]   <- chi$p_bh

xlsx_path <- file.path(here_root, "data/output/stats_tests_results.xlsx")
openxlsx::write.xlsx(results, xlsx_path, overwrite = TRUE)
message("Wrote (with p_raw/p_holm/p_bh columns): ", xlsx_path)
