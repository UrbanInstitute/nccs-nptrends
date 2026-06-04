# Iterate the template, run each row's test, and write two deliverables:
#   data/output/stats_tests_results.xlsx — the template with `result` filled
#   (accumulates the tidy per-estimate frame for 03_report.R via `tidy_all`)
#
# Reads: template (00_load.R), runners (01_prep.R).

results <- template
results$result <- NA_character_
tidy_all <- list()

for (i in seq_len(nrow(results))) {
  row <- results[i, ]
  out <- tryCatch(
    dispatch(
      variable   = row$variable,
      dataset    = row$dataset,
      response   = row$response,
      disagg     = row$disaggregation,
      stats_test = row$`stats.test`
    ),
    error = function(e) list(result = paste("ERROR:", conditionMessage(e)),
                             tidy = NULL)
  )
  results$result[i] <- out$result
  if (!is.null(out$tidy)) {
    out$tidy$template_row <- i
    out$tidy$variable     <- row$variable
    out$tidy$response     <- row$response
    out$tidy$disaggregation <- row$disaggregation
    tidy_all[[length(tidy_all) + 1]] <- out$tidy
  }
  if (i %% 10 == 0) message(sprintf("Completed %d / %d", i, nrow(results)))
}

tidy_all <- do.call(rbind, tidy_all)

out_path <- file.path(here_root, "data/output/stats_tests_results.xlsx")
openxlsx::write.xlsx(results, out_path, overwrite = TRUE)
message("Wrote: ", out_path)
