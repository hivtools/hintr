run_calibrate <- function(model_output, calibration_options, path_results,
                          language = NULL) {
  if (!is.null(language)) {
    reset_hintr <- traduire::translator_set_language(language)
    reset_naomi <-
      traduire::translator_set_language(language, package = "naomi")
    on.exit({
      reset_hintr()
      reset_naomi()
    })
  }

  path_results <- normalizePath(path_results, mustWork = TRUE)
  output_path <- tempfile(tmpdir = path_results, fileext = ".rds")
  spectrum_path <- tempfile(tmpdir = path_results, fileext = ".zip")
  coarse_output_path <- tempfile(tmpdir = path_results, fileext = ".zip")
  summary_report_path <- tempfile(tmpdir = path_results, fileext = ".html")
  calibration_path <- tempfile(tmpdir = path_results, fileext = ".rds")

  naomi::hintr_calibrate(model_output, calibration_options, output_path,
                         spectrum_path, coarse_output_path,
                         summary_report_path, calibration_path)
}
