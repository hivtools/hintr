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
  plot_data_path <- tempfile("plot_data", tmpdir = path_results,
                             fileext = ".qs")
  model_output_path <- tempfile("model_output", tmpdir = path_results,
                                fileext = ".qs")

  naomi::hintr_calibrate(model_output, calibration_options, plot_data_path,
                         model_output_path)
}
