download <- function(model_output, type, path_results, language = NULL) {
  if (!is.null(language)) {
    reset_hintr <- traduire::translator_set_language(language)
    reset_naomi <-
      traduire::translator_set_language(language, package = "naomi")
    on.exit({
      reset_hintr()
      reset_naomi()
    })
  }

  func <- switch(type,
                 spectrum = naomi::hintr_prepare_spectrum_download,
                 coarse_output = naomi::hintr_prepare_coarse_age_group_download,
                 summary = naomi::hintr_prepare_summary_report_download,
                 comparison = naomi::hintr_prepare_comparison_report_download,
                 hintr_error(t_("INVALID_DOWNLOAD_TYPE", list(type = type)),
                             "INVALID_DOWNLOAD_TYPE"))
  file_ext <- switch(type,
                     spectrum = ".zip",
                     coarse_output = ".zip",
                     summary = ".html",
                     comparison = ".html",
                     hintr_error(t_("INVALID_DOWNLOAD_TYPE", list(type = type)),
                                 "INVALID_DOWNLOAD_TYPE"))
  path_results <- normalizePath(path_results, mustWork = TRUE)
  download_path <- tempfile(type, tmpdir = path_results, fileext = file_ext)
  func(model_output, download_path)
}
