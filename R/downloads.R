download <- function(model_output, type, path_results, input, language = NULL) {
  if (!is.null(language)) {
    reset_hintr <- traduire::translator_set_language(language)
    reset_naomi <-
      traduire::translator_set_language(language, package = "naomi")
    on.exit({
      reset_hintr()
      reset_naomi()
    })
  }

  file_ext <- switch(type,
                     spectrum = ".zip",
                     coarse_output = ".zip",
                     summary = ".html",
                     comparison = ".html",
                     agyw = ".xlsx",
                     hintr_error(t_("INVALID_DOWNLOAD_TYPE", list(type = type)),
                                 "INVALID_DOWNLOAD_TYPE"))
  path_results <- normalizePath(path_results, winslash = "/", mustWork = TRUE)
  download_path <- tempfile(type, tmpdir = path_results, fileext = file_ext)

  if (type == "spectrum") {
    out <- naomi::hintr_prepare_spectrum_download(model_output,
                                                  download_path,
                                                  input$notes,
                                                  input$vmmc)
    if (file_exists(out$path) && !is.null(input$state)) {
      add_state_json(out$path, input$state)
    }
  } else if (type == "agyw") {
    out <- naomi::hintr_prepare_agyw_download(model_output, input$pjnz,
                                              download_path)
  } else {
    func <- switch(type,
                   coarse_output = naomi::hintr_prepare_coarse_age_group_download,
                   summary = naomi::hintr_prepare_summary_report_download,
                   comparison = naomi::hintr_prepare_comparison_report_download,
                   hintr_error(t_("INVALID_DOWNLOAD_TYPE", list(type = type)),
                               "INVALID_DOWNLOAD_TYPE"))
    out <- func(model_output, download_path)
  }
  out
}

add_state_json <- function(path, state) {
  t <- tempfile()
  state_path <- file.path(t, PROJECT_STATE_PATH)
  dir.create(dirname(state_path), FALSE, TRUE)
  writeLines(jsonlite::prettify(state), state_path)
  zip::zip_append(path, dirname(state_path), mode = "cherry-pick")
  invisible(TRUE)
}
