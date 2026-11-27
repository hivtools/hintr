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
                     datapack = ".xlsx",
                     hintr_error(t_("INVALID_DOWNLOAD_TYPE", list(type = type)),
                                 "INVALID_DOWNLOAD_TYPE"))
  path_results <- normalizePath(path_results, winslash = "/", mustWork = TRUE)
  download_path <- tempfile(type, tmpdir = path_results, fileext = file_ext)

  prepare_download <- download_function(type)
  if (type == "spectrum") {
    out <- prepare_download(model_output, download_path, input$notes)
    if (file_exists(out$path) && !is.null(input$state)) {
      add_state_json(out$path, input$state)
    }
  } else if (type == "datapack") {
    state_json <- jsonlite::fromJSON(input$state, simplifyVector = FALSE)
    model_fit_id <- state_json$model_fit$id
    calibrate_id <- state_json$calibrate$id
    ids <- data.frame(c("Model fit ID", "Calibrate ID"),
                      c(model_fit_id, calibrate_id))
    out <- prepare_download(model_output, download_path, ids = ids)
  } else {
    out <- prepare_download(model_output, download_path)
  }
  out
}

download_function <- function(type) {
  prepare_download <- switch(
    type,
    spectrum = naomi::hintr_prepare_spectrum_download,
    coarse_output = naomi::hintr_prepare_coarse_age_group_download,
    summary = naomi::hintr_prepare_summary_report_download,
    comparison = naomi::hintr_prepare_comparison_report_download,
    datapack = naomi::hintr_prepare_datapack_download,
    hintr_error(t_("INVALID_DOWNLOAD_TYPE", list(type = type)),
                "INVALID_DOWNLOAD_TYPE"))
  if (use_mock_download()) {
    prepare_download <- function(output, path, notes = NULL, ids = NULL) {
      mock_download(type, path, notes)
    }
  }
  prepare_download
}

use_mock_download <- function() {
  Sys.getenv("USE_MOCK_DOWNLOAD") == "true"
}

## Stand-in for naomi's download functions, for tests of hintr's handling of
## downloads (queueing, metadata, appending state). Generating the real files
## takes up to 30s each; naomi's own tests cover their content. Mirrors
## naomi's contract: signals one progress message, writes notes verbatim to
## notes.txt and returns the same metadata shape.
mock_download <- function(type, path, notes = NULL) {
  signalCondition(structure(list(message = "Generating mock download"),
                            class = c("progress", "condition")))
  if (type %in% c("spectrum", "coarse_output")) {
    t <- tempfile()
    dir.create(t)
    files <- "README.md"
    writeLines("Mock download", file.path(t, files))
    if (!is.null(notes)) {
      writeLines(notes, file.path(t, NOTES_PATH))
      files <- c(files, NOTES_PATH)
    }
    zip::zip(path, files, root = t)
  } else {
    writeLines("Mock download", path)
  }
  metadata <- list(areas = "MWI", type = type)
  if (type != "coarse_output") {
    metadata$description <- "Naomi output uploaded from Naomi web app (mock)"
  }
  list(path = path, metadata = metadata)
}

add_state_json <- function(path, state) {
  t <- tempfile()
  state_path <- file.path(t, PROJECT_STATE_PATH)
  dir.create(dirname(state_path), FALSE, TRUE)
  writeLines(jsonlite::prettify(state), state_path)
  zip::zip_append(path, dirname(state_path), mode = "cherry-pick")
  invisible(TRUE)
}
