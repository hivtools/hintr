# nocov start
# These are test helpers so skipping coverage checks, they should not
# be used by anything in the package
#' Build JSON from template and a set of params
#'
#' @param template Template JSON of payload
#' @param params List of named key value pairs for substituting from template.
#'
#' @return JSON built from template and params.
#' @keywords internal
#'
build_json <- function(template, params) {
  param_env <- list2env(params, parent = .GlobalEnv)
  glue::glue(template, .envir = param_env, .open = '"<+',
             .close = '+>"', .transformer = json_transformer)
}

json_transformer <- function(text, envir) {
  res <- get(text, envir = envir, inherits = FALSE)
  to_json(res)
}
# nocov end

setup_payload_submit <- function(version = NULL,
                                 include_anc_art = TRUE) {
  if (is.null(version)) {
    version <- to_json(cfg$version_info)
  }
  if (include_anc_art) {
    payload <- readLines(
      system_file("payload", "model_submit_payload.json"))
  } else {
    payload <- readLines(
      system_file("payload", "model_submit_payload_minimal.json"))
  }
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      version_info = version
    ))
}

setup_payload_calibrate <- function(version = NULL) {
  if (is.null(version)) {
    version <- to_json(cfg$version_info)
  }
  payload <- readLines(system_file("payload", "model_calibrate_payload.json"))
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      version_info = version
    ))
}

setup_payload_download_request <- function(version = NULL,
                                           include_notes = TRUE,
                                           include_state = TRUE,
                                           include_pjnz = FALSE,
                                           include_vmmc = FALSE) {
  if (!any(include_notes, include_state, include_pjnz)) {
    stop("Must include one or more of notes, state or pjnz in payload")
  }
  payload <- list()
  path <- tempfile()
  if (include_notes) {
    notes <- paste0(readLines(
      system_file("payload", "spectrum_download_notes_payload.json")),
      collapse = "\n")
    payload <- c(
      payload,
      paste('"notes":', notes))
  }
  if (include_state) {
    if (is.null(version)) {
      version <- to_json(cfg$version_info)
    }
    state_payload <- readLines(
      system_file("payload", "spectrum_download_state_payload.json"))
    state_payload <- build_json(
      paste0(state_payload, collapse = "\n"),
      list(
        version_info = version
      )
    )
    payload <- c(payload,
                 paste('"state":', state_payload))
  }
  if (include_pjnz) {
    pjnz <- jsonlite::read_json(
      system_file("payload", "model_submit_payload.json"))$data$pjnz
    payload <- c(payload, paste(
      '"pjnz": ', jsonlite::toJSON(pjnz, auto_unbox = TRUE, null = "null")))
  }
  if (include_vmmc) {
    path <- testthat::test_path("testdata", "vmmc.csv")
    payload <- c(payload, paste(
      '"vmmc":',
      jsonlite::toJSON(file_object(path), auto_unbox = TRUE, null = "null")))
  }
  payload <- paste(payload, collapse = ",\n")
  payload <- paste("{\n", payload, "\n}")
  to_json(json_verbatim(payload))
}
