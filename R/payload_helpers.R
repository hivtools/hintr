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
                                           include_state = TRUE) {
  if (!any(include_notes, include_state)) {
    stop("Must include notes and/or state info in payload")
  }
  payload <- "{"
  path <- tempfile()
  if (include_notes) {
    payload <- c(
      payload,
      '"notes":',
      readLines(system_file("payload", "spectrum_download_notes_payload.json")))
  }
  if (include_state) {
    if (include_notes) {
      payload <- c(payload, ",")
    }
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
                 '"state":',
                 state_payload)
  }
  to_json(json_verbatim(c(payload, "}")))
}
