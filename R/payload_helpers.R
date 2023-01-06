setup_payload_submit <- function(version = NULL, include_anc_art = TRUE) {
  path <- tempfile()
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
  payload <- gsub("<version_info>", version, payload, fixed = TRUE)
  writeLines(payload, path)
  path
}

setup_payload_calibrate <- function(version = NULL) {
  path <- tempfile()
  if (is.null(version)) {
    version <- to_json(cfg$version_info)
  }
  payload <- readLines(system_file("payload", "model_calibrate_payload.json"))
  payload <- gsub("<version_info>", version, payload, fixed = TRUE)
  writeLines(payload, path)
  path
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
    state_payload <- gsub("<version_info>", version, state_payload,
                          fixed = TRUE)
    payload <- c(payload,
                 '"state":',
                 state_payload)
  }
  payload <- c(payload, "}")
  writeLines(payload, path)
  path
}

setup_payload_rehydrate <- function(data_root,
    path = system_file("output", "malawi_spectrum_download.zip")) {
  c('{"file":{',
    paste0('"path": "', path, '",'),
    '"hash": "1234",',
    '"filename": "malawi_spectrum_download.zip"',
    '}',
    '}'
  )
}
