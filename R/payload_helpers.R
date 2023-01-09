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

setup_payload_validate_pjnz <- function(data_root_dir) {
  data_path <- normalizePath(file.path(data_root_dir, "Malawi2019.PJNZ"),
                             mustWork = TRUE)
  payload_path <- system_file("payload", "validate_pjnz_payload.json")
  payload <- readLines(payload_path)
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      pjnz = scalar(data_path)
    ))
}

setup_payload_validate_shape <- function(data_root_dir) {
  data_path <- normalizePath(file.path(data_root_dir, "malawi.geojson"),
                             mustWork = TRUE)
  payload_path <- system_file("payload", "validate_shape_payload.json")
  payload <- readLines(payload_path)
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      shape = scalar(data_path)
    ))
}

setup_payload_validate_population <- function(data_root_dir) {
  data_path <- normalizePath(file.path(data_root_dir, "population.csv"),
                             mustWork = TRUE)
  payload_path <- system_file("payload", "validate_population_payload.json")
  payload <- readLines(payload_path)
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      population = scalar(data_path)
    ))
}

setup_payload_validate_baseline <- function(data_root_dir) {
  pjnz <- normalizePath(file.path(data_root_dir, "Malawi2019.PJNZ"),
                        mustWork = TRUE)
  shape <- normalizePath(file.path(data_root_dir, "malawi.geojson"),
                         mustWork = TRUE)
  population <- normalizePath(file.path(data_root_dir, "population.csv"),
                              mustWork = TRUE)
  payload_path <- system_file("payload", "validate_baseline_payload.json")
  payload <- readLines(payload_path)
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      pjnz = scalar(pjnz),
      shape = scalar(shape),
      population = scalar(population)
    ))
}

setup_payload_validate_survey <- function(data_root_dir) {
  survey <- normalizePath(file.path(data_root_dir, "survey.csv"),
                        mustWork = TRUE)
  shape <- normalizePath(file.path(data_root_dir, "malawi.geojson"),
                         mustWork = TRUE)
  payload_path <- system_file("payload", "validate_survey_payload.json")
  payload <- readLines(payload_path)
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      shape = scalar(shape),
      survey = scalar(survey)
    ))
}

setup_payload_validate_programme <- function(data_root_dir) {
  programme <- normalizePath(file.path(data_root_dir, "programme.csv"),
                          mustWork = TRUE)
  shape <- normalizePath(file.path(data_root_dir, "malawi.geojson"),
                         mustWork = TRUE)
  payload_path <- system_file("payload", "validate_programme_payload.json")
  payload <- readLines(payload_path)
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      shape = scalar(shape),
      programme = scalar(programme)
    ))
}

setup_payload_validate_anc <- function(data_root_dir) {
  anc <- normalizePath(file.path(data_root_dir, "anc.csv"), mustWork = TRUE)
  shape <- normalizePath(file.path(data_root_dir, "malawi.geojson"),
                         mustWork = TRUE)
  payload_path <- system_file("payload", "validate_anc_payload.json")
  payload <- readLines(payload_path)
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      shape = scalar(shape),
      anc = scalar(anc)
    ))
}

setup_payload_model_options <- function(data_root_dir) {
  shape <- normalizePath(file.path(data_root_dir, "malawi.geojson"),
                         mustWork = TRUE)
  survey <- normalizePath(file.path(data_root_dir, "survey.csv"),
                          mustWork = TRUE)
  programme <- normalizePath(file.path(data_root_dir, "programme.csv"),
                             mustWork = TRUE)
  anc <- normalizePath(file.path(data_root_dir, "anc.csv"), mustWork = TRUE)
  payload_path <- system_file("payload", "model_run_options_payload.json")
  payload <- readLines(payload_path)
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      shape = scalar(shape),
      survey = scalar(survey),
      programme = scalar(programme),
      anc = scalar(anc)
    ))
}

setup_payload_model_options_validate <- function(data_root_dir) {
  pjnz <- normalizePath(file.path(data_root_dir, "Malawi2019.PJNZ"),
                         mustWork = TRUE)
  shape <- normalizePath(file.path(data_root_dir, "malawi.geojson"),
                         mustWork = TRUE)
  population <- normalizePath(file.path(data_root_dir, "population.csv"),
                         mustWork = TRUE)
  survey <- normalizePath(file.path(data_root_dir, "survey.csv"),
                          mustWork = TRUE)
  programme <- normalizePath(file.path(data_root_dir, "programme.csv"),
                             mustWork = TRUE)
  anc <- normalizePath(file.path(data_root_dir, "anc.csv"), mustWork = TRUE)
  payload_path <- system_file("payload", "validate_options_payload.json")
  payload <- readLines(payload_path)
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      pjnz = scalar(pjnz),
      shape = scalar(shape),
      population = scalar(population),
      survey = scalar(survey),
      programme = scalar(programme),
      anc = scalar(anc)
    ))
}

setup_payload_submit <- function(data_root_dir,
                                 version = NULL,
                                 include_anc_art = TRUE) {
  pjnz <- normalizePath(file.path(data_root_dir, "Malawi2019.PJNZ"),
                        mustWork = TRUE)
  shape <- normalizePath(file.path(data_root_dir, "malawi.geojson"),
                         mustWork = TRUE)
  population <- normalizePath(file.path(data_root_dir, "population.csv"),
                              mustWork = TRUE)
  survey <- normalizePath(file.path(data_root_dir, "survey.csv"),
                          mustWork = TRUE)
  programme <- normalizePath(file.path(data_root_dir, "programme.csv"),
                             mustWork = TRUE)
  anc <- normalizePath(file.path(data_root_dir, "anc.csv"), mustWork = TRUE)
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
      version_info = version,
      pjnz = scalar(pjnz),
      shape = scalar(shape),
      population = scalar(population),
      survey = scalar(survey),
      programme = scalar(programme),
      anc = scalar(anc)
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
  paste(c(payload, "}"), collapse = "\n")
}

setup_payload_rehydrate <- function(
    path = system_file("output", "malawi_spectrum_download.zip")) {
  c('{"file":{',
    paste0('"path": "', path, '",'),
    '"hash": "1234",',
    '"filename": "malawi_spectrum_download.zip"',
    '}',
    '}'
  )
}

setup_payload_input_time_series <- function(data_root_dir, file_path, type) {
  file <- normalizePath(file.path(data_root_dir, file_path),
                        mustWork = TRUE)
  shape <- normalizePath(file.path(data_root_dir, "malawi.geojson"),
                         mustWork = TRUE)

  sprintf(
    '{
      "data": {
        "%s": {
          "path": "%s",
          "hash": "12345",
          "filename": "original",
          "fromADR": false,
          "resource_url": "https://adr.unaids.org/file/123.csv"
        },
        "shape": {
          "path": "%s",
          "hash": "6789",
          "filename": "shape_file",
          "fromADR": false,
          "resource_url": "https://adr.unaids.org/file/123.csv"
        }
      }
    }', type, file, shape)
}

setup_payload_project_state <- function(version = NULL) {
  if (is.null(version)) {
    version <- to_json(cfg$version_info)
  }
  payload <- readLines(
    system_file("payload", "spectrum_download_state_payload.json"))
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      version_info = version
    ))
}
