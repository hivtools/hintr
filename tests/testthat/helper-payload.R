setup_payload_input_time_series <- function(data_root_dir, file_path, type) {
  file <- normalizePath(file.path(data_root_dir, file_path),
                        winslash = "/", mustWork = TRUE)
  shape <- normalizePath(file.path(data_root_dir, "malawi.geojson"),
                         winslash = "/", mustWork = TRUE)

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

setup_payload_review_inputs_metadata <- function(data_root_dir,
                                                 include_anc = TRUE,
                                                 include_programme = TRUE) {
  shape <- normalizePath(file.path(data_root_dir, "malawi.geojson"),
                         winslash = "/", mustWork = TRUE)
  survey <- normalizePath(file.path(data_root_dir, "survey.csv"),
                          winslash = "/", mustWork = TRUE)
  anc <- normalizePath(file.path(data_root_dir, "anc.csv"),
                       winslash = "/", mustWork = TRUE)
  programme <- normalizePath(file.path(data_root_dir, "programme.csv"),
                             winslash = "/", mustWork = TRUE)

  file_template <- '
    "%s": {
      "path": "%s",
      "hash": "12345",
      "filename": "original",
      "fromADR": false,
      "resource_url": "https://adr.unaids.org/file/123.csv"
    }'
  paste0(
    '{
      "iso3": "MWI",
      "data": {',
        paste(c(
          sprintf(file_template, "shape", shape),
          sprintf(file_template, "survey", survey),
          if (include_anc) sprintf(file_template, "anc", anc),
          if (include_programme) sprintf(file_template, "programme", programme)),
          collapse = ","
        ),
      '}
    }')
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
