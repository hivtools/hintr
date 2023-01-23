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
