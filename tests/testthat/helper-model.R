## Use to return mock data from model run instead of always running the model
Sys.setenv("USE_MOCK_MODEL" = "true")

## Create mock model if files exist
mock_model <- list(
  output_path = system.file("output", "malawi_output.rds", package = "hintr"),
  spectrum_path = system.file("output", "malawi_spectrum_download.zip",
                              package = "hintr"),
  summary_path = system.file("output", "malawi_summary_download.zip",
                             package = "hintr"))

test_mock_model_available <- function() {
  invisible(lapply(mock_model, function(x) {
    if(!file.exists(x)) {
      testthat::skip(sprintf(
        "Test data %s is missing - run ./scripts/build_test_data to create test data.", x))
    }
  }))
}

setup_submit_payload <- function(version = NULL, include_anc_art = TRUE) {
  path <- tempfile()
  if (is.null(version)) {
    version <- to_json(cfg$version_info)
  }
  if (include_anc_art) {
    payload <- readLines("payload/model_submit_payload.json")
  } else {
    payload <- readLines("payload/model_submit_payload_minimal.json")
  }
  payload <- gsub("<version_info>", version, payload, fixed = TRUE)
  writeLines(payload, path)
  path
}
