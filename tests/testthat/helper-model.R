## Use to return mock data from model run instead of always running the model
Sys.setenv("USE_MOCK_MODEL" = "true")

## Create mock model if files exist
mock_model <- list(
  output_path = system.file("output", "malawi_output.rds", package = "hintr"),
  spectrum_path =
    system.file("output", "malawi_spectrum_download.zip", package = "hintr"),
  coarse_output_path =
    system.file("output", "malawi_coarse_output_download.zip",
                package = "hintr"),
  summary_report_path =
    system.file("output", "malawi_summary_report.html", package = "hintr"),
  calibration_path =
    system.file("output", "malawi_calibration.rds", package = "hintr"))
class(mock_model) <- "hintr_output"

test_mock_model_available <- function() {
  invisible(lapply(mock_model, function(x) {
    if(!file.exists(x)) {
      testthat::skip(sprintf(
        "Test data %s is missing - run ./scripts/build_test_data to create test data.", x))
    }
  }))
}

## Model output as returned by
## hintr version 0.1.4 and naomi version 1.0.8
mock_model_v0.1.4 <- list(
  output_path = system.file("output", "malawi_output.rds", package = "hintr"),
  spectrum_path = system.file("output", "malawi_spectrum_download.zip",
                              package = "hintr"),
  coarse_output_path =
    system.file("output", "malawi_coarse_output_download.zip",
                package = "hintr"),
  calibration_path = system.file("output", "malawi_calibration.rds",
                                 package = "hintr"))
class(mock_model) <- "hintr_output"

## Model output as returned by
## hintr version 0.1.2 and naomi version 1.0.4
mock_model_v0.1.2 <- list(
  output_path = system.file("output", "malawi_output.rds", package = "hintr"),
  spectrum_path = system.file("output", "malawi_spectrum_download.zip",
                              package = "hintr"),
  coarse_output_path =
    system.file("output", "malawi_coarse_output_download.zip",
                package = "hintr"))
class(mock_model_v0.1.2) <- "hintr_output"

## Model output as returned by
## hintr version 0.1.1 and naomi version 1.0.3
mock_model_v0.1.1 <- list(
  output_path = system.file("output", "malawi_output.rds", package = "hintr"),
  spectrum_path = system.file("output", "malawi_spectrum_download.zip",
                              package = "hintr"),
  summary_path = system.file("output", "malawi_coarse_output_download.zip",
                             package = "hintr"))

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

setup_calibrate_payload <- function(version = NULL) {
  path <- tempfile()
  if (is.null(version)) {
    version <- to_json(cfg$version_info)
  }
  payload <- readLines("payload/model_calibrate_payload.json")
  payload <- gsub("<version_info>", version, payload, fixed = TRUE)
  writeLines(payload, path)
  path
}


clone_model_output <- function(output) {
  output_path <- tempfile()
  file.copy(output$output_path, output_path)
  spectrum_path <- tempfile(fileext = ".zip")
  file.copy(output$spectrum_path, spectrum_path)
  coarse_output_path <- tempfile(fileext = ".zip")
  file.copy(output$coarse_output_path, coarse_output_path)
  if (!is.null(output$calibration_path)) {
    calibration_path <- tempfile(fileext = ".rds")
    file.copy(output$calibration_path, calibration_path)
  } else {
    calibration_path <- NULL
  }
  if (!is.null(output$summary_report_path)) {
    summary_report_path <- tempfile(fileext = ".rds")
    file.copy(output$summary_report_path, summary_report_path)
  } else {
    summary_report_path <- NULL
  }
  out <- list(output_path = output_path,
              spectrum_path = spectrum_path,
              coarse_output_path = coarse_output_path,
              calibration_path = calibration_path,
              summary_report_path = summary_report_path,
              metadata = output$metadata)
  class(out) <- "hintr_output"
  out
}
