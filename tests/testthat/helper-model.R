## Use to return mock data from model run instead of always running the model
Sys.setenv("USE_MOCK_MODEL" = "true")

## Don't use dynamic libaries as these don't currently work on travis with
## up to date INLA build
INLA:::inla.dynload.workaround()

## Create mock model if files exist
mock_model <- list(
  output_path = system.file("output", "malawi_output.RDS", package = "hintr"),
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
