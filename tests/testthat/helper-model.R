## Use to return mock data from model run instead of always running the model
Sys.setenv("USE_MOCK_MODEL" = "true")

## Don't use dynamic libaries as these don't currently work on travis with
## up to date INLA build
##INLA:::inla.dynload.workaround()

mock_model <- list(output_path = file.path("testdata", "malawi_output.RDS"),
                   spectrum_path = file.path("testdata", "malawi_spectrum_download.zip"),
                   summary_path = file.path("testdata", "malawi_summary_download.zip"))

test_mock_model_available <- function() {
  lapply(mock_model, function(x) {
    if(!file.exists(x)) {
      testthat::skip(sprintf(
        "Test data %s is missing - run ./scripts/build_test_data to create test data.", x))
    }
  })
}
