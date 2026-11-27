library(mockery)

## Don't wait for the mock model run; tests which need a model run to be in
## progress set this themselves
Sys.setenv("MOCK_MODEL_DURATION" = "0")
## Don't generate real output files from naomi for downloads, these take up
## to 30s each. Tests which check the real files use skip_unless_slow()
Sys.setenv("USE_MOCK_DOWNLOAD" = "true")

## Tests validate responses against the JSON schemas, so building the API
## compiles a schema validator for every endpoint which takes ~3s per
## api_build(). Compile each schema once per test process instead.
if (testthat::is_testing()) local({
  cache <- new.env(parent = emptyenv())
  json_validator <- jsonvalidate::json_validator
  testthat::local_mocked_bindings(
    json_validator = function(schema, engine = "imjv", ...) {
      ## schema is a path or the schema text, possibly split over lines
      key <- paste(c(schema, engine), collapse = "\n")
      if (is.null(cache[[key]])) {
        cache[[key]] <- json_validator(schema, engine, ...)
      }
      cache[[key]]
    },
    .package = "jsonvalidate",
    .env = testthat::teardown_env())
})

## Slow tests run the real model or generate real outputs via naomi. They are
## skipped by default and run on CI for pushes to main and nightly.
skip_unless_slow <- function() {
  testthat::skip_if_not(Sys.getenv("HINTR_TEST_SLOW") == "true",
                        "slow test, set HINTR_TEST_SLOW=true to run")
}
