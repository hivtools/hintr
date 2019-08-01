context("endpoints")

test_that("endpoint_validate_baseline correctly validates data", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  response <- endpoint_validate_baseline("request", pjnz, NULL, NULL)
  expected_response <- '{"status":"success","errors":{},"data":"Botswana"}'
  expect_equal(as.vector(response), expected_response)
})

test_that("hintr_response correctly prepares response", {
  value <- list(
    success = TRUE,
    value = scalar("Passed")
  )
  expected_response <- '{"status":"success","errors":{},"data":"Passed"}'
  expect_equal(as.vector(hintr_response(value, "Test Schema")),
               expected_response)

  value <- list(
    success = FALSE,
    errors = list(error = "INVALID_BASELINE",
                  message = "Example error")
  )
  expected_response <-
    '{"status":"failure","errors":{"error":["INVALID_BASELINE"],"message":["Example error"]},"data":{}}'
  expect_equal(as.vector(hintr_response(value ,"Test Schema")),
               expected_response)
})

test_that("with_success correctly builds success message", {
  expr <- "Passed validation"
  expected_message <- list(
    success = TRUE,
    value = "Passed validation"
  )
  expect_equal(with_success(expr), expected_message)

  expr <- function() {
    stop("Failed validation")
  }
  expected_message <- list(
    success = FALSE,
    error = "Failed validation"
  )
  expect_equal(with_success(expr()), expected_message)
})
