context("endpoints")

test_that("prepare_success_response correctly prepares response", {
  pjnz <- system.file("testdata", "Botswana2018.PJNZ", package = "hintr")
  expected_response <- list(
    "status" = "success",
    "errors" = list(),
    "data" = list(
      "pjnz" = list(
        "country" = "Botswana"
      ),
      "shape" = list(),
      "population" = list(),
      "survey" = list(),
      "programme" = list(),
      "anc" = list()
    )
  )
  expect_equal(prepare_success_response(pjnz), expected_response)
  ## TODO: Test that response matches schema
})

test_that("prepare_fail_response correctly prepares response", {
  expected_response <- list(
    "status" = "failure",
    "errors" = list(
      "pjnz" = c("EXAMPLE_FAILURE")
    )
  )
  expect_equal(prepare_fail_response(), expected_response)
  ## TODO: Test that response matches schema
})

test_that("handle_validate correctly handles response", {
  pjnz <- list(
    path = system.file("testdata", "Botswana2018.PJNZ", package = "hintr"))
  req <- "request body"
  handle_validate(req, pjnz, NULL, NULL, NULL, NULL, NULL)
})
