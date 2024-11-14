test_that("can return input comparison metadata", {
  input <- setup_payload_input_comparison(test_path("testdata"))
  out <- input_comparison(input)

  expect_setequal(names(out), c("data", "metadata", "warnings"))
  expect_input_comparison_metadata(out$metadata)
  expect_true(nrow(out$data) > 50)
  expect_equal(out$warnings, list())
})

test_that("error returned if neither anc nor programme data", {
  input <- setup_payload_input_comparison(test_path("testdata"),
                                          FALSE, FALSE)
  error <- expect_error(input_comparison(input))
  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_GENERATE_INPUT_COMPARISON"))
  expect_match(
    error$data[[1]]$detail,
    scalar("Cannot build input comparison plot without either programme or anc data"))
  expect_equal(error$status_code, 400)
})

test_that("input comparison metadata endpoint", {
  test_redis_available()
  input <- setup_payload_input_comparison(test_path("testdata"))

  endpoint <- endpoint_input_comparison_plot()
  res <- endpoint$run(input)
  expect_equal(res$status_code, 200)
  body <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(names(body$data), c("data", "metadata", "warnings"))

  expect_input_comparison_metadata(body$data$metadata)
  expect_true(length(body$data$data) > 50)
  expect_equal(body$data$warnings, list())
})

test_that("api can return input comparison data", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue, validate = TRUE)
  input <- setup_payload_input_comparison(test_path("testdata"))
  res <- api$request("POST", "/chart-data/input-comparison",
                     body = input)
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(names(body$data), c("data", "metadata", "warnings"))
  expect_input_comparison_metadata(body$data$metadata)
  expect_true(length(body$data$data) > 50)
  expect_equal(body$data$warnings, list())
})
