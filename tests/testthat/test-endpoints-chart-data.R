test_that("input time series works with programme/art data", {
  input <- setup_payload_input_time_series(
    test_path("testdata"),
    "programme.csv",
    "programme")
  out <- input_time_series("programme", input)

  expect_equal(names(out), c("data", "metadata", "warnings"))
  expect_true(nrow(out$data) > 100)
  expect_equal(names(out$metadata$defaults$selected_filter_options),
               c("plot_type", "area_level", "quarter"))
})

test_that("input time series works with anc data", {
  input <- setup_payload_input_time_series(
    test_path("testdata"),
    "anc.csv",
    "anc")
  out <- input_time_series("anc", input)

  expect_equal(names(out), c("data", "metadata", "warnings"))
  expect_true(nrow(out$data) > 100)
  expect_equal(names(out$metadata$defaults$selected_filter_options),
               c("plot_type", "area_level", "age", "quarter"))
})

test_that("input time series works if both anc and programme are provided", {
  input_json <- setup_payload_input_time_series(
    test_path("testdata"),
    "anc.csv",
    "anc")
  input <- jsonlite::fromJSON(input_json)
  input$data$programme <- list(
    path = file.path("testdata", "programme.csv"),
    hash = "12345",
    filename = "programme",
    fromADR = FALSE
  )
  input_json <- jsonlite::toJSON(input, auto_unbox = TRUE)
  out <- input_time_series("anc", input_json)

  expect_equal(names(out), c("data", "metadata", "warnings"))
  expect_true(nrow(out$data) > 100)
  expect_equal(names(out$metadata$defaults$selected_filter_options),
               c("plot_type", "area_level", "age", "quarter"))
})

test_that("input_time_series throws error if unknown file type", {
  input <- setup_payload_input_time_series(
    test_path("testdata"),
    "survey.csv",
    "survey")
  expect_error(input_time_series("survey", input), paste0(
    "Time series data can only be returned for programme or anc,",
    " received 'survey'."))
})

test_that("input_time_series catches unexpected errors", {
  input <- setup_payload_input_time_series(
    test_path("testdata"),
    "malawi.geojson",
    "anc")
  error <- expect_error(input_time_series("anc", input))
  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_GENERATE_TIME_SERIES"))
})

test_that("endpoint_input_time_series_plot works with programme data", {
  endpoint <- endpoint_input_time_series_plot()
  input <- setup_payload_input_time_series(
    test_path("testdata"),
    "programme.csv",
    "programme")
  res <- endpoint$run("programme", input)
  expect_equal(res$status_code, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(names(body$data), c("data", "metadata", "warnings"))
  expect_true(nrow(body$data$data) > 100)
  expect_equal(names(body$data$metadata$defaults$selected_filter_options),
               c("plot_type", "area_level", "quarter"))
})

test_that("endpoint_input_time_series_plot works without vls indicators", {
  endpoint <- endpoint_input_time_series_plot()
  input <- setup_payload_input_time_series(
    test_path("testdata"),
    "programme_no_vls.csv",
    "programme")
  res <- endpoint$run("programme", input)
  expect_equal(res$status_code, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(names(body$data), c("data", "metadata", "warnings"))
  expect_true(nrow(body$data$data) > 100)
  expect_equal(names(body$data$metadata$defaults$selected_filter_options),
               c("plot_type", "area_level", "quarter"))
})

test_that("endpoint_input_time_series_plot works with anc data", {
  endpoint <- endpoint_input_time_series_plot()
  input <- setup_payload_input_time_series(
    test_path("testdata"),
    "anc.csv",
    "anc")
  res <- endpoint$run("anc", input)
  expect_equal(res$status_code, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(names(body$data), c("data", "metadata", "warnings"))
  expect_true(nrow(body$data$data) > 100)
  expect_equal(names(body$data$metadata$defaults$selected_filter_options),
               c("plot_type", "area_level", "age", "quarter"))
})

test_that("api can return input time series data for programme/art", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue, validate = TRUE)
  input <- setup_payload_input_time_series(
    test_path("testdata"),
    "programme.csv",
    "programme")
  res <- api$request("POST", "/chart-data/input-time-series/programme",
                     body = input)
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(names(body$data), c("data", "metadata", "warnings"))
  expect_true(nrow(body$data$data) > 100)
  expect_equal(names(body$data$metadata$defaults$selected_filter_options),
               c("plot_type", "area_level", "quarter"))
})

test_that("api can return input time series data for anc", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  input <- setup_payload_input_time_series(
    test_path("testdata"),
    "anc.csv",
    "anc")
  res <- api$request("POST", "/chart-data/input-time-series/anc",
                     body = input)
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(names(body$data), c("data", "metadata", "warnings"))
  expect_true(nrow(body$data$data) > 100)
  expect_equal(names(body$data$metadata$defaults$selected_filter_options),
               c("plot_type", "area_level", "age", "quarter"))
})

test_that("input time series returns warnings programme data", {
  input <- setup_payload_input_time_series(
    test_path("testdata"),
    "programme.csv",
    "programme")
  out <- input_time_series("programme", input)

  expect_equal(names(out), c("data", "metadata", "warnings"))
  expect_length(out$warnings, 0)
})

test_that("input time series returns warnings anc data", {
  input <- setup_payload_input_time_series(
    test_path("testdata"),
    "anc.csv",
    "anc")
  out <- input_time_series("anc", input)

  expect_equal(names(out), c("data", "metadata", "warnings"))
  expect_length(out$warnings, 0)
})
