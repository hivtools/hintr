test_that("input time series works with programme/art data", {
  input <- input_time_series_request(
    file.path("testdata", "programme.csv"),
    "programme",
    file.path("testdata", "malawi.geojson"))
  out <- input_time_series("programme", input)

  expect_equal(names(out), c("data", "filters", "defaults"))
  expect_true(nrow(out$data) > 100)
  expect_equal(names(out$defaults$selected_filter_options),
               c("plot_type", "area_level", "time_step"))
})

test_that("input time series works with anc data", {
  input <- input_time_series_request(
    file.path("testdata", "anc.csv"),
    "anc",
    file.path("testdata", "malawi.geojson"))
  out <- input_time_series("anc", input)

  expect_equal(names(out), c("data", "filters", "defaults"))
  expect_true(nrow(out$data) > 100)
  expect_equal(names(out$defaults$selected_filter_options),
               c("plot_type", "area_level", "age"))
})

test_that("input time series works if both anc and programme are provided", {
  input_json <- input_time_series_request(
    file.path("testdata", "anc.csv"),
    "anc",
    file.path("testdata", "malawi.geojson"))
  input <- jsonlite::fromJSON(input_json)
  input$data$programme <- list(
    path = file.path("testdata", "programme.csv"),
    hash = "12345",
    filename = "programme",
    fromADR = FALSE
  )
  input_json <- jsonlite::toJSON(input, auto_unbox = TRUE)
  out <- input_time_series("anc", input_json)

  expect_equal(names(out), c("data", "filters", "defaults"))
  expect_true(nrow(out$data) > 100)
  expect_equal(names(out$defaults$selected_filter_options),
               c("plot_type", "area_level", "age"))
})

test_that("input_time_series throws error if unknown file type", {
  input <- input_time_series_request(
    file.path("testdata", "survey.csv"),
    "survey",
    file.path("testdata", "malawi.geojson"))
  expect_error(input_time_series("survey", input), paste0(
    "Time series data can only be returned for programme or anc,",
    " received 'survey'."))
})

test_that("input_time_series catches unexpected errors", {
  input <- input_time_series_request(
    file.path("testdata", "unknown.csv"),
    "anc",
    file.path("testdata", "malawi.geojson"))
  error <- expect_error(input_time_series("anc", input))
  expect_equal(error$data[[1]]$error, "FAILED_TO_GENERATE_TIME_SERIES")
})

test_that("api can return input time series data for programme/art", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  input <- input_time_series_request(
    file.path("testdata", "programme.csv"),
    "programme",
    file.path("testdata", "malawi.geojson"))
  res <- api$request("POST", "/chart-data/input-time-series/programme",
                     body = input)
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(names(body$data), c("data", "filters", "defaults"))
  expect_true(nrow(body$data$data) > 100)
  expect_equal(names(body$data$defaults$selected_filter_options),
               c("plot_type", "area_level", "time_step"))
})

test_that("api can return input time series data for anc", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  input <- input_time_series_request(
    file.path("testdata", "anc.csv"),
    "anc",
    file.path("testdata", "malawi.geojson"))
  res <- api$request("POST", "/chart-data/input-time-series/anc",
                     body = input)
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(names(body$data), c("data", "filters", "defaults"))
  expect_true(nrow(body$data$data) > 100)
  expect_equal(names(body$data$defaults$selected_filter_options),
               c("plot_type", "area_level", "age"))
})
