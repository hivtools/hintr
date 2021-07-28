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
