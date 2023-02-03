test_that("download_debug prevents overwriting", {
  tmp <- tempfile()
  id <- "abc"
  dir.create(file.path(tmp, id), FALSE, TRUE)
  expect_error(
    download_debug(id, dest = tmp),
    "Path 'abc' already exists at destination")
})

test_that("Debug endpoint returns debug information", {
  test_redis_available()
  test_mock_model_available()

  ## Start the model running
  payload <- setup_payload_submit()
  queue <- test_queue(workers = 1)
  model_submit <- submit_model(queue)
  response <- model_submit(payload)
  expect_true("id" %in% names(response))
  id <- response$id

  model_debug <- download_model_debug(queue)
  bin <- model_debug(id)
  tmp <- tempfile()
  dest <- tempfile()
  writeBin(as.vector(bin), tmp)
  zip::unzip(tmp, exdir = dest)
  expect_equal(scalar(dir(dest)), id)
  expect_setequal(
    dir(file.path(dest, id)),
    c("data.rds", "files"))
  info <- readRDS(file.path(dest, id, "data.rds"))
  ## Smoke test options are passed through
  expect_true(length(info$objects$options) > 25)
  expect_true(list(area_scope = "MWI") %in% info$objects$options)
  expect_s3_class(info$sessionInfo, "sessionInfo")
  expect_equal(names(info$objects$data),
               c("pjnz", "shape", "population", "survey", "programme", "anc"))
  expect_equal(names(info$objects$data$pjnz), c("path", "hash", "filename"))
  expect_setequal(
    dir(file.path(dest, id, "files")),
    c("anc.csv", "malawi.geojson", "Malawi2019.PJNZ", "population.csv",
      "programme.csv", "survey.csv"))
})

test_that("Debug endpoint returns debug information for calibrate", {
  test_mock_model_available()
  q <- test_queue_result()

  submit <- endpoint_model_calibrate_submit(q$queue)
  payload <- setup_payload_calibrate()
  submit_response <- submit$run(q$model_run_id, payload)
  id <- submit_response$data$id

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(id))

  ## Wait for complete status
  out <- q$queue$queue$task_wait(id)
  status <- endpoint_model_calibrate_status(q$queue)
  status_response <- status$run(id)
  expect_equal(status_response$data$status, scalar("COMPLETE"))

  model_debug <- download_model_debug(q$queue)
  bin <- model_debug(id)
  tmp <- tempfile()
  dest <- tempfile()
  writeBin(as.vector(bin), tmp)
  zip::unzip(tmp, exdir = dest)
  expect_equal(scalar(dir(dest)), id)
  expect_setequal(
    dir(file.path(dest, id)),
    c("data.rds", "files"))
  info <- readRDS(file.path(dest, id, "data.rds"))

  expect_s3_class(info$objects$model_output, "hintr_output")
  expect_true(length(info$objects$calibration_options) > 5)
  expect_s3_class(info$sessionInfo, "sessionInfo")
  expect_equal(info$objects$model_output$model_output_path,
               dir(file.path(dest, id, "files")))
  expect_null(info$objects$model_output$plot_data_path)
})

test_that("Debug endpoint returns debug information for download", {
  test_mock_model_available()
  q <- test_queue_result()

  submit <- endpoint_download_submit(q$queue)
  submit_response <- submit$run(q$calibrate_id, "spectrum")
  id <- submit_response$data$id

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(id))

  ## Wait for complete status
  out <- q$queue$queue$task_wait(id)
  status <- endpoint_model_calibrate_status(q$queue)
  status_response <- status$run(id)
  expect_equal(status_response$data$status, scalar("COMPLETE"))

  model_debug <- download_model_debug(q$queue)
  bin <- model_debug(id)
  tmp <- tempfile()
  dest <- tempfile()
  writeBin(as.vector(bin), tmp)
  zip::unzip(tmp, exdir = dest)
  expect_equal(scalar(dir(dest)), id)
  expect_setequal(
    dir(file.path(dest, id)),
    c("data.rds", "files"))
  info <- readRDS(file.path(dest, id, "data.rds"))

  expect_s3_class(info$objects$model_output, "hintr_output")
  expect_equal(info$objects$type, "spectrum")
  expect_s3_class(info$sessionInfo, "sessionInfo")
  files <- c(info$objects$model_output$model_output_path,
             info$objects$model_output$plot_data_path)
  expect_setequal(files, dir(file.path(dest, id, "files")))
})

test_that("Debug endpoint errors on nonexistant id", {
  test_redis_available()
  queue <- test_queue()
  model_debug <- download_model_debug(queue)

  error <- expect_error(model_debug("1234"))
  expect_equal(error$data[[1]]$error, scalar("INVALID_TASK"))
  expect_equal(error$data[[1]]$detail,
               scalar("Task '1234' not found"))
  expect_equal(error$status_code, 400)
})
