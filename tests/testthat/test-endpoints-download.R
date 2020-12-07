context("endpoints-download")

test_that("indicator download returns bytes", {
  test_redis_available()
  test_mock_model_available()

  ## Setup payload
  path <- setup_submit_payload()

  ## Run the model
  queue <- test_queue(workers = 1)
  model_submit <- submit_model(queue)
  response <- model_submit(readLines(path))
  expect_true("id" %in% names(response))

  out <- queue$queue$task_wait(response$id)
  coarse_output <- download_coarse_output(queue)
  download <- coarse_output(response$id)
  expect_type(download, "raw")
  expect_length(download, file.size(
    system_file("output", "malawi_coarse_output_download.zip")))
})

test_that("spectrum download returns bytes", {
  test_redis_available()
  test_mock_model_available()

  ## Setup payload
  path <- setup_submit_payload()

  ## Run the model
  queue <- test_queue(workers = 1)
  model_submit <- submit_model(queue)
  response <- model_submit(readLines(path))
  expect_true("id" %in% names(response))

  out <- queue$queue$task_wait(response$id)
  spectrum <- download_spectrum(queue)
  download <- spectrum(response$id)
  expect_type(download, "raw")
  expect_length(download, file.size(
    system_file("output", "malawi_spectrum_download.zip")))
})

test_that("summary download returns bytes", {
  test_redis_available()
  test_mock_model_available()

  ## Setup payload
  path <- setup_submit_payload()

  ## Run the model
  queue <- test_queue(workers = 1)
  model_submit <- submit_model(queue)
  response <- model_submit(readLines(path))
  expect_true("id" %in% names(response))

  out <- queue$queue$task_wait(response$id)
  summary <- download_summary(queue)
  download <- summary(response$id)
  expect_type(download, "raw")
  expect_length(download, file.size(
    system_file("output", "malawi_summary_report.html")))
})

test_that("download returns useful error if model run fails", {
  test_redis_available()
  test_mock_model_available()

  ## Create a population file which deliberately will cause an error
  path <- setup_submit_payload()
  payload <- readLines(path)
  payload <- jsonlite::read_json(path)
  pop <- read.csv(payload$data$population$path)
  pop$sex <- NULL
  t <- tempfile()
  write.csv(pop, t)
  payload$data$population$path <- t
  writeLines(jsonlite::toJSON(payload), path)

  ## Run the model
  withr::with_envvar(c("USE_MOCK_MODEL" = "false"), {
    queue <- test_queue(workers = 1)
    model_submit <- submit_model(queue)
    response <- model_submit(readLines(path))
    expect_true("id" %in% names(response))
  })

  out <- queue$queue$task_wait(response$id)
  spectrum <- download_spectrum(queue)
  error <- expect_error(spectrum(response$id))
  expect_equal(error$data[[1]]$error, scalar("MODEL_RUN_FAILED"))
  expect_match(error$data[[1]]$detail,
               scalar("Required columns not found: sex"))
  expect_equal(error$status_code, 400)
})

test_that("download returns useful error if model result can't be retrieved", {
  test_redis_available()
  test_mock_model_available()

  ## Try to download with task ID doesn't exist
  queue <- test_queue()
  spectrum <- download_spectrum(queue)
  error <- expect_error(spectrum("id1"))
  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_RETRIEVE_RESULT"))
  expect_equal(error$data[[1]]$detail, scalar("Missing some results"))
  expect_equal(error$status_code, 400)
})

test_that("download works with v0.1.1 model run result", {
  test_redis_available()
  test_mock_model_available()

  ## Setup payload
  path <- setup_submit_payload()

  ## Return v0.1.1 model results
  queue <- MockQueue$new()
  unlockBinding("result", queue)
  queue$result <- mockery::mock(mock_model_v0.1.1)
  coarse_output <- download_coarse_output(queue)
  download <- coarse_output("id")
  expect_type(download, "raw")
  expect_length(download, file.size(
    system_file("output", "malawi_coarse_output_download.zip")))
})

test_that("trying to download report for old model run returns error", {
  test_mock_model_available()

  ## Mock model run
  queue <- test_queue(workers = 0)
  unlockBinding("result", queue)
  ## Clone model output as it modifies in place
  out <- clone_model_output(mock_model_v0.1.4)
  queue$result <- mockery::mock(out)
  unlockBinding("queue", queue)
  unlockBinding("task_status", queue$queue)
  queue$queue$task_status <- mockery::mock("COMPLETE")

  ## Try to download missing summary report
  path <- setup_calibrate_payload()
  download <- download_summary(queue)
  error <- expect_error(download("id"))

  expect_equal(error$data[[1]]$error, scalar("MODEL_RESULT_OUT_OF_DATE"))
  expect_match(error$data[[1]]$detail, scalar(paste0(
    "Can't download summary report, please re-run model",
    " and try download again.")))
  expect_equal(error$status_code, 400)
})
