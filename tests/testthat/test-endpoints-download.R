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
  summary <- download_summary(queue)
  download <- summary(response$id)
  expect_type(download, "raw")
  expect_length(download, file.size(
    system.file("output", "malawi_summary_download.zip", package = "hintr")))
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
    system.file("output", "malawi_spectrum_download.zip", package = "hintr")))
})

test_that("download returns useful error if model run fails", {
  test_redis_available()
  test_mock_model_available()

  ## Setup payload with options which will throw an error
  path <- setup_submit_payload()
  input <- jsonlite::fromJSON(path)
  input$options <- NULL
  writeLines(jsonlite::toJSON(input), path)

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
               scalar("Required model options not supplied:.+"))
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

