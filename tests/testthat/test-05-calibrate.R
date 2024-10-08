test_that("calibration can be run", {
  test_mock_model_available()

  model_output <- clone_model_output(mock_model)

  ## Calibrate the result
  payload <- setup_payload_calibrate()
  calibration_options <- jsonlite::fromJSON(payload)$options

  calibrated_result <- run_calibrate(model_output, calibration_options,
                                     tempdir())

  ## Expected files are returned
  expect_equal(names(calibrated_result),
               c("plot_data_path", "model_output_path", "version", "warnings"))
})

test_that("calibrate can set language", {
  model_output <- list(calibration_path = "test")
  expect_error(run_calibrate(model_output, list(opt = "options"), tempdir()),
               "Model output out of date please re-run model and try again.")

  ## With french translation
  expect_error(run_calibrate(model_output, list(opt = "options"), tempdir(),
                             language = "fr"),
               paste0("La sortie du modèle est obsolète, ",
                      "veuillez relancer le modèle et réessayer."))
})

test_that("can calibrate a model result", {
  test_mock_model_available()
  q <- test_queue_result()

  ## Calibrate the result
  payload <- setup_payload_calibrate()
  calibrate <- submit_calibrate(q$queue)
  res <- calibrate(q$model_run_id, payload)
  expect_equal(names(res), "id")
  expect_true(!is.null(res$id))

  ## Get status
  out <- q$queue$task_wait(res$id)
  status <- queue_status(q$queue)
  res_status <- status(res$id)
  expect_equal(res_status$id, res$id)
  expect_true(res_status$done)
  expect_equal(res_status$status, scalar("COMPLETE"))
  expect_true(res_status$success)
  expect_equal(res_status$queue, scalar(0))
  expect_match(res_status$progress[[1]],
               "Saving outputs - [\\d.m\\s]+s elapsed", perl = TRUE)

  get_result <- calibrate_result(q$queue)
  result <- get_result(res$id)
  expect_equal(names(result),
               c("data", "warnings"))
  expect_equal(colnames(result$data),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator", "mode", "mean", "lower", "upper"))
  expect_true(nrow(result$data) > 84042)
})

test_that("model calibration fails is version out of date", {
  test_mock_model_available()

  ## Mock model run
  queue <- test_queue(workers = 0)
  unlockBinding("result", queue)
  ## Clone model output as it modifies in place
  out <- clone_model_output(mock_model)
  queue$result <- mockery::mock(out)
  mock_verify_result_available <- mockery::mock(TRUE)

  ## Calibrate the result
  payload <- setup_payload_calibrate('{
                                       "hintr": "0.0.12",
                                       "naomi": "0.0.15",
                                       "rrq": "0.2.1"
                                     }')
  calibrate <- submit_calibrate(queue)
  mockery::stub(calibrate, "verify_result_available",
                mock_verify_result_available)
  error <- expect_error(calibrate("id", payload))

  expect_equal(error$data[[1]]$error, scalar("VERSION_OUT_OF_DATE"))
  expect_equal(error$data[[1]]$detail, scalar(
    paste0("Trying to calibrate model with",
           " old version of options. Update calibration options")))
  expect_equal(error$status, 400)
})

test_that("calibrate fails with old model run result", {
  test_redis_available()
  test_mock_model_available()

  ## Return v0.1.38 model results
  q <- test_queue_result(model = mock_model_v0.1.38,
                         calibrate = mock_model_v0.1.38)

  endpoint <- endpoint_model_calibrate_submit(q$queue)
  res <- endpoint$run(q$model_run_id)
  expect_equal(res$value$status, scalar("failure"))
  expect_equal(res$value$errors[[1]]$error, scalar("SERVER_ERROR"))
  expect_equal(
    res$value$errors[[1]]$detail,
    scalar("Model output out of date please re-run model and try again."))
  expect_equal(res$status_code, 500)
})

test_that("can get data for calibration plot", {
  test_redis_available()
  test_mock_model_available()

  q <- test_queue_result()

  endpoint <- calibrate_plot(q$queue)
  res <- endpoint(q$calibrate_id)
  expect_setequal(names(res), c("data", "metadata"))
  expect_setequal(names(res$data),
                  c("data_type", "spectrum_region_code", "spectrum_region_name",
                    "sex", "age_group", "calendar_quarter", "indicator",
                    "mean"))
  expect_true(nrow(res$data) > 0)
  expect_false(any(is.na(res$data$mean)))
  expect_calibrate_plot_metadata(res$metadata)
})
