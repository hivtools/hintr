context("calibrate")

test_that("calibration can be run", {
  test_mock_model_available()

  model_output <- clone_model_output(mock_model)

  ## Calibrate the result
  path <- setup_calibrate_payload()
  calibration_options <- jsonlite::fromJSON(path)$options

  calibrated_result <- run_calibrate(model_output, calibration_options)

  ## Expected files are returned
  expect_equal(names(calibrated_result),
               c("output_path", "spectrum_path", "coarse_output_path",
                 "summary_report_path", "calibration_path", "metadata"))
})

test_that("calibrate can set language", {
  model_output <- list(calibration_path = "test")
  expect_error(run_calibrate(model_output, list(opt = "options")),
               paste0("Can't calibrate this model output please re-run ",
                      "model and try calibration again"))

  ## With french translation
  expect_error(run_calibrate(model_output, list(opt = "options"),
                             language = "fr"),
               paste0("Impossible de calibrer la sortie de ce modèle, veuillez",
               " réexécuter le modèle pour obtenir une sortie mise à jour"))
})

test_that("can calibrate a model result", {
  test_mock_model_available()

  ## Mock model run
  queue <- test_queue(workers = 1)
  unlockBinding("result", queue)
  ## Clone model output as it modifies in place
  out <- clone_model_output(mock_model)
  queue$result <- mockery::mock(out,  cycle = TRUE)
  mock_verify_result_available <- mockery::mock(TRUE)

  ## Calibrate the result
  path <- setup_calibrate_payload()
  with_mock("hintr:::verify_result_available" = mock_verify_result_available, {
    calibrate <- submit_calibrate(queue)
    res <- calibrate("id", readLines(path))
  })
  expect_equal(names(res), "id")
  expect_true(!is.null(res$id))

  ## Get status
  out <- queue$queue$task_wait(res$id)
  status <- queue_status(queue)
  res_status <- status(res$id)
  expect_equal(res_status$id, res$id)
  expect_true(res_status$done)
  expect_equal(res_status$status, scalar("COMPLETE"))
  expect_true(res_status$success)
  expect_equal(res_status$queue, scalar(0))
  expect_match(res_status$progress[[1]],
               "Generating report - [\\d.m\\s]+s elapsed", perl = TRUE)

  get_result <- calibrate_result(queue)
  result <- get_result(res$id)
  expect_equal(names(result), c("data", "plottingMetadata"))
  expect_equal(colnames(result$data),
               c("area_id", "sex", "age_group", "calendar_quarter",
                 "indicator", "mode", "mean", "lower", "upper"))
  expect_true(nrow(result$data) > 84042)
  expect_equal(names(result$plottingMetadata), c("barchart", "choropleth"))

  ## Barchart
  barchart <- result$plottingMetadata$barchart
  expect_equal(names(barchart), c("indicators", "filters", "defaults"))
  expect_length(barchart$filters, 4)
  expect_equal(names(barchart$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(barchart$filters[[2]]),
               c("id", "column_id", "label", "options"))
  ## Choropleth has the correct filters in correct order
  filters <- lapply(barchart$filters, function(filter) {
    filter$column_id
  })
  expect_equal(filters[[1]], scalar("area_id"))
  expect_equal(filters[[2]], scalar("calendar_quarter"))
  expect_equal(filters[[3]], scalar("sex"))
  expect_equal(filters[[4]], scalar("age_group"))
  expect_length(barchart$filters[[2]]$options, 3)
  expect_equal(barchart$filters[[2]]$options[[2]]$id, scalar("CY2018Q3"))
  expect_equal(barchart$filters[[2]]$options[[2]]$label,
               scalar("September 2018"))
  expect_true(length(barchart$filters[[4]]$options) >= 29)
  expect_equal(nrow(barchart$indicators), 20)

  ## Quarters are in descending order
  calendar_quarters <-
    lapply(barchart$filters[[2]]$options, function(option) {
      option$id
    })
  expect_equal(unlist(calendar_quarters),
               sort(unlist(calendar_quarters), decreasing = TRUE))


  ## Barchart indicators are in numeric id order
  expect_equal(barchart$indicators$indicator,
               c("population", "prevalence", "plhiv", "art_coverage",
                 "art_current_residents", "art_current",
                 "untreated_plhiv_num", "aware_plhiv_prop",
                 "unaware_plhiv_num", "incidence",
                 "infections", "anc_prevalence", "anc_art_coverage",
                 "anc_clients", "anc_plhiv", "anc_already_art",
                 "anc_art_new", "anc_known_pos",
                 "anc_tested_pos", "anc_tested_neg"))

  ## Choropleth
  choropleth <- result$plottingMetadata$choropleth
  expect_equal(names(choropleth), c("indicators", "filters"))
  expect_length(choropleth$filters, 4)
  expect_equal(names(choropleth$filters[[1]]),
               c("id", "column_id", "label", "options", "use_shape_regions"))
  expect_equal(names(choropleth$filters[[2]]),
               c("id", "column_id", "label", "options"))
  ## Choropleth has the correct filters in correct order
  filters <- lapply(choropleth$filters, function(filter) {
    filter$column_id
  })
  expect_equal(filters[[1]], scalar("area_id"))
  expect_equal(filters[[2]], scalar("calendar_quarter"))
  expect_equal(filters[[3]], scalar("sex"))
  expect_equal(filters[[4]], scalar("age_group"))
  expect_length(choropleth$filters[[2]]$options, 3)
  expect_equal(choropleth$filters[[2]]$options[[2]]$id, scalar("CY2018Q3"))
  expect_equal(choropleth$filters[[2]]$options[[2]]$label,
               scalar("September 2018"))
  expect_true(length(choropleth$filters[[4]]$options) >= 29)
  expect_equal(nrow(choropleth$indicators), 20)

  ## Quarters are in descending order
  calendar_quarters <-
    lapply(choropleth$filters[[2]]$options, function(option) {
      option$id
    })
  expect_equal(unlist(calendar_quarters),
               sort(unlist(calendar_quarters), decreasing = TRUE))

  ## Choropleth indicators are in numeric id order
  expect_equal(choropleth$indicators$indicator,
               c("population", "prevalence", "plhiv", "art_coverage",
                 "art_current_residents", "art_current",
                 "untreated_plhiv_num", "aware_plhiv_prop",
                 "unaware_plhiv_num", "incidence",
                 "infections", "anc_prevalence", "anc_art_coverage",
                 "anc_clients", "anc_plhiv", "anc_already_art",
                 "anc_art_new", "anc_known_pos",
                 "anc_tested_pos", "anc_tested_neg"))
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
  path <- setup_calibrate_payload('{
                               "hintr": "0.0.12",
                               "naomi": "0.0.15",
                               "rrq": "0.2.1"
                               }')
  with_mock("hintr:::verify_result_available" = mock_verify_result_available, {
    calibrate <- submit_calibrate(queue)
    error <- expect_error(calibrate("id", readLines(path)))
  })

  expect_equal(error$data[[1]]$error, scalar("VERSION_OUT_OF_DATE"))
  expect_equal(error$data[[1]]$detail, scalar(
    paste0("Trying to calibrate model with",
           " old version of options. Update calibration options")))
  expect_equal(error$status, 400)
})

test_that("trying to calibrate old model result returns error", {
  test_mock_model_available()

  ## Mock model run
  queue <- test_queue(workers = 1)
  unlockBinding("result", queue)
  ## Clone model output as it modifies in place
  out <- clone_model_output(mock_model_v0.1.2)
  queue$result <- mockery::mock(out, cycle = TRUE)
  mock_verify_result_available <- mockery::mock(TRUE)

  ## Calibrate the result
  with_mock("hintr:::verify_result_available" = mock_verify_result_available, {
    calibrate <- submit_calibrate(queue)
    res <- calibrate("id", readLines(setup_calibrate_payload()))
  })
  expect_equal(names(res), "id")
  expect_true(!is.null(res$id))

  ## Get status
  out <- queue$queue$task_wait(res$id)
  status <- queue_status(queue)
  res_status <- status(res$id)
  expect_equal(res_status$id, res$id)
  expect_true(res_status$done)
  expect_equal(res_status$status, scalar("ERROR"))
  expect_false(res_status$success)
  expect_equal(res_status$queue, scalar(0))
  expect_equal(res_status$progress, list())

  get_result <- calibrate_result(queue)
  error <- expect_error(get_result(res$id))
  expect_equal(error$data[[1]]$error, scalar("MODEL_RUN_FAILED"))
  expect_equal(error$data[[1]]$detail, scalar(
    paste0("Can't calibrate this model output please",
           " re-run model and try calibration again")))
  expect_equal(error$status, 400)
})

test_that("model calibration returns error if queueing fails", {
  test_redis_available()
  ## Create request data
  path <- setup_calibrate_payload()

  ## Create mocks
  queue <- test_queue(workers = 0)
  unlockBinding("result", queue)
  ## Clone model output as it modifies in place
  out <- clone_model_output(mock_model_v0.1.2)
  queue$result <- mockery::mock(out, cycle = TRUE)
  unlockBinding("queue", queue)
  unlockBinding("task_status", queue$queue)
  queue$queue$task_status <- mockery::mock("COMPLETE", cycle = TRUE)
  mock_submit_calibrate <- function(data, options) { stop("Failed to queue") }

  ## Call the endpoint
  calibrate <- submit_calibrate(queue)
  mockery::stub(calibrate, "queue$submit_calibrate", mock_submit_calibrate)
  error <- expect_error(calibrate("id", readLines(path)))

  expect_equal(error$data[[1]]$error, scalar("FAILED_TO_QUEUE"))
  expect_equal(error$data[[1]]$detail, scalar("Failed to queue"))
  expect_equal(error$status, 400)
})
