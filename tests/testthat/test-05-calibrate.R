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
  out <- q$queue$queue$task_wait(res$id)
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
  expect_equal(names(result), c("data", "plottingMetadata", "warnings"))
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
  expect_equal(nrow(barchart$indicators), 25)

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
                 "unaware_plhiv_num", "aware_plhiv_num", "plhiv_attend",
                 "untreated_plhiv_attend", "aware_plhiv_attend",
                 "unaware_plhiv_attend", "incidence",
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
  expect_equal(nrow(choropleth$indicators), 25)

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
                 "unaware_plhiv_num", "aware_plhiv_num", "plhiv_attend",
                 "untreated_plhiv_attend", "aware_plhiv_attend",
                 "unaware_plhiv_attend", "incidence",
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
  expect_setequal(names(res), c("data", "plottingMetadata"))
  expect_setequal(names(res$data),
                  c("data_type", "spectrum_region_code", "spectrum_region_name",
                    "sex", "age_group", "calendar_quarter", "indicator",
                    "mean"))
  expect_true(nrow(res$data) > 0)
  expect_equal(names(res$plottingMetadata), "barchart")
  expect_setequal(names(res$plottingMetadata$barchart),
                  c("indicators", "filters", "defaults"))

  expect_setequal(names(res$plottingMetadata$barchart$indicators),
                  c("indicator", "value_column", "error_low_column",
                    "error_high_column", "indicator_column", "indicator_value",
                    "indicator_sort_order", "name", "scale", "accuracy",
                    "format"))
  expect_true(nrow(res$plottingMetadata$barchart$indicators) > 0)

  filters <- lapply(res$plottingMetadata$barchart$filters, function(filter) {
    filter$column_id
  })
  expect_equal(filters[[1]], scalar("spectrum_region_code"))
  expect_equal(filters[[2]], scalar("calendar_quarter"))
  expect_equal(filters[[3]], scalar("sex"))
  expect_equal(filters[[4]], scalar("age_group"))
  expect_equal(filters[[5]], scalar("data_type"))

  expect_setequal(names(res$plottingMetadata$barchart$defaults),
                 c("indicator_id", "x_axis_id", "disaggregate_by_id",
                   "selected_filter_options"))

  expect_false(any(is.na(res$data$mean)))
})
