test_that("prerun returns project state", {
  prerun_setup <- setup_prerun_queue()
  run_prerun <- prerun(prerun_setup$queue)
  state <- run_prerun(prerun_setup$payload)

  expect_setequal(names(state),
                  c("datasets", "model_fit", "calibrate", "version"))

  ## Input data
  expect_setequal(
    names(state$datasets),
    c("pjnz", "population", "shape", "survey", "programme", "anc"))
  for (data in state$datasets) {
    expect_setequal(names(data), c("path", "filename"))
    expect_match(data$path,
                 paste0(substring(prerun_setup$queue$inputs_dir, 2), "/\\w+"))
  }

  ## Model fit
  expect_equal(state$model_fit$options$area_scope, scalar("MWI"))
  expect_equal(state$model_fit$options$survey_prevalence,
               list(scalar("DEMO2016PHIA"), scalar("DEMO2015DHS")))

  fit <- prerun_setup$queue$result(state$model_fit$id)
  expect_s3_class(fit, "hintr_output")
  expect_null(fit$plot_data_path)
  expect_true(file.exists(fit$model_output_path))

  ## Calibration
  expect_equal(state$calibrate$options$spectrum_plhiv_calibration_level,
               scalar("none"))
  calibrate <- prerun_setup$queue$result(state$calibrate$id)
  expect_s3_class(fit, "hintr_output")
  expect_true(file.exists(calibrate$plot_data_path))
  expect_true(file.exists(calibrate$model_output_path))

  ## Version
  expect_setequal(names(state$version),
                  c("hintr", "naomi", "rrq", "traduire"))
  expect_equal(state$version$naomi,
               scalar(as.character(packageVersion("naomi"))))
})
