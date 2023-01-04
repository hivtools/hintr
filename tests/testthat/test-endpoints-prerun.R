test_that("prerun endpoint creates outputs and returns state", {
  prerun_setup <- setup_prerun_queue()
  prerun <- endpoint_prerun(prerun_setup$queue)
  res <- prerun$run(prerun_setup$payload)

  expect_equal(res$status_code, 200)
  expect_null(res$error)

  state <- res$data
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

test_that("api can call prerun endpoint", {
  prerun_setup <- setup_prerun_queue()
  api <- api_build(prerun_setup$queue)
  res <- api$request("POST", "/internal/prerun",
                     body = prerun_setup$payload)

  expect_equal(res$status, 200)

  out <- jsonlite::parse_json(res$body)
  expect_null(out$errors)
  state <- out$data
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
  expect_equal(state$model_fit$options$area_scope, "MWI")
  expect_equal(state$model_fit$options$survey_prevalence,
               list("DEMO2016PHIA", "DEMO2015DHS"))

  fit <- prerun_setup$queue$result(state$model_fit$id)
  expect_s3_class(fit, "hintr_output")
  expect_null(fit$plot_data_path)
  expect_true(file.exists(fit$model_output_path))

  ## Calibration
  expect_equal(state$calibrate$options$spectrum_plhiv_calibration_level,
               "none")
  calibrate <- prerun_setup$queue$result(state$calibrate$id)
  expect_s3_class(fit, "hintr_output")
  expect_true(file.exists(calibrate$plot_data_path))
  expect_true(file.exists(calibrate$model_output_path))

  ## Version
  expect_setequal(names(state$version),
                  c("hintr", "naomi", "rrq", "traduire"))
  expect_equal(state$version$naomi,
               as.character(packageVersion("naomi")))
})

test_that("prerun endpoint errors if file missing", {
  prerun_setup <- setup_prerun_queue()
  files <- list.files(prerun_setup$queue$inputs_dir, full.names = TRUE)
  anc <- files[basename(files) == "anc.csv"]
  programme <- files[basename(files) == "programme.csv"]
  unlink(anc)
  unlink(programme)
  prerun <- endpoint_prerun(prerun_setup$queue)
  res <- prerun$run(prerun_setup$payload)

  expect_equal(res$status_code, 400)
  expect_equal(res$value$errors[[1]]$error,
               scalar("PRERUN_MISSING_FILES"))
  expect_equal(res$value$errors[[1]]$detail, scalar(sprintf(paste0(
    "File 'programme' at path '%s' with original name '%s' does not exist.\n",
    "File 'anc' at path '%s' with original name '%s' does not exist.\n",
    "Make sure to upload them first with '/internal/upload/*' endpoints."),
    programme, basename(programme), anc, basename(anc))))
})
