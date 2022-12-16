test_that("prerun returns project state", {
  test_redis_available()
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

test_that("hintr_submit_prerun fails if invalid object used", {
  expect_error(hintr_submit_prerun(NULL, NULL, NULL),
               "Model output must be hintr_output object")
  expect_error(hintr_submit_prerun(NULL, mock_model, NULL),
               "Calibrate output must be hintr_output object")
})

test_that("error returned if httr request fails", {
  expect_error(
    hintr_submit_prerun(prerun_inputs, mock_model, mock_calibrate,
                        "https://example.com", port = NULL),
    "Not Found (HTTP 404)",
    fixed = TRUE)
})

test_that("hintr_submit_prerun uploads files and returns output zip", {
  test_redis_available()
  inputs_dir <- tempfile()
  dir.create(inputs_dir)
  results_dir <- tempfile()
  dir.create(results_dir)
  server <- porcelain::porcelain_background$new(
    api, args = list(queue_id = paste0("hintr:", ids::random_id()),
                     workers = 0,
                     results_dir = results_dir,
                     inputs_dir = inputs_dir))
  server$start()

  existing_inputs <- length(list.files(inputs_dir))
  existing_outputs <- length(list.files(results_dir))

  t <- tempfile(fileext = ".zip")
  out <- hintr_submit_prerun(prerun_inputs, mock_model, mock_calibrate,
                             "http://localhost", port = server$port,
                             output_zip_path = t)
  expect_equal(out, t)

  expect_equal(length(list.files(inputs_dir)) - existing_inputs, 6)
  expect_equal(length(list.files(results_dir)) - existing_outputs, 3)

  dest <- tempfile()
  zip::unzip(t, exdir = dest)
  expect_true(PROJECT_STATE_PATH %in% list.files(dest, recursive = TRUE))
  state <- jsonlite::read_json(file.path(dest, PROJECT_STATE_PATH),
                               simplifyVector = FALSE)
  expect_equal(names(state), c("datasets", "model_fit", "calibrate", "version"))
})
