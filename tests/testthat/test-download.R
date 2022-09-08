test_that("download can include project state", {
  test_mock_model_available()
  q <- test_queue_result()

  model_output <- q$queue$result(q$calibrate_id)
  results <- tempfile()
  dir.create(results, FALSE, TRUE)
  out <- download(model_output,
           type = "spectrum",
           path_results = results,
           notes = "notes",
           state = '{"state": "example"}')

  expect_true(file.exists(out$path))
  dest <- tempfile()
  zip::unzip(out$path, exdir = dest)
  expect_true(PROJECT_STATE_PATH %in% list.files(dest, recursive = TRUE))
  state <- jsonlite::read_json(file.path(dest, PROJECT_STATE_PATH),
                               simplifyVector = FALSE)
  expect_equal(state, list(state = "example"))
})

test_that("download can be translated", {
  test_mock_model_available()
  q <- test_queue_result()

  model_output <- q$queue$result(q$calibrate_id)
  results <- tempfile()
  dir.create(results, FALSE, TRUE)
  out <- download(model_output,
                  type = "spectrum",
                  path_results = results,
                  notes = "notes",
                  state = '{"state": "example"}',
                  language = "fr")

  expect_true(file.exists(out$path))
  dest <- tempfile()
  zip::unzip(out$path, exdir = dest)
  expect_true(INDICATORS_PATH %in% list.files(dest))
  indicators <- read.csv(file.path(dest, INDICATORS_PATH))
  expect_true("Prévalence du VIH" %in% unique(indicators$indicator_label))
})
