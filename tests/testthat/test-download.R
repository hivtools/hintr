test_that("download can include project state", {
  test_mock_model_available()
  q <- test_queue_result()

  model_output <- q$queue$result(q$calibrate_id)
  results <- tempfile()
  dir.create(results, FALSE, TRUE)
  out <- download(model_output,
           type = "spectrum",
           path_results = results,
           input = list(notes = "notes",
                        state = '{"state": "example"}'))

  expect_true(file.exists(out$path))
  dest <- tempfile()
  zip::unzip(out$path, exdir = dest)
  expect_true(PROJECT_STATE_PATH %in% list.files(dest, recursive = TRUE))
  state <- jsonlite::read_json(file.path(dest, PROJECT_STATE_PATH),
                               simplifyVector = FALSE)
  expect_equal(state, list(state = "example"))
})

test_that("download can be translated", {
  skip_unless_slow()
  test_mock_model_available()
  withr::local_envvar(USE_MOCK_DOWNLOAD = "false")
  q <- test_queue_result()

  model_output <- q$queue$result(q$calibrate_id)
  results <- tempfile()
  dir.create(results, FALSE, TRUE)
  out <- download(model_output,
                  type = "spectrum",
                  path_results = results,
                  input = list(notes = "notes",
                               state = '{"state": "example"}'),
                  language = "fr")

  expect_true(file.exists(out$path))
  dest <- tempfile()
  zip::unzip(out$path, exdir = dest)
  expect_true(INDICATORS_PATH %in% list.files(dest))
  indicators <- read.csv(file.path(dest, INDICATORS_PATH))
  expect_true("Prévalence du VIH" %in% unique(indicators$indicator_label))
})

test_that("real naomi downloads return what the mock download returns", {
  ## Guards against the mock in download_function() drifting from naomi
  skip_unless_slow()
  test_mock_model_available()
  model_output <- clone_model_output(mock_calibrate)
  results <- tempfile()
  dir.create(results)
  input <- list(notes = "notes",
                state = '{"model_fit": {"id": "1"}, "calibrate": {"id": "2"}}')
  for (type in c("spectrum", "coarse_output", "summary", "comparison",
                 "datapack")) {
    mock <- withr::with_envvar(
      c(USE_MOCK_DOWNLOAD = "true"),
      download(model_output, type, results, input))
    real <- withr::with_envvar(
      c(USE_MOCK_DOWNLOAD = "false"),
      download(model_output, type, results, input))
    expect_true(file.exists(real$path), label = type)
    expect_equal(tools::file_ext(real$path), tools::file_ext(mock$path),
                 label = type)
    expect_setequal(names(real$metadata), names(mock$metadata))
    expect_equal(real$metadata$type, mock$metadata$type)
    expect_equal(real$metadata$areas, mock$metadata$areas)
    if (type == "spectrum") {
      dest <- tempfile()
      zip::unzip(real$path, exdir = dest)
      expect_equal(readLines(file.path(dest, NOTES_PATH)), "notes")
    }
  }
})
