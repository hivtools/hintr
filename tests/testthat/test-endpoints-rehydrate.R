test_that("rehydrate returns json", {
  payload <- setup_payload_rehydrate()
  input <- jsonlite::fromJSON(payload)
  out <- rehydrate(input$file)

  state <- jsonlite::fromJSON(out$state)
  expect_setequal(names(state),
                  c("datasets", "model_fit", "calibrate", "version"))
  expect_setequal(
    names(state$datasets),
    c("pjnz", "population", "shape", "survey", "programme", "anc"))

  expect_match(out$notes, "These are my project notes")
})

test_that("rehydrate endpoint returns json", {
  test_mock_model_available()
  q <- test_queue_result(inputs_dir = normalizePath(test_path("testdata")))

  ## Submit rehydrate request
  payload <- setup_payload_rehydrate()
  submit <- endpoint_rehydrate_submit(q$queue)
  submit_response <- submit$run(payload)

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$task_wait(submit_response$data$id)
  status <- endpoint_rehydrate_status(q$queue)
  status_response <- status$run(submit_response$data$id)

  expect_equal(status_response$status_code, 200)
  expect_equal(status_response$data$id, submit_response$data$id)
  expect_true(status_response$data$done)
  expect_equal(status_response$data$status, scalar("COMPLETE"))
  expect_true(status_response$data$success)
  expect_equal(status_response$data$queue, scalar(0))
  expect_length(status_response$data$progress, 0)

  ## Get result
  result <- endpoint_rehydrate_result(q$queue)
  response <- result$run(status_response$data$id)
  state <- jsonlite::fromJSON(response$data$state)
  expect_equal(response$status_code, 200)
  expect_setequal(names(state),
                  c("datasets", "model_fit", "calibrate", "version"))
  expect_setequal(
    names(state$datasets),
    c("pjnz", "population", "shape", "survey", "programme", "anc"))
  expect_match(response$data$notes, "These are my project notes")
})

test_that("api can call spectrum download", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result(inputs_dir = normalizePath(test_path("testdata")))
  api <- api_build(q$queue)

  ## Submit rehydrate request
  payload <- setup_payload_rehydrate()
  submit <- api$request("POST", "/rehydrate/submit", body = payload)
  expect_equal(submit$status, 200)
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$task_wait(submit_body$data$id)
  status <- api$request("GET",
                        paste0("/rehydrate/status/", submit_body$data$id))

  expect_equal(status$status, 200)
  status_body <- jsonlite::fromJSON(status$body)
  expect_equal(status_body$data$id, submit_body$data$id)
  expect_true(status_body$data$done)
  expect_equal(status_body$data$status, "COMPLETE")
  expect_true(status_body$data$success)
  expect_equal(status_body$data$queue, 0)
  expect_length(status_body$data$progress, 0)

  ## Get result
  response <- api$request("GET",
                          paste0("/rehydrate/result/", submit_body$data$id))
  expect_equal(response$status, 200)
  res <- jsonlite::fromJSON(response$body)
  expect_equal(res$status, "success")
  expect_length(res$errors, 0)
  expect_setequal(names(res$data$state),
                  c("datasets", "model_fit", "calibrate", "version"))
  expect_setequal(
    names(res$data$state$datasets),
    c("pjnz", "population", "shape", "survey", "programme", "anc"))
  expect_match(res$data$notes, "These are my project notes")
})

test_that("rehydrate returns useful error if cannot rehydrate from zip", {
  test_mock_model_available()
  q <- test_queue_result(inputs_dir = normalizePath(test_path("testdata")))

  ## Submit rehydrate request which will error
  payload <- setup_payload_rehydrate(test_path("testdata/Botswana2018.PJNZ"))
  submit <- endpoint_rehydrate_submit(q$queue)
  submit_response <- submit$run(payload)
  expect_equal(submit_response$status_code, 200)

  ## Get result
  out <- q$queue$task_wait(submit_response$data$id)
  result <- endpoint_rehydrate_result(q$queue)
  response <- result$run(submit_response$data$id)
  expect_equal(response$status_code, 400)
  expect_equal(response$value$errors[[1]]$error,
               scalar("PROJECT_REHYDRATE_FAILED"))
  expect_equal(response$value$errors[[1]]$detail,
               scalar(paste0("Cannot load from this zip file, archive missing",
                             " required information. Please regenerate output",
                             " zip and try again.")))
})

test_that("rehydrate returns useful error when submission fails", {
  test_mock_model_available()
  q <- test_queue_result(inputs_dir = normalizePath(test_path("testdata")))

  payload <- setup_payload_rehydrate("missing/file.zip")
  submit <- endpoint_rehydrate_submit(q$queue)
  response <- submit$run(payload)

  expect_equal(response$status_code, 400)
  expect_equal(response$value$errors[[1]]$error,
               scalar("REHYDRATE_SUBMIT_FAILED"))
  expect_equal(response$value$errors[[1]]$detail,
               scalar(paste0("File at path missing/file.zip does not exist. ",
                             "Create it, or fix the path.")))
})

test_that("trying to rehydrate with no notes does not error", {
  ## Setup payload and remove notes.txt from zip
  payload <- setup_payload_rehydrate()
  input <- jsonlite::fromJSON(payload)
  t <- tempfile()
  dir.create(t, FALSE, TRUE)
  zip::unzip(input$file$path, exdir = t)
  file.remove(file.path(t, NOTES_PATH))
  zip_without_notes <- tempfile(fileext = ".zip")
  zip::zip(zip_without_notes, list.files(t), root = t)
  input$file$path <- zip_without_notes

  out <- rehydrate(input$file)

  state <- jsonlite::fromJSON(out$state)
  expect_setequal(names(state),
                  c("datasets", "model_fit", "calibrate", "version"))
  expect_setequal(
    names(state$datasets),
    c("pjnz", "population", "shape", "survey", "programme", "anc"))

  expect_null(out$notes)
})

test_that("rehydrate throws error if input files are unknown", {
  payload <- setup_payload_rehydrate()
  input <- jsonlite::fromJSON(payload)
  out <- rehydrate(input$file)

  mock_queue <- list(
    inputs_dir = tempdir(),
    result = function(id) out
  )

  get_rehydrate_result <- rehydrate_result(mock_queue)

  error <- expect_error(get_rehydrate_result("123"))
  expect_equal(error$data[[1]]$error, scalar("PROJECT_REHYDRATE_FAILED"))
  expect_match(error$data[[1]]$detail,
               scalar(paste("Failed to locate Spectrum file from this output",
                            "zip. Please contact a system admin.")))
  expect_equal(error$status_code, 400)
})

test_that("rehydrate throws error if model fit or calibrate IDs are unknown", {
  payload <- setup_payload_rehydrate()
  input <- jsonlite::fromJSON(payload)
  out <- rehydrate(input$file)
  state <- jsonlite::fromJSON(out$state, simplifyVector = FALSE)
  state$datasets[["pjnz"]]$path <- "Malawi2019.PJNZ"
  state$datasets[["population"]]$path <- "population.csv"
  state$datasets[["shape"]]$path <- "malawi.geojson"
  state$datasets[["survey"]]$path <- "survey.csv"
  state$datasets[["programme"]]$path <- "programme.csv"
  state$datasets[["anc"]]$path <- "anc.csv"
  out$state <- jsonlite::toJSON(state)


  mock_exists <- function(id) {
    FALSE
  }
  mock_queue <- list(
    inputs_dir = normalizePath(test_path("testdata")),
    result = function(id) out,
    exists = mock_exists
  )

  get_rehydrate_result <- rehydrate_result(mock_queue)

  error <- expect_error(get_rehydrate_result("123"))
  expect_equal(error$data[[1]]$error, scalar("PROJECT_REHYDRATE_FAILED"))
  expect_match(error$data[[1]]$detail,
               scalar(paste("Failed to find model fit result from this output",
                            "zip. Please contact a system admin.")))
  expect_equal(error$status_code, 400)

  ## Missing calibrate ID
  mock_exists <- function(id) {
    if (id == state$model_fit$id) {
      TRUE
    } else {
      FALSE
    }
  }
  mock_queue <- list(
    inputs_dir = normalizePath(test_path("testdata")),
    result = function(id) out,
    exists = mock_exists
  )

  get_rehydrate_result <- rehydrate_result(mock_queue)

  error <- expect_error(get_rehydrate_result("123"))
  expect_equal(error$data[[1]]$error, scalar("PROJECT_REHYDRATE_FAILED"))
  expect_match(error$data[[1]]$detail,
               scalar(paste("Failed to find calibration result from this",
                            "output zip. Please contact a system admin.")))
  expect_equal(error$status_code, 400)
})
