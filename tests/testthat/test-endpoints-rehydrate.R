test_that("rehydrate returns json", {
  payload <- setup_rehydrate_payload()
  input <- jsonlite::fromJSON(payload)
  out <- rehydrate_from_state(input$file)

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
  q <- test_queue_result()

  ## Submit rehydrate request
  payload <- setup_rehydrate_payload()
  submit <- endpoint_rehydrate_submit(q$queue)
  submit_response <- submit$run(payload)

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
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

test_that("api can rehydrate", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()
  api <- api_build(q$queue)

  ## Submit rehydrate request
  payload <- setup_rehydrate_payload()
  submit <- api$request("POST", "/rehydrate/submit", body = payload)
  expect_equal(submit$status, 200)
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
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
  q <- test_queue_result()

  ## Submit rehydrate request which will error
  payload <- setup_rehydrate_payload("testdata/Malawi2019.PJNZ")
  submit <- endpoint_rehydrate_submit(q$queue)
  submit_response <- submit$run(payload)
  expect_equal(submit_response$status_code, 400)
  expect_equal(submit_response$value$errors[[1]]$error,
               scalar("REHYDRATE_SUBMIT_FAILED"))
  expect_equal(submit_response$value$errors[[1]]$detail,
               scalar(paste0("Cannot load from this zip file, archive missing",
                             " required information. Please regenerate output",
                             " zip and try again.")))
})

test_that("trying to rehydrate with no notes does not error", {
  ## Setup payload and remove notes.txt from zip
  payload <- setup_rehydrate_payload()
  input <- jsonlite::fromJSON(payload)
  t <- tempfile()
  dir.create(t, FALSE, TRUE)
  zip::unzip(input$file$path, exdir = t)
  file.remove(file.path(t, NOTES_PATH))
  zip_without_notes <- tempfile(fileext = ".zip")
  zip::zip(zip_without_notes, list.files(t), root = t)
  input$file$path <- zip_without_notes

  out <- rehydrate_from_state(input$file)

  state <- jsonlite::fromJSON(out$state)
  expect_setequal(names(state),
                  c("datasets", "model_fit", "calibrate", "version"))
  expect_setequal(
    names(state$datasets),
    c("pjnz", "population", "shape", "survey", "programme", "anc"))

  expect_null(out$notes)
})

test_that("rehydrate from files works", {
  test_mock_model_available()
  q <- test_queue_result()

  hintr_output <- q$queue$result(q$calibrate_id)
  output <- rehydrate_from_files(hintr_output,
                                 q$model_run_id,
                                 q$calibrate_id)

  expect_equal(output$notes, scalar(""))
  state <- output$state

  ## Input data
  expect_setequal(
    names(state$datasets),
    c("pjnz", "population", "shape", "survey", "programme", "anc"))
  for (data in state$datasets) {
    expect_setequal(names(data), c("path", "filename"))
    expect_match(data$path, "uploads/[A-Z0-9]+.[A-Za-z]+")
  }

  ## Model fit
  expect_equal(state$model_fit$options$area_scope, scalar("MWI"))
  expect_equal(state$model_fit$options$survey_prevalence,
               list(scalar("DEMO2016PHIA"), scalar("DEMO2015DHS")))
  fit <- q$queue$result(state$model_fit$id)
  expect_s3_class(fit, "hintr_output")
  expect_true(file.exists(fit$model_output_path))

  ## Calibration
  expect_equal(state$calibrate$options$spectrum_plhiv_calibration_level,
               scalar("none"))
  calibrate <- q$queue$result(state$calibrate$id)
  expect_s3_class(calibrate, "hintr_output")
  expect_true(file.exists(calibrate$plot_data_path))
  expect_true(file.exists(calibrate$model_output_path))

  ## Version
  expect_setequal(names(state$version),
                  c("hintr", "naomi", "rrq", "traduire"))
})

test_that("rehydrate endpoint from data returns json", {
  test_mock_model_available()
  q <- test_queue_result(workers = 1)

  ## Submit rehydrate request
  payload <- setup_rehydrate_payload(
    file.path("testdata", "malawi_rehydrate.zip"))
  submit <- endpoint_rehydrate_submit(q$queue)
  submit_response <- submit$run(payload)

  expect_equal(submit_response$status_code, 200)
  expect_true(!is.null(submit_response$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_response$data$id)
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

  expect_equal(response$data$notes, scalar(""))

  state <- response$data$state

  expect_equal(response$status_code, 200)
  expect_setequal(names(state),
                  c("datasets", "model_fit", "calibrate", "version"))

  ## Input data
  expect_setequal(
    names(state$datasets),
    c("pjnz", "population", "shape", "survey", "programme", "anc"))
  for (data in state$datasets) {
    expect_setequal(names(data), c("path", "filename"))
    expect_match(data$path, "uploads/[A-Z0-9]{32}.[a-z]+")
  }

  ## Model fit
  expect_equal(state$model_fit$options$area_scope, scalar("MWI"))
  expect_equal(state$model_fit$options$survey_prevalence,
               list(scalar("DEMO2016PHIA"), scalar("DEMO2015DHS")))
  fit <- q$queue$result(state$model_fit$id)
  expect_s3_class(fit, "hintr_output")
  expect_true(file.exists(fit$plot_data_path))
  expect_true(file.exists(fit$model_output_path))

  ## Calibration
  expect_equal(state$calibrate$options$spectrum_plhiv_calibration_level,
               scalar("subnational"))
  calibrate <- q$queue$result(state$calibrate$id)
  expect_s3_class(calibrate, "hintr_output")
  expect_true(file.exists(calibrate$plot_data_path))
  expect_true(file.exists(calibrate$model_output_path))

  ## Version
  expect_setequal(names(state$version),
                  c("hintr", "naomi", "rrq", "traduire"))
})

test_that("api can rehydrate", {
  test_redis_available()
  test_mock_model_available()
  q <- test_queue_result()
  api <- api_build(q$queue)

  ## Submit rehydrate request
  payload <- setup_rehydrate_payload(
    file.path("testdata", "malawi_rehydrate.zip"))
  submit <- api$request("POST", "/rehydrate/submit", body = payload)
  expect_equal(submit$status, 200)
  submit_body <- jsonlite::fromJSON(submit$body)
  expect_true(!is.null(submit_body$data$id))

  ## Status
  out <- q$queue$queue$task_wait(submit_body$data$id)
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
  state <- res$data$state
  expect_setequal(names(res$data$state),
                  c("datasets", "model_fit", "calibrate", "version"))

  ## Input data
  expect_setequal(
    names(state$datasets),
    c("pjnz", "population", "shape", "survey", "programme", "anc"))
  for (data in state$datasets) {
    expect_setequal(names(data), c("path", "filename"))
    expect_match(data$path, "uploads/[A-Z0-9]{32}.[a-z]+")
  }

  ## Model fit
  expect_equal(state$model_fit$options$area_scope, "MWI")
  expect_equal(state$model_fit$options$survey_prevalence,
               c("DEMO2016PHIA", "DEMO2015DHS"))
  fit <- q$queue$result(state$model_fit$id)
  expect_s3_class(fit, "hintr_output")
  expect_true(file.exists(fit$plot_data_path))
  expect_true(file.exists(fit$model_output_path))

  ## Calibration
  expect_equal(state$calibrate$options$spectrum_plhiv_calibration_level,
               "subnational")
  calibrate <- q$queue$result(state$calibrate$id)
  expect_s3_class(calibrate, "hintr_output")
  expect_true(file.exists(calibrate$plot_data_path))
  expect_true(file.exists(calibrate$model_output_path))

  ## Version
  expect_setequal(names(state$version),
                  c("hintr", "naomi", "rrq", "traduire"))
})
