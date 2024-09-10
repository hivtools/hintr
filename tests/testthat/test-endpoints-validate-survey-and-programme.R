test_that("endpoint_validate_survey_programme supports programme file", {
  input <- validate_programme_survey_input(
    file.path("testdata", "programme.csv"),
    "programme",
    file.path("testdata", "malawi.geojson"))
  response <- validate_survey_programme(input)

  expect_equal(response$filename, scalar("original"))
  expect_equal(response$hash, scalar("12345"))
  expect_equal(response$fromADR, scalar(FALSE))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data) >= 200)
  expect_type(response$data[, "art_current"], "double")
  expect_length(response$warnings, 0)
})

test_that("endpoint_validate_survey_programme returns error on invalid programme data", {
  input <- validate_programme_survey_input(
    file.path("testdata", "malformed_programme.csv"),
    "programme",
    file.path("testdata", "malawi.geojson"))
  error <- expect_error(validate_survey_programme(input))

  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_equal(
    error$data[[1]]$detail,
    scalar("Data missing column calendar_quarter."))
  expect_equal(error$status_code, 400)
})

test_that("endpoint_validate_survey_programme supports ANC file", {
  input <- validate_programme_survey_input(
    file.path("testdata", "anc.csv"),
    "anc",
    file.path("testdata", "malawi.geojson"))
  response <- validate_survey_programme(input)

  expect_equal(response$filename, scalar("original"))
  expect_equal(response$hash, scalar("12345"))
  expect_equal(response$fromADR, scalar(FALSE))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data) >= 200)
  expect_type(response$data[, "anc_prevalence"], "double")
  expect_type(response$data[, "anc_art_coverage"], "double")
  expect_length(response$warnings, 0)
})

test_that("endpoint_validate_survey_programme returns error on invalid ANC data", {
  input <- validate_programme_survey_input(
    file.path("testdata", "malformed_anc.csv"),
    "anc",
    file.path("testdata", "malawi.geojson"))
  error <- expect_error(validate_survey_programme(input))

  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_equal(
    error$data[[1]]$detail,
    scalar(paste0("Anc file contains regions for more than one country. ",
                  "Got countries MWI, AGO.")))
  expect_equal(error$status_code, 400)
})

test_that("endpoint_validate_survey_programme supports survey file", {
  survey <- file.path("testdata", "survey.csv")
  input <- validate_programme_survey_input(
    file.path("testdata", "survey.csv"),
    "survey",
    file.path("testdata", "malawi.geojson"))
  response <- validate_survey_programme(input)

  expect_equal(response$filename, scalar("original"))
  expect_equal(response$hash, scalar("12345"))
  expect_equal(response$fromADR, scalar(FALSE))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data) >= 20000)
  expect_type(response$data[, "estimate"], "double")
  expect_length(response$warnings, 0)
})

test_that("endpoint_validate_survey_programme returns error on invalid survey data", {
  input <- validate_programme_survey_input(
    file.path("testdata", "malformed_survey.csv"),
    "survey",
    file.path("testdata", "malawi.geojson"))
  error <- expect_error(validate_survey_programme(input))

  expect_equal(error$data[[1]]$error, scalar("INVALID_FILE"))
  expect_equal(
    error$data[[1]]$detail,
    scalar("Data missing column survey_id."))
  expect_equal(error$status_code, 400)
})

test_that("endpoint_validate_survey_programme programme", {
  endpoint <- endpoint_validate_survey_programme()
  response <- endpoint$run(
    system_file("payload", "validate_programme_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$filename, scalar("original.csv"))
  expect_equal(response$data$hash, scalar("12345"))
  expect_equal(response$data$fromADR, scalar(FALSE))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data$data) >= 200)
  expect_type(response$data$data[, "art_current"], "double")
  expect_length(response$data$warnings, 0)
})

test_that("endpoint_validate_survey_programme works with programme data", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request(
    "POST", "/validate/survey-and-programme",
    body = system_file("payload", "validate_programme_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$filename, "original.csv")
  expect_equal(body$data$hash, "12345")
  expect_equal(body$data$fromADR, FALSE)
  ## Sanity check that data has been returned
  expect_true(nrow(body$data$data) >= 200)
  expect_type(body$data$data[, "art_current"], "integer")
  expect_length(body$data$warnings, 0)
})

test_that("endpoint_validate_survey_programme anc", {
  endpoint <- endpoint_validate_survey_programme()
  response <- endpoint$run(system_file("payload", "validate_anc_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$filename, scalar("original.csv"))
  expect_equal(response$data$hash, scalar("12345"))
  expect_equal(response$data$fromADR, scalar(FALSE))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data$data) >= 200)
  expect_type(response$data$data[, "anc_prevalence"], "double")
  expect_type(response$data$data[, "anc_art_coverage"], "double")
  expect_length(response$data$warnings, 0)
})

test_that("endpoint_validate_survey_programme works with anc data", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request(
    "POST", "/validate/survey-and-programme",
    body = system_file("payload", "validate_anc_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$filename, "original.csv")
  expect_equal(body$data$hash, "12345")
  expect_equal(body$data$fromADR, FALSE)
  ## Sanity check that data has been returned
  expect_true(nrow(body$data$data) >= 200)
  expect_type(body$data$data[, "anc_prevalence"], "double")
  expect_type(body$data$data[, "anc_art_coverage"], "double")
  expect_length(body$data$warnings, 0)
})

test_that("endpoint_validate_survey_programme survey", {
  endpoint <- endpoint_validate_survey_programme()
  response <- endpoint$run(
    system_file("payload", "validate_survey_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$filename, scalar("original.csv"))
  expect_equal(response$data$hash, scalar("12345"))
  expect_equal(response$data$fromADR, scalar(FALSE))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data$data) >= 20000)
  expect_type(response$data$data[, "estimate"], "double")
})

test_that("endpoint_validate_survey_programme works with survey data", {
  test_redis_available()
  queue <- test_queue(workers = 0)
  api <- api_build(queue)
  res <- api$request(
    "POST", "/validate/survey-and-programme",
    body = system_file("payload", "validate_survey_payload.json"))
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$filename, "original.csv")
  expect_equal(body$data$fromADR, FALSE)
  expect_equal(body$data$hash, "12345")
  ## Sanity check that data has been returned
  expect_true(nrow(body$data$data) >= 20000)
  expect_type(body$data$data[, "estimate"], "double")
})

test_that("endpoint_validate_survey_programme vmmc", {
  endpoint <- endpoint_validate_survey_programme()
  response <- endpoint$run(
    system_file("payload", "validate_vmmc_payload.json"))

  expect_equal(response$status_code, 200)
  expect_null(response$error)
  expect_equal(response$data$filename, scalar("original.xlsx"))
  expect_equal(response$data$hash, scalar("12345"))
  expect_equal(response$data$fromADR, scalar(FALSE))
  ## No data returned from this endpoint
  expect_equal(response$data$data, json_verbatim("null"))
  expect_equal(response$data$warnings, list())
})

test_that("anc data can be validated can be run with relaxed validation", {
  test_redis_available()
  ## Create some data which will fail when validation is strict but
  ## not otherwise
  anc <- read.csv("testdata/anc.csv")
  anc[1, "anc_tested_pos"] <- anc[1, "anc_tested"] + 5
  t <- tempfile(fileext = ".csv")
  write.csv(anc, t, row.names = FALSE)

  input <- validate_programme_survey_input(
    normalizePath(t, winslash = "/"),
    "anc",
    file.path("testdata", "malawi.geojson"))
  queue <- test_queue(workers = 0)
  api <- api_build(queue)

  ## In strict mode
  res <- api$request("POST", "/validate/survey-and-programme",
                     body = input)
  expect_equal(res$status, 400)
  body <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  expect_equal(body$status, "failure")
  expect_length(body$errors, 1)
  expect_equal(body$errors[[1]]$error, "INVALID_FILE")
  expect_equal(body$errors[[1]]$detail,
    paste0("The number of people who tested positive is greater than the ",
           "number of people tested"))

  ## With strict = FALSE
  res <- api$request("POST", "/validate/survey-and-programme",
                     query = list(strict = "false"),
                     body = input)
  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_equal(body$data$hash, "12345")
  expect_equal(body$data$fromADR, FALSE)
  ## Sanity check that data has been returned
  expect_true(nrow(body$data$data) >= 200)
  expect_type(body$data$data[, "anc_prevalence"], "double")
  expect_type(body$data$data[, "anc_art_coverage"], "double")
  expect_length(body$data$warnings, 0)
})

test_that("file read errors early if file only partially read", {
  t <- tempfile(fileext = ".csv")
  writeLines("file,header\nrow1,value1\nrow2\none,two", t)

  expect_error(do_validate_programme(file_object(t), NULL),
               "Stopped early on line 3")
})
