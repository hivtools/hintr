context("survey-and-programme")

test_that("endpoint_validate_survey_programme supports programme file", {
  programme <- file.path("testdata", "programme.csv")
  shape <- file.path("testdata", "malawi.geojson")
  file <- list(path = programme, hash = "12345", filename = "original")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_survey_programme(
    list(postBody = '{"type":"programme", "file": {"path":"path/to/file","hash": "12345","filename":"original"}, "shape":"path"}'),
    res,
    "programme",
    file,
    shape)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "original")
  expect_equal(response$data$hash, "12345")
  expect_equal(res$status, 200)
  ## Sanity check that data has been returned
  expect_true(length(response$data$data) >= 200)
  expect_equal(typeof(response$data$data[[1]]$current_art), "double")
})

test_that("endpoint_validate_survey_programme returns error on invalid programme data", {
  programme <- file.path("testdata", "malformed_programme.csv")
  shape <- file.path("testdata", "malawi.geojson")
  file <- list(path = programme, hash = "12345", filename = "original")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_survey_programme(
    list(postBody = '{"type":"programme", "file": {"path":"path/to/file","hash": "12345","filename":"original"}, "shape":"path"}'),
    res,
    "programme",
    file,
    shape)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "failure")
  expect_equal(res$status, 400)
  expect_equal(response$data, structure(list(), names = character(0)))
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail, "Data missing column year.")
})

test_that("endpoint_validate_survey_programme supports ANC file", {
  anc <- file.path("testdata", "anc.csv")
  shape <- file.path("testdata", "malawi.geojson")
  file <- list(path = anc, hash = "12345", filename = "original")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_survey_programme(
    list(postBody = '{"type":"anc", "file": {"path":"path/to/file","hash": "12345","filename":"original"}, "shape":"path"}'),
    res,
    "anc",
    file,
    shape)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "original")
  expect_equal(response$data$hash, "12345")
  expect_equal(res$status, 200)
  ## Sanity check that data has been returned
  expect_true(length(response$data$data) >= 200)
  expect_equal(typeof(response$data$data[[1]]$prevalence), "double")
  expect_equal(typeof(response$data$data[[1]]$art_coverage), "integer")
})

test_that("endpoint_validate_survey_programme returns error on invalid ANC data", {
  anc <- file.path("testdata", "malformed_anc.csv")
  shape <- file.path("testdata", "malawi.geojson")
  file <- list(path = anc, hash = "12345", filename = "original")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_survey_programme(
    list(postBody = '{"type":"anc", "file": {"path":"path/to/file","hash": "12345","filename":"original"}, "shape":"path"}'),
    res,
    "anc",
    file,
    shape)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "failure")
  expect_equal(res$status, 400)
  expect_equal(response$data, structure(list(), names = character(0)))
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail,
               "Anc file contains regions for more than one country. Got countries MWI, AGO.")
})

test_that("endpoint_validate_survey_programme supports survey file", {
  survey <- file.path("testdata", "survey.csv")
  shape <- file.path("testdata", "malawi.geojson")
  file <- list(path = survey, hash = "12345", filename = "original")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_survey_programme(
    list(postBody = '{"type":"survey", "file": {"path":"path/to/file","hash": "12345","filename":"original"}, "shape":"path"}'),
    res,
    "survey",
    file,
    shape)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "original")
  expect_equal(response$data$hash, "12345")
  expect_equal(res$status, 200)
  ## Sanity check that data has been returned
  expect_true(length(response$data$data) >= 20000)
  expect_equal(typeof(response$data$data[[1]]$est), "double")
})

test_that("endpoint_validate_survey_programme returns error on invalid survey data", {
  survey <- file.path("testdata", "malformed_survey.csv")
  shape <- file.path("testdata", "malawi.geojson")
  file <- list(path = survey, hash = "12345", filename = "original")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_survey_programme(
    list(postBody = '{"type":"survey", "file": {"path":"path/to/file","hash": "12345","filename":"original"}, "shape":"path"}'),
    res,
    "survey",
    file,
    shape)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "failure")
  expect_equal(res$status, 400)
  expect_equal(response$data, structure(list(), names = character(0)))
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail, "Data missing column survey_id.")
})
