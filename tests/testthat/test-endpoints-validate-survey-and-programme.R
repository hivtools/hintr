context("survey-and-programme")

test_that("endpoint_validate_survey_programme supports programme file", {
  input <- validate_programme_survey_input(
    file.path("testdata", "programme.csv"),
    "programme",
    file.path("testdata", "malawi.geojson"))
  response <- validate_survey_programme(input)

  expect_equal(response$filename, scalar("original"))
  expect_equal(response$hash, scalar("12345"))
  ## Sanity check that data has been returned
  expect_true(nrow(response$data) >= 200)
  expect_equal(typeof(response$data[, "current_art"]), "double")
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
    scalar("Data missing column year."))
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
  ## Sanity check that data has been returned
  expect_true(nrow(response$data) >= 200)
  expect_equal(typeof(response$data[, "prevalence"]), "double")
  expect_equal(typeof(response$data[, "art_coverage"]), "double")
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
  ## Sanity check that data has been returned
  expect_true(nrow(response$data) >= 20000)
  expect_equal(typeof(response$data[, "est"]), "double")
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

test_that("possible filters are returned for data", {
  input <- validate_programme_survey_input(
    file.path("testdata", "programme.csv"),
    "programme",
    file.path("testdata", "malawi.geojson"))
  response <- validate_survey_programme(input)

  expect_equal(names(response$filters), c("age", "year", "indicators"))
  expect_length(response$filters$age, 2)
  expect_equal(response$filters$age, list(
    list(
      id = scalar("15+"),
      label = scalar("15+")
    ),
    list(
      id = scalar("00-14"),
      label = scalar("0-14")
    )
  ))
  expect_length(response$filters$year, 8)
  expect_equal(response$filters$year[[1]]$id, scalar("2018"))
  expect_equal(response$filters$year[[1]]$label, scalar("2018"))

  expect_length(response$filters$indicators, 1)
  expect_equal(response$filters$indicators[[1]]$id, scalar("current_art"))
  expect_equal(response$filters$indicators[[1]]$label,
               scalar("ART number (attending)"))


  input <- validate_programme_survey_input(
    file.path("testdata", "anc.csv"),
    "anc",
    file.path("testdata", "malawi.geojson"))
  response <- validate_survey_programme(input)

  expect_equal(names(response$filters), c("year", "indicators"))
  expect_length(response$filters$year, 8)
  expect_equal(response$filters$year[[1]]$id, scalar("2018"))
  expect_equal(response$filters$year[[1]]$label, scalar("2018"))

  expect_length(response$filters$indicators, 2)
  expect_equal(response$filters$indicators[[1]]$id, scalar("prevalence"))
  expect_equal(response$filters$indicators[[1]]$label, scalar("HIV prevalence"))
  expect_equal(response$filters$indicators[[2]]$id, scalar("art_coverage"))
  expect_equal(response$filters$indicators[[2]]$label, scalar("ART coverage"))

  input <- validate_programme_survey_input(
    file.path("testdata", "survey.csv"),
    "survey",
    file.path("testdata", "malawi.geojson"))
  response <- validate_survey_programme(input)

  expect_equal(names(response$filters), c("age", "surveys", "indicators"))
  expect_length(response$filters$age, 21)
  expect_length(response$filters$surveys, 4)
  expect_equal(response$filters$surveys, list(
    list(
      id = scalar("MWI2016PHIA"),
      label = scalar("MWI2016PHIA")
    ),
    list(
      id = scalar("MWI2015DHS"),
      label = scalar("MWI2015DHS")
    ),
    list(
      id = scalar("MWI2010DHS"),
      label = scalar("MWI2010DHS")
    ),
    list(
      id = scalar("MWI2004DHS"),
      label = scalar("MWI2004DHS")
    )
  ))

  expect_length(response$filters$indicators, 4)
  expect_equal(response$filters$indicators[[1]]$id, scalar("prevalence"))
  expect_equal(response$filters$indicators[[1]]$label, scalar("HIV prevalence"))
  expect_equal(response$filters$indicators[[2]]$id, scalar("art_coverage"))
  expect_equal(response$filters$indicators[[2]]$label, scalar("ART coverage"))
  expect_equal(response$filters$indicators[[3]]$id, scalar("recent"))
  expect_equal(response$filters$indicators[[3]]$label,
               scalar("Proportion recently infected"))
  expect_equal(response$filters$indicators[[4]]$id, scalar("vls"))
  expect_equal(response$filters$indicators[[4]]$label,
               scalar("Viral load suppression"))
})
