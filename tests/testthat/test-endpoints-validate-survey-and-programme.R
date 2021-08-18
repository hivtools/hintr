context("survey-and-programme")

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

  expect_equal(names(response$filters), c("age", "calendar_quarter", "indicators"))
  expect_length(response$filters$age, 2)
  expect_equal(response$filters$age, list(
    list(
      id = scalar("Y015_999"),
      label = scalar("15+")
    ),
    list(
      id = scalar("Y000_014"),
      label = scalar("0-14")
    )
  ))
  expect_length(response$filters$calendar_quarter, 8)
  expect_equal(response$filters$calendar_quarter[[1]]$id, scalar("CY2018Q4"))
  expect_equal(response$filters$calendar_quarter[[1]]$label, scalar("December 2018"))

  expect_length(response$filters$indicators, 1)
  expect_equal(response$filters$indicators[[1]]$id, scalar("art_current"))
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
  expect_equal(response$filters$indicators[[1]]$id, scalar("anc_prevalence"))
  expect_equal(response$filters$indicators[[1]]$label, scalar("ANC HIV prevalence"))
  expect_equal(response$filters$indicators[[2]]$id, scalar("anc_art_coverage"))
  expect_equal(response$filters$indicators[[2]]$label, scalar("ANC prior ART coverage"))

  input <- validate_programme_survey_input(
    file.path("testdata", "survey.csv"),
    "survey",
    file.path("testdata", "malawi.geojson"))
  response <- validate_survey_programme(input)

  expect_equal(names(response$filters), c("age", "surveys", "indicators"))
  expect_length(response$filters$age, 23)
  expect_length(response$filters$surveys, 4)
  expect_equal(response$filters$surveys, list(
    list(
      id = scalar("DEMO2016PHIA"),
      label = scalar("DEMO2016PHIA")
    ),
    list(
      id = scalar("DEMO2015DHS"),
      label = scalar("DEMO2015DHS")
    ),
    list(
      id = scalar("DEMO2010DHS"),
      label = scalar("DEMO2010DHS")
    ),
    list(
      id = scalar("DEMO2004DHS"),
      label = scalar("DEMO2004DHS")
    )
  ))

  expect_length(response$filters$indicators, 4)
  expect_equal(response$filters$indicators[[1]]$id, scalar("prevalence"))
  expect_equal(response$filters$indicators[[1]]$label, scalar("HIV prevalence"))
  expect_equal(response$filters$indicators[[2]]$id, scalar("art_coverage"))
  expect_equal(response$filters$indicators[[2]]$label, scalar("ART coverage"))
  expect_equal(response$filters$indicators[[3]]$id, scalar("recent_infected"))
  expect_equal(response$filters$indicators[[3]]$label,
               scalar("Proportion recently infected"))
  expect_equal(response$filters$indicators[[4]]$id, scalar("viral_suppression_plhiv"))
  expect_equal(response$filters$indicators[[4]]$label,
               scalar("Viral load suppression"))
})
