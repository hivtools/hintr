context("validate-inputs")

test_that("PJNZ can be validated and return data", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  expect_equal(do_validate_pjnz(pjnz), list(country = scalar("Botswana")))
  mock_read_country <- mockery::mock("GBR")
  with_mock("hintr:::read_country" = mock_read_country, {
    expect_error(do_validate_pjnz(pjnz), "Invalid country")
  })
})

test_that("country can be read from PJNZ file", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  expect_equal(read_country(pjnz), "Botswana")
})

test_that("do_validate_shape validates shape and returns geojson as list", {
  shape <- file.path("testdata", "malawi.geojson")
  json <- do_validate_shape(shape)
  expect_s3_class(json, "json")
})

test_that("do_validate_population validates population file", {
  population <- file.path("testdata", "population.csv")
  pop <- do_validate_population(population)
  ## No actual data to return but has been validated
  expect_true(is.na(pop))
})

test_that("do_validate_programme validates programme file", {
  programme <- file.path("testdata", "programme.csv")
  data <- do_validate_programme(programme)
  ## Some arbitrary test that the data has actually been returned
  expect_true(nrow(data) > 1400)
  expect_equal(typeof(data$value), "integer")
})

test_that("do_validate_anc validates ANC file", {
  anc <- file.path("testdata", "anc.csv")
  data <- do_validate_anc(anc)
  ## Some arbitrary test that the data has actually been returned
  expect_true(nrow(data) > 800)
  expect_equal(typeof(data$value), "double")
})

test_that("do_validate_survey validates survey file", {
  survey <- file.path("testdata", "survey.csv")
  data <- do_validate_survey(survey)
  ## Some arbitrary test that the data has actually been returned
  expect_true(nrow(data) > 30000)
  expect_equal(typeof(data$value), "double")
})

test_that("baseline data can be validated as a collection", {
  pjnz <- file.path("testdata", "Malawi2019.PJNZ")
  shape <- file.path("testdata", "malawi.geojson")
  population <- file.path("testdata", "population.csv")
  response <- do_validate_baseline(pjnz, shape, population)
  expect_true(response$consistent)
  expect_true(response$complete)

  response <- do_validate_baseline(NULL, shape, population)
  expect_true(response$consistent)
  expect_false(response$complete)

  response <- do_validate_baseline(pjnz, NULL, population)
  expect_true(response$consistent)
  expect_false(response$complete)

  response <- do_validate_baseline(pjnz, shape, NULL)
  expect_true(response$consistent)
  expect_false(response$complete)

  botswana_pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  expect_error(do_validate_baseline(botswana_pjnz, shape, NULL))

  response <- do_validate_baseline(botswana_pjnz, NULL, population)
  expect_true(response$consistent)
  expect_false(response$complete)
})
