context("endpoints-validate")

test_that("endpoint_validate_baseline correctly validates data", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  req <- list(postBody = '{"type": "pjnz", "path": "path/to/file"}')
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_baseline(req, res, "pjnz", pjnz)
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "Botswana2018.PJNZ")
  expect_equal(response$data$data$country, "Botswana")
  expect_equal(res$status, 200)
})

test_that("endpoint_validate_baseline returns error on invalid data", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  req <- list(postBody = '{"type": "pjnz", "path": "path/to/file"}')
  mock_read_country <- mockery::mock("GBR")
  with_mock("hintr:::read_country" = mock_read_country, {
    res <- MockPlumberResponse$new()
    response <- endpoint_validate_baseline(req, res, "pjnz", pjnz)
    response <- jsonlite::parse_json(response)
    expect_equal(response$status, "failure")
    expect_length(response$errors, 1)
    expect_equal(response$errors[[1]]$error, "INVALID_FILE")
    expect_equal(response$errors[[1]]$detail, "Invalid country")
    expect_equal(res$status, 400)
  })
})

test_that("endpoint_validate_baseline returns nice error if file does not exist", {
  req <- list(postBody = '{"type": "pjnz", "path": "path/to/file"}')
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_baseline(req, res, "pjnz", "path/to/file")
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "INVALID_FILE")
  expect_equal(response$errors[[1]]$detail,
               "File at path path/to/file does not exist. Create it, or fix the path.")
  expect_equal(res$status, 400)

})

test_that("endpoint_validate_baseline validates the input and response", {
  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  mock_validate_json_schema <- mockery::mock(TRUE, cycle = TRUE)
  with_mock("hintr:::validate_json_schema" = mock_validate_json_schema, {
    ret <- endpoint_validate_baseline(list(postBody = "request"),
                                   MockPlumberResponse$new(), "pjnz", pjnz)
  })

  mockery::expect_called(mock_validate_json_schema, 4)
  mockery::expect_args(mock_validate_json_schema, 1, "request",
                       "ValidateInputRequest")
  mockery::expect_args(mock_validate_json_schema, 4, ret,
                       "Response")
  mockery::expect_args(mock_validate_json_schema, 3, ret,
                       "ValidateInputResponse", "data")
})

test_that("endpoint_validate_baseline support shape file", {
  shape <- file.path("testdata", "malawi.geojson")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_baseline(
    list(postBody = '{"type":"shape","path":"path/to/file"}'),
    res,
    "shape",
    shape)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "malawi.geojson")
  expect_true(all(c("type", "features") %in% names(response$data$data)))
  expect_equal(length(response$data$data$features), 69)
  expect_equal(res$status, 200)
  expect_equal(names(response$data$filters), c("regions", "level_labels"))
})

test_that("endpoint_validate_baseline supports population file", {
  population <- file.path("testdata", "population.csv")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_baseline(
    list(postBody = '{"type":"population","path":"path/to/file"}'),
    res,
    "population",
    population)
  response <- jsonlite::parse_json(response)

  expect_equal(response$status, "success")
  expect_equal(response$data$filename, "population.csv")
  expect_length(response$data$data, 0)
  expect_equal(res$status, 200)
})
