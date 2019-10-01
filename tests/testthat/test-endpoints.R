context("endpoints")

test_that("hintr_response correctly prepares response", {
  value <- list(
    success = TRUE,
    value = list(
      filename = scalar("original.pjnz"),
      hash = scalar("12345"),
      type = scalar("pjnz"),
      data = list(country = scalar("Botswana"))
    )
  )

  ## NOTE: using a schema here that will work for now at least, but if
  ## that gets stricter it won't!
  response <- hintr_response(value, "ValidateInputResponse")

  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "success")
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.pjnz")
  expect_equal(response$data$data$country, "Botswana")
  expect_equal(response$errors, list())

  value <- list(
    success = FALSE,
    errors = list(list(error = scalar("INVALID_PJNZ"),
                       detail = scalar("Example error")),
                  list(error = scalar("OTHER_ERROR"),
                       detail = scalar("Second example")))
  )
  response <- hintr_response(value, "ValidateInputRequest")
  response <- jsonlite::parse_json(response)
  expect_equal(response$status, "failure")
  expect_length(response$errors, 2)
  expect_length(response$errors[[1]]$error, 1)
  expect_equal(response$errors[[1]]$error[[1]], "INVALID_PJNZ")
  expect_length(response$errors[[1]]$detail, 1)
  expect_equal(response$errors[[1]]$detail[[1]], "Example error")
  expect_length(response$errors[[2]]$error, 1)
  expect_equal(response$errors[[2]]$error[[1]], "OTHER_ERROR")
  expect_length(response$errors[[2]]$detail, 1)
  expect_equal(response$errors[[2]]$detail[[1]], "Second example")
})

test_that("hintr_response distinguishes incorrect data schema", {
  ## This is a correct value for the ValidateInputResponse schema
  value <- list(
    success = TRUE,
    value = list(
      filename = scalar("test.pjnz"),
      type = scalar("pjnz"),
      hash = scalar("12345"),
      data = list(
        country = scalar("Botswana"))
    )
  )

  expect_error(
    hintr_response(value, "ValidateInputResponse"),
    NA)
  expect_error(
    hintr_response(value, "ModelResultResponse"),
    class = "validation_error")
})

test_that("with_success correctly builds success message", {
  expr <- "Passed validation"
  response <- with_success(expr)
  expect_true(response$success)
  expect_equal(response$value, "Passed validation")

  expr <- function() {
    stop("Failed validation")
  }
  err <- with_success(expr())
  expect_false(err$success)
  expect_equal(err$message, "Failed validation")
  expect_equal(err$type, "simpleError")
})

test_that("hintr errors correctly formats errors", {
  errors <- hintr_errors(list(KEY1 = "value1", KEY2 = NULL))
  expect_length(errors, 2)
  expect_equal(as.character(errors[[1]]$error), "KEY1")
  expect_s3_class(errors[[1]]$error, "scalar")
  expect_equal(as.character(errors[[1]]$detail), "value1")
  expect_s3_class(errors[[1]]$detail, "scalar")
  expect_equal(as.character(errors[[2]]$error), "KEY2")
  expect_s3_class(errors[[2]]$error, "scalar")
  expect_null(errors[[2]]$detail)
})

test_that("hintr API can be tested", {
  expect_s3_class(api_root(), "scalar")
})

test_that("plumber api can be built", {
  api <- api_build()
  expect_s3_class(api, "plumber")
  expect_length(api$routes, 4)
  expect_equal(names(api$routes),
               c("validate", "model", "meta", ""))
  expect_equal(names(api$routes$validate),
               c("baseline-individual", "baseline-combined", "survey-and-programme"))
  expect_equal(names(api$routes$model), c("submit", "status", "result"))
  expect_equal(names(api$routes$meta), "plotting")
})

test_that("format_response_data correctly formats data and validates it", {
  mock_validate <- mockery::mock(TRUE)
  file <- list(path = "path", hash = "12345", filename = "original.pjnz")
  with_mock("hintr:::validate_json_schema" = mock_validate, {
    response <- input_response(list(data = list(country = scalar("Botswana")),
                                    filters = scalar(NA)),
                               "pjnz",
                                file)
  })
  expect_equal(response$data$country, scalar("Botswana"))
  expect_equal(response$hash, scalar("12345"))
  mockery::expect_called(mock_validate, 1)
  args <- mockery::mock_args(mock_validate)[[1]]
  expected_data <- list(
    hash = "12345",
    type = "pjnz",
    data = list(
      country = "Botswana"
    ),
    filename = "original.pjnz",
    filters = NULL
  )
  expect_equal(jsonlite::fromJSON(args[[1]]), expected_data)
  expect_equal(args[[2]], "PjnzResponseData")
  expect_equal(args[[3]], "data")
})

test_that("hintr json serializer supports splicing in json objects", {
  serializer <- serializer_json_hintr()
  req <- '{"test":"example request"}'
  res <- MockPlumberResponse$new()
  errorHandler <- function(req, res, e) {
    e$message
  }

  test <- '{"example_json":123}'
  response <- serializer(test, req, res, errorHandler)
  expect_equal(response$header, "Content-Type: application/json")
  expect_match(response$body, '["{\\"example_json\\":123}"]', fixed = TRUE)

  ## Declaring test as a json object
  response <- serializer(json_verbatim(test), req, res, errorHandler)
  expect_equal(response$header, "Content-Type: application/json")
  expect_match(response$body, test, fixed = TRUE)

  ## With a list
  test_list <- list(example_json = scalar(123))
  response <- serializer(test_list, req, res, errorHandler)
  expect_equal(response$header, "Content-Type: application/json")
  expect_match(response$body, test, fixed = TRUE)

  ## With error handling
  ## When trying to convert a value to JSON which throws an error
  response <- serializer(stop("Throw error"), req, res, errorHandler)
  expect_equal(response, "Throw error")
})

test_that("Schemas are draft-04", {
  meta <- readLines("http://json-schema.org/draft-04/schema")
  path <- system.file("schema", package = "hintr", mustWork = TRUE)
  files <- dir(path, full.names = TRUE, pattern = "\\.schema\\.json$")
  for (f in files) {
    expect_true(jsonvalidate::json_validate(f, meta))
  }
})

test_that("Schemas do not use const", {
  path <- system.file("schema", package = "hintr", mustWork = TRUE)
  files <- dir(path, full.names = TRUE, pattern = "\\.schema\\.json$")

  check1 <- function(x) {
    if ("const" %in% names(x)) {
      stop("Schema uses 'const' property and is not supported in draft-04")
    }
    if (is.recursive(x)) {
      lapply(x, check1)
    }
  }

  files <- dir(path, full.names = TRUE, pattern = "\\.schema\\.json$")
  for (f in files) {
    expect_error(check1(jsonlite::fromJSON(f)), NA, label = f)
  }
})

test_that("possible filters are returned for data", {
  programme <- file.path("testdata", "programme.csv")
  shape <- file.path("testdata", "malawi.geojson")
  res <- MockPlumberResponse$new()
  file <- list(path = programme, hash = "12345", filename = "original.pjnz")
  response <- endpoint_validate_survey_programme(
    list(postBody = '{"type":"programme","shape":"path","file": {"path":"path/to/file","hash":"12345","filename":"original"}}'),
    res,
    "programme",
    file,
    shape)
  response <- jsonlite::parse_json(response)

  expect_equal(names(response$data$filters), c("age", "quarter"))
  expect_length(response$data$filters$age, 2)
  expect_equal(response$data$filters$age, list(
    list(
      id = "20",
      name = "15+"
    ),
    list(
      id = "24",
      name = "0-14"
    )
  ))
  expect_length(response$data$filters$quarter, 32)
  expect_equal(response$data$filters$quarter[[1]]$id, "445")
  expect_equal(response$data$filters$quarter[[1]]$name, "Jan-Mar 2011")


  anc <- file.path("testdata", "anc.csv")
  file <- list(path = anc, hash = "12345", filename = "original")
  res <- MockPlumberResponse$new()
  response <- endpoint_validate_survey_programme(
    list(postBody = '{"type":"anc","shape":"path","file": {"path":"path/to/file","hash":"12345","filename":"original"}}'),
    res,
    "anc",
    file,
    shape)
  response <- jsonlite::parse_json(response)

  expect_equal(names(response$data$filters), "quarter")
  expect_length(response$data$filters$quarter, 29)
  expect_equal(response$data$filters$quarter[[1]]$id, "447")
  expect_equal(response$data$filters$quarter[[1]]$name, "Jul-Sep 2011")

  survey <- file.path("testdata", "survey.csv")
  res <- MockPlumberResponse$new()
  file <- list(path = survey, hash = "12345", filename = "original")
  response <- endpoint_validate_survey_programme(
    list(postBody = '{"type":"survey", "shape":"path", "file": {"path":"path/to/file","hash":"12345", "filename":"original"}}'),
    res,
    "survey",
    file,
    shape)
  response <- jsonlite::parse_json(response)

  expect_equal(names(response$data$filters), c("age", "surveys"))
  expect_length(response$data$filters$age, 21)
  expect_length(response$data$filters$surveys, 4)
  expect_equal(response$data$filters$surveys, list(
    list(
      id = "MWI2016PHIA",
      name = "MWI2016PHIA"
    ),
    list(
      id = "MWI2015DHS",
      name = "MWI2015DHS"
    ),
    list(
      id = "MWI2010DHS",
      name = "MWI2010DHS"
    ),
    list(
      id = "MWI2004DHS",
      name = "MWI2004DHS"
    )
  ))
})

test_that("endpoint_plotting_metadata gets metadata", {
  res <- MockPlumberResponse$new()
  response <- endpoint_plotting_metadata(NULL, res, "Malawi")
  response <- jsonlite::parse_json(response)

  expect_equal(res$status, 200)
  expect_true(all(names(response$data) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(response$data$survey), "choropleth")
  expect_equal(names(response$data$anc), "choropleth")
  expect_equal(names(response$data$output), "choropleth")
  expect_equal(names(response$data$programme), "choropleth")
  expect_equal(names(response$data$anc$choropleth$indicators),
               c("art_coverage", "prevalence"))
  expect_equal(response$data$anc$choropleth$indicators$art_coverage$name,
               "ART coverage")
})

test_that("endpoint_plotting_metadata returns useful error", {
  res <- MockPlumberResponse$new()
  response <- endpoint_plotting_metadata(NULL, res, "Missing Country")
  response <- jsonlite::parse_json(response)

  expect_equal(res$status, 400)
  expect_length(response$errors, 1)
  expect_equal(response$errors[[1]]$error, "FAILED_TO_GET_METADATA")
  expect_equal(response$errors[[1]]$detail,
               "Can't retrieve colour scale for country Missing Country. Country not found in configuration.")
})
