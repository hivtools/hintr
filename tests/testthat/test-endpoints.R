context("endpoints")

test_that("hintr_response correctly prepares response", {
  value <- list(
    success = TRUE,
    value = list(
      filename = scalar("original.pjnz"),
      hash = scalar("12345"),
      type = scalar("pjnz"),
      data = list(country = scalar("Botswana"),
                  iso3 = scalar("BWA"))
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
  expect_equal(response$data$data$iso3, "BWA")
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
        country = scalar("Botswana"),
        iso3 = scalar("BWA"))
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
  expect_s3_class(endpoint_root(), "scalar")
})

test_that("plumber api can be built", {
  api <- api_build(NULL)
  expect_s3_class(api, "plumber")
  expect_length(api$routes, 6)
  expect_equal(names(api$routes),
               c("validate", "model", "meta", "download", "hintr", ""))
  expect_equal(names(api$routes$validate),
               c("baseline-individual", "baseline-combined",
                 "survey-and-programme", "options"))
  expect_equal(names(api$routes$model),
               c("options", "submit", "status", "result", "cancel", "debug"))
  expect_equal(names(api$routes$meta), "plotting")
})

test_that("format_response_data correctly formats data and validates it", {
  mock_validate <- mockery::mock(TRUE)
  file <- list(path = "path", hash = "12345", filename = "original.pjnz")
  with_mock("hintr:::validate_json_schema" = mock_validate, {
    response <- input_response(list(data = list(country = scalar("Botswana")),
                                    filters = json_verbatim("null")),
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
  expect_equal(names(response$headers), "Content-Type")
  expect_equal(response$headers$`Content-Type`, "application/json")
  expect_match(response$body, '["{\\"example_json\\":123}"]', fixed = TRUE)

  ## Declaring test as a json object
  res <- MockPlumberResponse$new()
  response <- serializer(json_verbatim(test), req, res, errorHandler)
  expect_equal(names(response$headers), "Content-Type")
  expect_equal(response$headers$`Content-Type`, "application/json")
  expect_match(response$body, test, fixed = TRUE)

  ## With a list
  res <- MockPlumberResponse$new()
  test_list <- list(example_json = scalar(123))
  response <- serializer(test_list, req, res, errorHandler)
  expect_equal(names(response$headers), "Content-Type")
  expect_equal(response$headers$`Content-Type`, "application/json")
  expect_match(response$body, test, fixed = TRUE)

  ## With error handling
  ## When trying to convert a value to JSON which throws an error
  response <- serializer(stop("Throw error"), req, res, errorHandler)
  expect_equal(response, "Throw error")
})

test_that("serializer_zip sets headers and streams bytes", {
  req <- '{"test":"example request"}'
  res <- MockPlumberResponse$new()
  errorHandler <- function(req, res, e) {
    e$message
  }

  serializer <- serializer_zip("test")

  output <- serializer(list(bytes = "value",
                            id = "1234567",
                            metadata = list(areas = "MWI")),
                       req, res,
                       errorHandler)
  expect_equal(output$body, "value")
  expect_equal(names(output$headers), c("Content-Type", "Content-Disposition"))
  expect_equal(output$headers$`Content-Type`, "application/octet-stream")
  expect_match(output$headers$`Content-Disposition`,
               'attachment; filename="MWI_\\d+-\\d+_test.zip"')
})

test_that("serializer_zip passes errors along as json", {
  req <- '{"test":"example request"}'
  res <- MockPlumberResponse$new()
  errorHandler <- function(req, res, e) {
    e$message
  }

  serializer <- serializer_zip("test")
  res$status <- 400
  value <- list(status = scalar("failure"),
                errors = hintr_errors(list(ERROR = "message")))
  output <- serializer(value, req, res, errorHandler)
  expect_equal(output$headers$`Content-Type`, "application/json")
  response <- jsonlite::parse_json(output$body)
  expect_equal(response$status, "failure")
  expect_equal(response$errors[[1]]$error, "ERROR")
  expect_equal(response$errors[[1]]$detail, "message")
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

test_that("schemas always contain a type", {
  ## This added to fix bugs caused by missing types in schema marking query
  ## json as valid. See mrc-652 for example.
  path <- system.file("schema", package = "hintr", mustWork = TRUE)

  has_properties <- function(x) {
    "properties" %in% names(x)
  }

  has_items <- function(x) {
    "items" %in% names(x)
  }

  has_definitions <- function(x) {
    "definitions" %in% names(x)
  }

  is_valid <- function(x) {
    ## For schema to be valid it must either be:
    ## * A reference - $ref
    ## * A combined schema - oneOf, allOf, anyOf
    ## * An enum - enum
    ## * or declare a type - type
    required_names <- c("type", "$ref", "oneOf", "allOf", "anyOf", "enum")
    any(required_names %in% names(x))
  }

  check_types <- function(schema, path) {
    if (!is_valid(schema)) {
      stop(sprintf("Missing type, reference or enum for node in schema %s", path))
    }
    if (has_properties(schema)) {
      lapply(schema$properties, function(x) check_types(x, path))
    }
    if (has_items(schema)) {
      check_types(schema$items, path)
    }
    if (has_definitions(schema)) {
      lapply(schema$definitions, function(x) check_types(x, path))
    }
  }

  files <- dir(path, full.names = TRUE, pattern = "\\.schema\\.json$")
  for (f in files) {
    expect_error(check_types(jsonlite::fromJSON(f), f), NA, label = f)
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

  expect_equal(names(response$data$filters), c("age", "year", "indicators"))
  expect_length(response$data$filters$age, 2)
  expect_equal(response$data$filters$age, list(
    list(
      id = "15+",
      label = "15+"
    ),
    list(
      id = "00-14",
      label = "0-14"
    )
  ))
  expect_length(response$data$filters$year, 8)
  expect_equal(response$data$filters$year[[1]]$id, "2018")
  expect_equal(response$data$filters$year[[1]]$label, "2018")

  expect_length(response$data$filters$indicators, 1)
  expect_equal(response$data$filters$indicators[[1]]$id, "current_art")
  expect_equal(response$data$filters$indicators[[1]]$label, "ART number (attending)")


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

  expect_equal(names(response$data$filters), c("year", "indicators"))
  expect_length(response$data$filters$year, 8)
  expect_equal(response$data$filters$year[[1]]$id, "2018")
  expect_equal(response$data$filters$year[[1]]$label, "2018")

  expect_length(response$data$filters$indicators, 2)
  expect_equal(response$data$filters$indicators[[1]]$id, "prevalence")
  expect_equal(response$data$filters$indicators[[1]]$label, "HIV prevalence")
  expect_equal(response$data$filters$indicators[[2]]$id, "art_coverage")
  expect_equal(response$data$filters$indicators[[2]]$label, "ART coverage")

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

  expect_equal(names(response$data$filters), c("age", "surveys", "indicators"))
  expect_length(response$data$filters$age, 21)
  expect_length(response$data$filters$surveys, 4)
  expect_equal(response$data$filters$surveys, list(
    list(
      id = "MWI2016PHIA",
      label = "MWI2016PHIA"
    ),
    list(
      id = "MWI2015DHS",
      label = "MWI2015DHS"
    ),
    list(
      id = "MWI2010DHS",
      label = "MWI2010DHS"
    ),
    list(
      id = "MWI2004DHS",
      label = "MWI2004DHS"
    )
  ))

  expect_length(response$data$filters$indicators, 4)
  expect_equal(response$data$filters$indicators[[1]]$id, "prevalence")
  expect_equal(response$data$filters$indicators[[1]]$label, "HIV prevalence")
  expect_equal(response$data$filters$indicators[[2]]$id, "art_coverage")
  expect_equal(response$data$filters$indicators[[2]]$label, "ART coverage")
  expect_equal(response$data$filters$indicators[[3]]$id, "recent")
  expect_equal(response$data$filters$indicators[[3]]$label,
               "Proportion recently infected")
  expect_equal(response$data$filters$indicators[[4]]$id, "vls")
  expect_equal(response$data$filters$indicators[[4]]$label,
               "Viral load suppression")
})

test_that("endpoint_plotting_metadata gets metadata", {
  res <- MockPlumberResponse$new()
  response <- endpoint_plotting_metadata(NULL, res, "MWI")
  response <- jsonlite::parse_json(response)

  expect_equal(res$status, 200)
  expect_true(all(names(response$data) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(response$data$survey), "choropleth")
  expect_equal(names(response$data$anc), "choropleth")
  expect_equal(names(response$data$output), c("barchart", "choropleth"))
  expect_equal(names(response$data$programme), "choropleth")
  expect_length(response$data$anc$choropleth$indicators, 2)
  expect_equal(response$data$anc$choropleth$indicators[[1]]$indicator,
               "prevalence")
  expect_equal(response$data$anc$choropleth$indicators[[2]]$indicator,
               "art_coverage")
  expect_equal(response$data$anc$choropleth$indicators[[1]]$name,
               "HIV prevalence")
  expect_equal(response$data$anc$choropleth$indicators[[2]]$name,
               "ART coverage")
})

test_that("endpoint_plotting_metadata returns default data for missing country", {
  res <- MockPlumberResponse$new()
  metadata <- testthat::evaluate_promise(
    endpoint_plotting_metadata(NULL, res, "Missing Country"))
  expect_equal(metadata$messages,
    "Country with iso3 code Missing Country not in metadata - returning default colour scales.\n")
  response <- jsonlite::parse_json(metadata$result)

  expect_equal(res$status, 200)
  expect_true(all(names(response$data) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(response$data$survey), "choropleth")
  expect_equal(names(response$data$anc), "choropleth")
  expect_equal(names(response$data$output), c("barchart", "choropleth"))
  expect_equal(names(response$data$programme), "choropleth")
  expect_length(response$data$anc$choropleth$indicators, 2)
  expect_equal(response$data$anc$choropleth$indicators[[1]]$indicator,
               "prevalence")
  expect_equal(response$data$anc$choropleth$indicators[[2]]$indicator,
               "art_coverage")
  expect_equal(response$data$anc$choropleth$indicators[[1]]$name,
               "HIV prevalence")
  expect_equal(response$data$anc$choropleth$indicators[[2]]$name,
               "ART coverage")
})

test_that("can convert model status to scalar", {
  progress <- list(
    started = list(
      started = TRUE,
      complete = FALSE,
      name = "Started mock model"
    ),
    complete = list(
      started = FALSE,
      complete = FALSE,
      name = "Finished mock model"
    )
  )
  model_status <- list(
    done = FALSE,
    status = "RUNNING",
    success = TRUE,
    queue = 0,
    progress = progress
  )
  response <- prepare_status_response(model_status, "id")
  expected_response <- list(
    done = scalar(FALSE),
    status = scalar("RUNNING"),
    success = scalar(TRUE),
    queue = scalar(0),
    progress = list(
      list(
        started = scalar(TRUE),
        complete = scalar(FALSE),
        name = scalar("Started mock model")
      ),
      list(
        started = scalar(FALSE),
        complete = scalar(FALSE),
        name = scalar("Finished mock model")
      )
    ),
    id = scalar("id")
  )
  expect_equal(response, expected_response)
})

test_that("404 handler", {
  res <- MockPlumberResponse$new()
  req <- list(REQUEST_METHOD = "POST",
              PATH_INFO = "/my/path")
  ans <- hintr_404_handler(req, res)
  expect_is(ans, "list") # not json
  expect_equal(ans$status, scalar("failure"))
  expect_is(ans$errors[[1]]$key, "scalar")
  expect_match(ans$errors[[1]]$key, "^[a-z]{5}-[a-z]{5}-[a-z]{5}$")
  # key is randomly generated - remove to compare rest
  ans$errors[[1]]$key <- NULL
  expect_equal(ans$errors,
               list(list(
                 error = scalar("NOT_FOUND"),
                 detail = scalar("POST /my/path is not a valid hintr path"))))
  expect_identical(res$status, 404L)
})

test_that("error handler", {
  err <- simpleCondition("some error", quote(f(x)))
  res <- MockPlumberResponse$new()
  req <- list(REQUEST_METHOD = "POST",
              PATH_INFO = "/my/path")
  ans <- hintr_error_handler(req, res, err)
  expect_is(ans, "PlumberResponse")
  expect_is(ans$body, "json")
  validate_json_schema(ans$body, "Response")
  dat <- jsonlite::fromJSON(ans$body, simplifyVector = FALSE)
  expect_equal(dat$status, "failure")
  detail <- paste("Unexpected server error in 'f(x)' :",
                  "'some error' while doing 'POST /my/path'")
  expect_is(dat$errors[[1]]$key, "character")
  expect_match(dat$errors[[1]]$key, "^[a-z]{5}-[a-z]{5}-[a-z]{5}$")
  # key is randomly generated - remove to compare rest
  dat$errors[[1]]$key <- NULL
  expect_equal(dat$errors,
               list(list(error = "SERVER_ERROR", detail = detail)))
  expect_identical(res$status, 500L)
})

test_that("error handler with no call", {
  err <- simpleCondition("some error", NULL)
  res <- MockPlumberResponse$new()
  req <- list(REQUEST_METHOD = "POST",
              PATH_INFO = "/my/path")
  ans <- hintr_error_handler(req, res, err)
  expect_is(ans, "PlumberResponse")
  expect_is(ans$body, "json")
  validate_json_schema(ans$body, "Response")
  dat <- jsonlite::fromJSON(ans$body, simplifyVector = FALSE)
  expect_equal(dat$status, "failure")
  expect_is(dat$errors[[1]]$key, "character")
  expect_match(dat$errors[[1]]$key, "^[a-z]{5}-[a-z]{5}-[a-z]{5}$")
  # key is randomly generated - remove to compare rest
  dat$errors[[1]]$key <- NULL
  detail <- paste("Unexpected server error in '<call missing>' :",
                  "'some error' while doing 'POST /my/path'")
  expect_equal(dat$errors,
               list(list(error = "SERVER_ERROR", detail = detail)))
  expect_identical(res$status, 500L)
})
