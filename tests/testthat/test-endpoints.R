test_that("plumber api can be built", {
  api <- api_build(NULL)
  expect_s3_class(api, "Plumber")
  expect_length(api$routes, 11)
  expect_setequal(names(api$routes),
                 c("", "validate", "model", "calibrate", "comparison", "meta",
                   "download", "hintr", "chart-data", "rehydrate", "internal"))
  expect_setequal(names(api$routes$validate),
                  c("baseline-individual", "baseline-combined",
                    "survey-and-programme", "options"))
  expect_setequal(names(api$routes$model),
                  c("options", "submit", "status", "result", "cancel", "debug"))
  expect_setequal(names(api$routes$calibrate),
                  c("options", "submit", "status", "result", "plot"))
  expect_setequal(names(api$routes$meta), c("plotting", "adr"))
  expect_equal(names(api$routes$`chart-data`), "input-time-series")
  expect_setequal(names(api$routes$rehydrate), c("submit", "status", "result"))
  expect_equal(names(api$routes$comparison), "plot")
  expect_setequal(names(api$routes$internal), c("upload", "prerun"))
  expect_setequal(names(api$routes$internal), c("upload", "prerun"))
  expect_setequal(names(api$routes$internal$upload), c("input", "result"))
})

test_that("input_response correctly formats data and validates it", {
  mock_validate <- mockery::mock(TRUE)
  file <- list(path = "path", hash = "12345", filename = "original.pjnz", fromADR = FALSE,
               resource_url = "https://adr.unaids.org/file/123.pjnz")
  with_mock(validate_json_schema = mock_validate, {
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
    fromADR = FALSE,
    resource_url = "https://adr.unaids.org/file/123.pjnz",
    filters = NULL
  )
  expect_equal(jsonlite::fromJSON(args[[1]]), expected_data)
  expect_equal(args[[2]], "PjnzResponseData")
  expect_equal(args[[3]], "data")
})

test_that("Schemas are draft-04", {
  meta <- readLines("http://json-schema.org/draft-04/schema")
  path <- system_file("schema")
  files <- dir(path, full.names = TRUE, pattern = "\\.schema\\.json$")
  for (f in files) {
    expect_true(jsonvalidate::json_validate(f, meta))
  }
})

test_that("Schemas do not use const", {
  path <- system_file("schema")

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
  path <- system_file("schema")

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

test_that("endpoint_plotting_metadata gets metadata", {
  response <- plotting_metadata("MWI")

  expect_true(all(names(response) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(response$survey), "choropleth")
  expect_equal(names(response$anc), "choropleth")
  expect_equal(names(response$output), c("barchart", "choropleth"))
  expect_equal(names(response$programme), "choropleth")
  expect_length(response$anc$choropleth$indicators, 2)
  expect_equal(response$anc$choropleth$indicators[[1]]$indicator,
               scalar("anc_prevalence"))
  expect_equal(response$anc$choropleth$indicators[[2]]$indicator,
               scalar("anc_art_coverage"))
  expect_equal(response$anc$choropleth$indicators[[1]]$name,
               scalar("ANC HIV prevalence"))
  expect_equal(response$anc$choropleth$indicators[[2]]$name,
               scalar("ANC prior ART coverage"))
})

test_that("endpoint_plotting_metadata returns default data for missing country", {
  response <- plotting_metadata("Missing Country")

  expect_true(all(names(response) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(response$survey), "choropleth")
  expect_equal(names(response$anc), "choropleth")
  expect_equal(names(response$output), c("barchart", "choropleth"))
  expect_equal(names(response$programme), "choropleth")
  expect_length(response$anc$choropleth$indicators, 2)
  expect_equal(response$anc$choropleth$indicators[[1]]$indicator,
               scalar("anc_prevalence"))
  expect_equal(response$anc$choropleth$indicators[[2]]$indicator,
               scalar("anc_art_coverage"))
  expect_equal(response$anc$choropleth$indicators[[1]]$name,
               scalar("ANC HIV prevalence"))
  expect_equal(response$anc$choropleth$indicators[[2]]$name,
               scalar("ANC prior ART coverage"))
})

test_that("endpoint_plotting_metadata returns default data for NULL country", {
  response <- plotting_metadata()

  expect_true(all(names(response) %in%
                    c("survey", "anc", "output", "programme")))
  expect_equal(names(response$survey), "choropleth")
  expect_equal(names(response$anc), "choropleth")
  expect_equal(names(response$output), c("barchart", "choropleth"))
  expect_equal(names(response$programme), "choropleth")
  expect_length(response$anc$choropleth$indicators, 2)
  expect_equal(response$anc$choropleth$indicators[[1]]$indicator,
               scalar("anc_prevalence"))
  expect_equal(response$anc$choropleth$indicators[[2]]$indicator,
               scalar("anc_art_coverage"))
  expect_equal(response$anc$choropleth$indicators[[1]]$name,
               scalar("ANC HIV prevalence"))
  expect_equal(response$anc$choropleth$indicators[[2]]$name,
               scalar("ANC prior ART coverage"))
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
  expect_type(ans, "list") # not json
  expect_equal(ans$status, scalar("failure"))
  expect_s3_class(ans$errors[[1]]$key, "scalar")
  expect_match(ans$errors[[1]]$key, "^[a-z]{5}-[a-z]{5}-[a-z]{5}$")
  # key is randomly generated - remove to compare rest
  ans$errors[[1]]$key <- NULL
  expect_equal(ans$errors,
               list(list(
                 error = scalar("NOT_FOUND"),
                 detail = scalar("POST /my/path is not a valid hintr path"))))
  expect_identical(res$status, 404L)
})
