context("server")

test_that("Root", {
  server <- hintr_server()

  r <- httr::GET(server$url)
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_from_json(r), "Welcome to hintr")
})

test_that("validate pjnz", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_pjnz_payload.json")
  r <- httr::POST(paste0(server$url, "/validate/baseline-individual"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  expect_equal(
    response_from_json(r),
    list(status = "success",
         errors = list(),
         data = list(hash = "12345",
                     type = "pjnz",
                     data = list(country = "Botswana"),
                     filename = "original.PJNZ",
                     filters = NULL)))
})

test_that("validate shape", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_shape_payload.json")
  r <- httr::POST(paste0(server$url, "/validate/baseline-individual"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)

  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.geojson")
  expect_equal(response$data$type, "shape")
  expect_true(all(c("type", "features") %in% names(response$data$data)))
  expect_equal(response$data$data$type, "FeatureCollection")
  expect_equal(names(response$data$filters), c("regions", "level_labels"))
})

test_that("validate population", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_population_payload.json")
  r <- httr::POST(paste0(server$url, "/validate/baseline-individual"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_from_json(r),
               list(status = "success",
                    errors = list(),
                    data = list(hash = "12345",
                                type = "population",
                                data = NULL,
                                filename = "original.csv",
                                filters = NULL)))
})

test_that("validate programme", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_programme_payload.json")
  r <- httr::POST(
    paste0(server$url, "/validate/survey-and-programme"),
    body = httr::upload_file(payload),
    encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.csv")
  expect_equal(response$data$type, "programme")
  expect_true(length(response$data$data) >= 1400)
  expect_equal(typeof(response$data$data[[1]]$current_art), "double")
  expect_equal(names(response$data$filters), "age")
  expect_length(response$data$filters$age, 2)
})

test_that("validate ANC", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_anc_payload.json")
  r <- httr::POST(
    paste0(server$url, "/validate/survey-and-programme"),
    body = httr::upload_file(payload),
    encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.csv")
  expect_equal(response$data$type, "anc")
  expect_true(length(response$data$data) >= 800)
  expect_equal(typeof(response$data$data[[1]]$ancrt_hiv_status), "integer")
  expect_equal(names(response$data$filters), "age")
  expect_length(response$data$filters$age, 1)
})

test_that("validate survey", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_survey_payload.json")
  r <- httr::POST(
    paste0(server$url, "/validate/survey-and-programme"),
    body = httr::upload_file(payload),
    encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$hash, "12345")
  expect_equal(response$data$filename, "original.csv")
  expect_equal(response$data$type, "survey")
  expect_true(length(response$data$data) >= 20000)
  expect_equal(typeof(response$data$data[[1]]$est), "double")
  expect_equal(names(response$data$filters), c("age", "surveys"))
  expect_length(response$data$filters$age, 21)
  expect_length(response$data$filters$surveys, 4)
})

test_that("validate baseline", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_baseline_payload.json")
  r <- httr::POST(paste0(server$url, "/validate/baseline-combined"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$complete, TRUE)
  expect_equal(response$data$consistent, TRUE)
})

test_that("model interactions", {
  server <- hintr_server()

  ## Submit a model run
  submit <- file.path("payload", "model_submit_payload.json")
  r <- httr::POST(paste0(server$url, "/model/submit"),
                  body = httr::upload_file(submit),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(names(response$data), c("id"))

  ## Get the status
  Sys.sleep(2)
  r <- httr::GET(paste0(server$url, "/model/status/", response$data$id))
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$done, TRUE)
  expect_equal(response$data$status, "COMPLETE")
  expect_equal(response$data$success, TRUE)
  expect_equal(response$data$queue, 0)
  expect_true("id" %in% names(response$data))

  ## Get the result
  r <- httr::GET(paste0(server$url, "/model/result/", response$data$id))
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(httr::status_code(r), 200)
  expect_equal(names(response$data), c("data", "filters"))
  expect_equal(names(response$data$data[[1]]),
               c("area_id", "sex", "age_group_id", "quarter_id", "indicator_id",
                 "mode", "mean", "lower", "upper"))
  expect_length(response$data$data, 42021)
  expect_equal(names(response$data$filters), c("age", "quarter", "indicator"))
  expect_length(response$data$filters$age, 29)
  expect_length(response$data$filters$quarter, 1)
  expect_equal(response$data$filters$quarter[[1]]$name, "Jan-Mar 2016")
  expect_length(response$data$filters$indicator, 7)
})

