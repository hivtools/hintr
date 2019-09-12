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
  r <- httr::POST(paste0(server$url, "/validate/input"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_from_json(r),
               list(status = "success",
                    errors = list(),
                    data = list(filename = "Botswana2018.PJNZ",
                                type = "pjnz",
                                data = list(country = "Botswana"))))
})

test_that("validate shape", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_shape_payload.json")
  r <- httr::POST(paste0(server$url, "/validate/input"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)

  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$filename, "malawi.geojson")
  expect_equal(response$data$type, "shape")
  expect_equal(names(response$data$data), c("type", "features"))
  expect_equal(response$data$data$type, "FeatureCollection")
})

test_that("validate population", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_population_payload.json")
  r <- httr::POST(paste0(server$url, "/validate/input"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_from_json(r),
               list(status = "success",
                    errors = list(),
                    data = list(filename = "population.csv",
                                type = "population",
                                data = NULL)))
})

test_that("validate programme", {
  server <- hintr_server()

  programme <- file.path("testdata", "programme.csv")
  body <- list(type = scalar("programme"), path = scalar(programme))

  r <- httr::POST(paste0(server$url, "/validate/input"), body = body,
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$filename, "programme.csv")
  expect_equal(response$data$type, "programme")
  expect_true(length(response$data$data) >= 1400)
  expect_equal(typeof(response$data$data[[1]]$value), "integer")
})

test_that("validate ANC", {
  server <- hintr_server()

  anc <- file.path("testdata", "anc.csv")
  body <- list(type = scalar("anc"), path = scalar(anc))

  r <- httr::POST(paste0(server$url, "/validate/input"), body = body,
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$filename, "anc.csv")
  expect_equal(response$data$type, "anc")
  expect_true(length(response$data$data) >= 800)
  expect_equal(typeof(response$data$data[[1]]$value), "double")
})

test_that("validate survey", {
  server <- hintr_server()

  survey <- file.path("testdata", "survey.csv")
  body <- list(type = scalar("survey"), path = scalar(survey))

  r <- httr::POST(paste0(server$url, "/validate/input"), body = body,
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, list())
  expect_equal(response$data$filename, "survey.csv")
  expect_equal(response$data$type, "survey")
  expect_true(length(response$data$data) >= 30000)
  expect_equal(typeof(response$data$data[[1]]$value), "double")
})

test_that("validate baseline", {
  server <- hintr_server()

  payload <- file.path("payload", "validate_baseline_payload.json")
  r <- httr::POST(paste0(server$url, "/validate/baseline"),
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
  Sys.sleep(1)
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
  expect_equal(response$data, 2)
})

