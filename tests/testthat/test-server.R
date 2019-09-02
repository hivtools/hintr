context("server")

test_that("Root", {
  test_redis_available()
  server <- hintr_server()

  r <- httr::GET(server$url)
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_to_json(r), "Welcome to hintr")
})

test_that("validate pjnz", {
  test_redis_available()
  server <- hintr_server()

  payload <- file.path("payload", "validate_pjnz_payload.json")
  r <- httr::POST(paste0(server$url, "/validate"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_to_json(r),
               list(status = "success",
                    errors = structure(list(), names = character(0)),
                    data = list(filename = "Botswana2018.PJNZ",
                                type = "pjnz",
                                data = list(country = "Botswana"))))
})

test_that("validate shape", {
  test_redis_available()
  server <- hintr_server()

  payload <- file.path("payload", "validate_shape_payload.json")
  r <- httr::POST(paste0(server$url, "/validate"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_to_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, structure(list(), names = character(0)))
  expect_equal(response$data$filename, "malawi.geojson")
  expect_equal(response$data$type, "shape")
  expect_equal(names(response$data$data), c("type", "features"))
  expect_equal(response$data$data$type, "FeatureCollection")
})

test_that("validate population", {
  test_redis_available()
  server <- hintr_server()

  payload <- file.path("payload", "validate_population_payload.json")
  r <- httr::POST(paste0(server$url, "/validate"),
                  body = httr::upload_file(payload),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_to_json(r),
               list(status = "success",
                    errors = structure(list(), names = character(0)),
                    data = list(filename = "population.csv",
                                type = "population",
                                data = NULL)))
})

test_that("model interactions", {
  test_redis_available()
  server <- hintr_server()

  ## Submit a model run
  submit <- file.path("payload", "model_submit_payload.json")
  r <- httr::POST(paste0(server$url, "/model/submit"),
                  body = httr::upload_file(submit),
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_to_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, structure(list(), names = character(0)))
  expect_equal(names(response$data), c("id"))

  ## Get the status
  Sys.sleep(1)
  r <- httr::GET(paste0(server$url, "/model/status/", response$data$id))
  expect_equal(httr::status_code(r), 200)
  response <- response_to_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, structure(list(), names = character(0)))
  expect_equal(response$data$done, TRUE)
  expect_equal(response$data$status, "COMPLETE")
  expect_equal(response$data$success, TRUE)
  expect_equal(response$data$queue, 0)
  expect_true("id" %in% names(response$data))

  ## Get the result
  r <- httr::GET(paste0(server$url, "/model/result/", response$data$id))
  expect_equal(httr::status_code(r), 200)
  response <- response_to_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, structure(list(), names = character(0)))
  expect_equal(response$data, 2)
})
