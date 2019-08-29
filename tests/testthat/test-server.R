context("server")

test_that("Root", {
  server <- hintr_server()

  r <- httr::GET(server$url)
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_to_json(r), "Welcome to hintr")
})

test_that("validate pjnz", {
  server <- hintr_server()

  pjnz <- file.path("testdata", "Botswana2018.PJNZ")
  body <- list(type = scalar("pjnz"), path = scalar(pjnz))

  r <- httr::POST(paste0(server$url, "/validate"), body = body,
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
  server <- hintr_server()

  shape <- file.path("testdata", "malawi.geojson")
  body <- list(type = scalar("shape"), path = scalar(shape))

  r <- httr::POST(paste0(server$url, "/validate"), body = body,
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
  server <- hintr_server()

  population <- file.path("testdata", "population.csv")
  body <- list(type = scalar("population"), path = scalar(population))

  r <- httr::POST(paste0(server$url, "/validate"), body = body,
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  expect_equal(response_to_json(r),
               list(status = "success",
                    errors = structure(list(), names = character(0)),
                    data = list(filename = "population.csv",
                                type = "population",
                                data = NULL)))
})

test_that("validate programme", {
  server <- hintr_server()

  programme <- file.path("testdata", "programme.csv")
  body <- list(type = scalar("programme"), path = scalar(programme))

  r <- httr::POST(paste0(server$url, "/validate"), body = body,
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_to_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, structure(list(), names = character(0)))
  expect_equal(response$data$filename, "programme.csv")
  expect_equal(response$data$type, "programme")
  expect_true(length(response$data$data) >= 1400)
  expect_equal(typeof(response$data$data[[1]]$value), "integer")
})

test_that("validate ANC", {
  server <- hintr_server()

  anc <- file.path("testdata", "anc.csv")
  body <- list(type = scalar("anc"), path = scalar(anc))

  r <- httr::POST(paste0(server$url, "/validate"), body = body,
                  encode = "json")
  expect_equal(httr::status_code(r), 200)
  response <- response_to_json(r)
  expect_equal(response$status, "success")
  expect_equal(response$errors, structure(list(), names = character(0)))
  expect_equal(response$data$filename, "anc.csv")
  expect_equal(response$data$type, "anc")
  expect_true(length(response$data$data) >= 800)
  expect_equal(typeof(response$data$data[[1]]$value), "double")
})

