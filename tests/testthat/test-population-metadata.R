test_that("population_metadata returns population metadata", {
  population <- setup_payload_poulation_metadata("testdata")
  pop <- population_metadata(population)

  expect_population_metadata(pop)
})

test_that("population_metadata returns error if issue reading file", {
  population <- '{
    "population": "missing.csv"
  }'
  expect_error(population_metadata(population),
               "Failed to read file.")
})

test_that("input population endpoint works as expected", {
  payload <- setup_payload_poulation_metadata("testdata")

  endpoint <- endpoint_input_population_plot()
  res <- endpoint$run(payload)

  expect_equal(res$status_code, 200)
  expect_null(res$error)
  expect_population_metadata(res$data)
})

test_that("input population endpoint works as expected via API", {
  test_redis_available()

  queue <- test_queue(workers = 0)
  api <- api_build(queue, validate = TRUE)
  payload <- setup_payload_poulation_metadata("testdata")
  res <- api$request("POST", "/chart-data/input-population", body = payload)

  expect_equal(res$status, 200)
  body <- jsonlite::fromJSON(res$body, simplifyVector = FALSE)
  expect_equal(body$status, "success")
  expect_null(body$errors)
  expect_population_metadata(body$data)
})
