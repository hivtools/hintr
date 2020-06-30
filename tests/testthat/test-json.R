context("validate-json")

test_that("schema validation can be turned off", {
  withr::with_envvar(list(VALIDATE_JSON_SCHEMAS = "true"), {
    expect_true(validate_schemas())
    expect_true(validate_json_schema('"path/to/file"', "FilePath"))
  })

  withr::with_envvar(list(VALIDATE_JSON_SCHEMAS = ""), {
    expect_false(validate_schemas())
    expect_true(validate_json_schema("data", "schema"))
  })
})

test_that("validate locates schema and does validation with referenced files", {
  test_json <- '{"type": "pjnz", "file": {"path": "path/to/file", "hash": "12345", "filename": "original"}}'
  expect_true(validate(test_json, "ValidateInputRequest"))

  test_json <- '{"type": "notvalid", "path": "path/to/file", "hash": "12345", "filename": "original"}'
  expect_error(validate(test_json, "ValidateInputRequest"))

  test_json <- '{"type": "pjnz"}'
  expect_error(validate(test_json, "ValidateInputRequest"))
})

test_that("to_json converts NA values to null", {
  null <- "null"
  class(null) <- "json"
  expect_equal(to_json(json_verbatim("null")), null)
  df <- data_frame(x = c("one", "two"), y = c(1, NA))
  df_json <- to_json(list("test" = df))
  out <- jsonlite::parse_json(df_json)
  expect_equal(out$test[[2]]$y, NULL)
})
