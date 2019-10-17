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
