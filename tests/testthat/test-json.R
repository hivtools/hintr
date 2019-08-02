context("validate-json")

test_that("schema can be validated", {
  withr::with_envvar(list(VALIDATE_JSON_SCHEMAS = "true"), {
    expect_true(validate_schemas())
    expect_true(validate_json_schema("data", "schema"))
  })

  withr::with_envvar(list(VALIDATE_JSON_SCHEMAS = ""), {
    expect_false(validate_schemas())
    expect_true(validate_json_schema("data", "schema"))
  })
})
