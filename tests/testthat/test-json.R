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
  test_json <- '{"type": "pjnz", "file": {"path": "path/to/file", "hash": "12345", "filename": "original", "fromADR": false}}'
  expect_true(validate(test_json, "ValidateInputRequest"))

  test_json <- '{"type": "notvalid", "path": "path/to/file", "hash": "12345", "filename": "original", "fromADR": false}'
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

test_that("can recursively convert to scalar", {
  expect_equal(recursive_scalar(2), list(scalar(2)))
  expect_equal(recursive_scalar(c(1, 2)), list(scalar(1), scalar(2)))
  expect_equal(recursive_scalar(list(x = "foo",
                                     y = "bar")),
               list(x = scalar("foo"),
                    y = scalar("bar")))
  expect_equal(recursive_scalar(list(x = "foo",
                                     y = list(y1 = "bar1", y2 = "bar2"))),
               list(x = scalar("foo"),
                    y = list(y1 = scalar("bar1"), y2 = scalar("bar2"))))
  expect_null(recursive_scalar(NULL))
  expect_equal(recursive_scalar(list(x = "foo", y = NULL)),
               list(x = scalar("foo"), y = NULL))
  expect_equal(recursive_scalar(list(x = 1)),
               list(x = scalar(1)))
})
