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

test_that("to_json unboxes scalars and leaves everything else as arrays", {
  expect_equal(as.character(to_json(list(a = scalar("x"), b = "y", c = 1:2))),
               '{"a":"x","b":["y"],"c":[1,2]}')
  expect_equal(
    as.character(to_json(recursive_scalar(list(a = "x", b = list(c = 1))))),
    '{"a":"x","b":{"c":1.0}}')
  expect_equal(as.character(to_json(list(a = scalar(NA_character_),
                                         b = scalar(NaN), c = scalar(Inf)))),
               '{"a":null,"b":null,"c":null}')
})

test_that("to_json writes verbatim json as is", {
  expect_equal(as.character(to_json(list(a = json_verbatim("null"),
                                         b = json_null(),
                                         c = json_verbatim('{"x":[1,2]}')))),
               '{"a":null,"b":null,"c":{"x":[1,2]}}')
  expect_equal(as.character(to_json(json_verbatim('{"a":"x\\\\y"}'))),
               '{"a":"x\\\\y"}')
})

test_that("to_json writes existing json as is", {
  json <- to_json(list(a = scalar("x")))
  expect_equal(as.character(to_json(json)), '{"a":"x"}')
  expect_equal(as.character(to_json(list(b = json))), '{"b":{"a":"x"}}')
  expect_equal(
    as.character(to_json(list(b = jsonlite::toJSON(list(a = "x"))))),
    '{"b":{"a":["x"]}}')
})

test_that("to_json writes NULL as an empty object like jsonlite", {
  expect_equal(as.character(to_json(NULL)), "{}")
  expect_equal(as.character(to_json(list(a = NULL, b = scalar(1)))),
               '{"a":{},"b":1.0}')
  df <- data.frame(a = 1:2)
  df$m <- list(NULL, c("p", "q"))
  expect_equal(as.character(to_json(df)),
               '[{"a":1,"m":{}},{"a":2,"m":["p","q"]}]')
  expect_equal(as.character(to_json(list(a = list()))), '{"a":[]}')
})

test_that("to_json output is the same as jsonlite", {
  x <- list(
    status = scalar("success"), errors = json_null(),
    data = list(
      data = data.frame(a = c(1.23456789, NA, 3), b = c("x", NA, "z")),
      filters = json_verbatim("null"),
      warnings = list(list(text = scalar("w"),
                           locations = list("model_fit")))),
    version = list(hintr = scalar("1.0.0")))
  expected <- jsonlite::toJSON(x, json_verbatim = TRUE, na = "null")
  expect_equal(jsonlite::fromJSON(to_json(x), simplifyVector = FALSE),
               jsonlite::fromJSON(expected, simplifyVector = FALSE))
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
