context("errors")

test_that("validate errors", {
  path <- system.file("schema/Response.schema.json", mustWork = TRUE,
                      package = "hintr")
  v <- jsonvalidate::json_validator(path, "ajv")

  mock_id <- mockery::mock(scalar("fake_key"), cycle = TRUE)
  f <- function(message, error, ...) {
    with_mock("ids::proquint" = mock_id, {
      tryCatch(
        hintr_error(message, error, ...),
        error = function(e) {
          pkgapi:::pkgapi_process_error(e)
        }
      )
    })
  }

  e1 <- f("msg", "ERROR")
  expect_equal(e1$value$errors, list(
    list(
      error = scalar("ERROR"),
      detail = scalar("msg"),
      key = scalar("fake_key")
    )
  ))
  expect_true(v(e1$body))

  e2 <- f("msg", "ERROR", trace = c(scalar("test"), scalar("trace")))
  expect_equal(e2$value$errors, list(
    list(
      error = scalar("ERROR"),
      detail = scalar("msg"),
      key = scalar("fake_key"),
      trace = c(scalar("test"), scalar("trace"))
    )
  ))
  expect_true(v(e2$body))
})
