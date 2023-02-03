test_that("validate errors", {
  path <- system_file("schema/Response.schema.json")
  v <- jsonvalidate::json_validator(path, "ajv")

  mock_id <- mockery::mock(scalar("fake_key"), cycle = TRUE)
  f <- function(message, error, ...) {
    with_mock(new_error_id = mock_id, {
      tryCatch(
        hintr_error(message, error, ...),
        error = function(e) {
          porcelain:::porcelain_process_error(e)
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

  e2 <- f("msg", "ERROR", job_id = scalar("123"))
  expect_equal(e2$value$errors, list(
    list(
      error = scalar("ERROR"),
      detail = scalar("msg"),
      key = scalar("fake_key"),
      job_id = scalar("123")
    )
  ))
  expect_true(v(e2$body))
})
