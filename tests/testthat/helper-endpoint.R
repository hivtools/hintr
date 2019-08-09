build_validate_request <- function(pjnz) {
  list(
    "pjnz" = list(
      "path" = pjnz
    ),
    "shape" = list(
      "path" = "path/to/file"
    ),
    "population" = list(
      "path" = "path/to/file"
    ),
    "survey" = list(
      "path" = "path/to/file"
    ),
    "programme" = list(
      "path" = "path/to/file"
    ),
    "anc" = list(
      "path" = "path/to/file"
    )
  )
}

validate_test_that <- function(desc, code) {
  withr::with_envvar(list(VALIDATE_JSON_SCHEMAS = "true"), {
    testthat::test_that(desc, code)
  })
}
