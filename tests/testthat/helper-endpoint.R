## Always validate schemas in tests
Sys.setenv("VALIDATE_JSON_SCHEMAS" = "true")

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

MockPlumberResponse <- R6::R6Class("PlumberResponse", list(
  body = NULL,
  status = 200,
  header = NULL,
  setHeader = function(...) {
    self$header <- paste(..., sep = ": ")
  },
  toResponse = function() {
    list(header = self$header, body = self$body)
  }
))
