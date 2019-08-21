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
  status = 200
))

test_redis_available <- function() {
  available <- redux::redis_available()
  if (!available) {
    testthat::skip(
"Skipping test as redis is not available, start redis in docker using
docker run --rm -d --network=host --name hintr_redis redis
to enable test.")
  }
  invisible(available)
}
