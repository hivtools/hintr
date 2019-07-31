api <- function() {
  pr <- plumber$new()
  pr$handle("POST", "/validate", handle_validate)
  pr
}

handle_validate <- function(req, pjnz, shape, population, survey, programme,
                            anc) {
  ## TODO: Validate req json against the schema
  valid <- validate_inputs(pjnz$path, shape$path, population$path, survey$path,
                           programme$path, anc$path)
  if (valid) {
    response <- prepare_success_response(pjnz$path)
  } else {
    response <- prepare_fail_response()
  }
  ## TODO: Validate response against schema
  response
}

prepare_success_response <- function(pjnz) {
  country <- read_country(pjnz)
  list(
    "status" = "success",
    "errors" = list(),
    "data" = list(
      "pjnz" = list(
        "country" = country
      ),
      "shape" = list(),
      "population" = list(),
      "survey" = list(),
      "programme" = list(),
      "anc" = list()
    )
  )
}

prepare_fail_response <- function() {
  list(
    "status" = "failure",
    "errors" = list(
      "pjnz" = c("EXAMPLE_FAILURE")
    )
  )
}
