api_build <- function() {
  pr <- plumber::plumber$new()
  pr$handle("POST", "/validate", endpoint_validate_pjnz)
  pr$handle("GET", "/", api_root)
  pr
}

api_run <- function(pr) {
  pr$run(host = "0.0.0.0", port = 8888) # nocov
}

api <- function() {
  api_run(api_build()) # nocov
}

endpoint_validate_pjnz <- function(req, pjnz) {
  validate_json_schema(req, "ValidateBaselineRequest")
  res <- with_success(
    do_validate_pjnz(pjnz))
  if (res$success) {
    res$value <- scalar(res$value)
  } else {
    res$errors <- hintr_errors(list("INVALID_PJNZ" = res$message))
  }
  hintr_response(res, "ValidateBaselineResponse")
}

hintr_response <- function(value, schema) {
  if (value$success) {
    status <- "success"
  } else {
    status <- "failure"
  }
  ret <- jsonlite::toJSON(list(
    "status" = scalar(status),
    "errors" = value$errors,
    "data" = value$value
  ))
  validate_json_schema(ret, schema)
  ret
}

hintr_errors <- function(errors) {
  lapply(names(errors), function(x)
    list(error = scalar(x), detail = scalar(errors[[x]])))
}

with_success <- function(expr) {
  tryCatch(
    list(success = TRUE,
         value = force(expr)),
    error = function(e) {
      list(success = FALSE,
           message = e$message,
           error = e,
           type = class(e)[[1]])
    }
  )
}

api_root <- function() {
  jsonlite::unbox("Welcome to hintr")
}
