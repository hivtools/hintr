api <- function() {
  pr <- plumber::plumber$new()
  pr$handle("POST", "/validate", endpoint_validate_baseline)
  pr
}

endpoint_validate_baseline <- function(req, pjnz, shape, population) {
  validate_json_schema(req, "ValidateBaselineRequest")
  res <- with_success(
    do_validate_baseline(pjnz, shape, population))
  if (res$success) {
    res$value <- scalar(res$value)
  } else {
    ## I'm expecting that we need to provide a way for do_validate_baseline to
    ## return some classification of the 'type' of error as well as the message
    ## Which then need to be put into the correct structure by this bit of code
    res$errors <- list(error = "INVALID_BASELINE",
                       message = res$error)
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

with_success <- function(expr) {
  tryCatch(
    list(success = TRUE,
         value = force(expr)),
    error = function(e) {
      list(success = FALSE,
           error = e$message)
    }
  )
}
