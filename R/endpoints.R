api_build <- function(path = tempfile(), workers = 2) {
  model_queue_start(tempfile(), workers)
  pr <- plumber::plumber$new()
  pr$handle("POST", "/validate", endpoint_validate_input,
            serializer = plumber::serializer_content_type("application/json"))
  pr$handle("POST", "/run_model", endpoint_run_model,
            serializer = plumber::serializer_content_type("application/json"))
  pr$handle("POST", "/run_status", endpoint_run_status,
            serializer = plumber::serializer_content_type("application/json"))
  pr$handle("GET", "/", api_root)
  pr
}

api_run <- function(pr) {
  pr$run(host = "0.0.0.0", port = 8888) # nocov
}

api <- function() {
  api_run(api_build()) # nocov
}

#' Validate an input file and return an indication of success and
#' if successful return the data required by UI.
#'
#' @param req The request as JSON.
#' @param type The type of file to validate: pjnz, shape, population, ANC,
#' survey or programme.
#' @param path Path to the file to validate.
#'
#' @return Validates JSON response with data and incidcation of success.
#' @keywords internal
endpoint_validate_input <- function(req, res, type, path) {
  validate_json_schema(req$postBody, "ValidateInputRequest")
  validate_func <- switch(type,
    pjnz = do_validate_pjnz)
  response <- with_success(
    validate_func(path))
  if (response$success) {
    response$value <- list(country = scalar(response$value))
  } else {
    response$errors <- hintr_errors(list("INVALID_FILE" = response$message))
    res$status <- 400
  }
  hintr_response(response)
}

endpoint_run_model <- function(req, res, inputs, options) {
  validate_json_schema(req, "InitialiseModelRunRequest")
  response <- with_success(
    model_queue_submit(inputs, options))
  if (response$success) {
    response$value <- list(job_id = scalar(response$value))
  } else {
    response$errors <- hintr_errors(list("FAILED_TO_QUEUE" = response$message))
    res$status <- 400
  }
  hintr_response(response)
}

endpoint_run_status <- function(req, res, job_id) {
  validate_json_schema(req, "ModelRunStatusRequest")
  response <- with_success(
    model_run_status(job_id))
  if (response$success) {
    response$value <- list(job_id = scalar(response$value))
  } else {
    response$errors <- hintr_errors(list("FAILED_TO_QUEUE" = response$message))
    res$status <- 400
  }
  hintr_response(response)
}

#' Format a hintr response.
#'
#' Returns the status, any errors occured and the data if successful.
#'
#' @param value List containing an indication of success, any errors and the
#' value to return.
#'
#' @return Formatted hintr response.
#' @keywords internal
hintr_response <- function(value) {
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
  validate_json_schema(ret, "Response")
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
