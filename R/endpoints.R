api_build <- function() {
  pr <- plumber::plumber$new()
  pr$handle("POST", "/validate", endpoint_validate_input,
            serializer = serializer_json_hintr())
  pr$handle("POST", "/validate_baseline", endpoint_validate_baseline,
            serializer = serializer_json_hintr())
  pr$handle("GET", "/", api_root)
  pr
}

api_run <- function(pr, port = 8888) {
  pr$run(host = "0.0.0.0", port = port) # nocov
}

api <- function(port = 8888) {
  api_run(api_build(), port) # nocov
}

#' Validate an input file and return an indication of success and
#' if successful return the data required by UI.
#'
#' @param req The request as PlumberRequest object.
#' @param res The response as a PlumberResponse object.
#' @param type The type of file to validate: pjnz, shape, population, ANC,
#' survey or programme.
#' @param path Path to the file to validate.
#'
#' @return Validated JSON response with data and incidcation of success.
#' @keywords internal
endpoint_validate_input <- function(req, res, type, path) {
  validate_json_schema(req$postBody, "ValidateInputRequest")
  validate_func <- switch(type,
    pjnz = do_validate_pjnz,
    shape = do_validate_shape,
    population = do_validate_population,
    programme = do_validate_programme,
    anc = do_validate_anc,
    survey = do_validate_survey)
  validate_if_exists <- function(path) {
    if (!file.exists(path)) {
      stop("File does not exist. Create it, or fix the path.", call. = FALSE)
    }
    validate_func(path)
  }
  response <- with_success(
    validate_if_exists(path))
  if (response$success) {
    response$value <- input_response(response$value, path, type)

    # Add placeholder filters
    if (type=="shape") {
      response$value$filters <- list()
    }
    if (type=="programme" || type=="anc") {
      response$value$filters <- list(age=list())
    }
    if (type=="survey") {
      response$value$filters <- list(age=list(), surveys=list())
    }

  } else {
    response$errors <- hintr_errors(list("INVALID_FILE" = response$message))
    res$status <- 400
  }

  hintr_response(response, "ValidateInputResponse")
}


input_response <- function(data, path, type) {
  ret <- list(filename = scalar(basename(path)),
              type = scalar(type),
              data = data)
  validate_json_schema(to_json(ret), get_input_response_schema(type), "data")
  ret
}

#' Validate the baseline data as a collection.
#'
#' Checks whether the collection of baseline data (pjnz, shape and population)
#' are consistent.
#'
#' @param req The request as PlumberRequest object.
#' @param res The response as a PlumberResponse object.
#' @param pjnz Path to the pjnz file.
#' @param shape Path to the shape file.
#' @param population Path to the population file.
#'
#' @return Validated JSON response with data and incidcation of success.
#' @keywords internal
endpoint_validate_baseline <- function(req, res, pjnz, shape, population) {
  validate_json_schema(req$postBody, "ValidateBaselineRequest")
  response <- with_success(validate_baseline(pjnz, shape, population))
  if (!response$success) {
    response$errors <- hintr_errors(list("INVALID_BASELINE" = response$message))
    res$status <- 400
  }
  hintr_response(response, "ValidateBaselineResponse")
}

#' Format a hintr response and validate against schema.
#'
#' Returns the status, any errors occured and the data if successful.
#'
#' @param value List containing an indication of success, any errors and the
#' value to return.
#'
#' @return Formatted hintr response.
#' @keywords internal
hintr_response <- function(value, schema) {
  if (value$success) {
    status <- "success"
  } else {
    status <- "failure"
  }
  if (is.null(value$errors)) {
    errors <- list()
  }
  else {
    errors = value$errors
  }
  ret <- to_json(list(
    status = scalar(status),
    errors = errors,
    data = value$value))
  validate_json_schema(ret, "Response")
  if (value$success) {
    validate_json_schema(ret, schema, query = "data")
  }
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
  scalar("Welcome to hintr")
}

# This serialiser allows us to splice in objects with a class "json"
# into list structures, without converting these structures into
# strings.  So if we have
#
#   list(a = scalar("foo"), b = json_verbatim('{"x": 1, "y": 2}'))
#
# We'll end up with the json string
#
#   {"a": "foo", "b": {"x": 1, "y": 2}}
#
# This is a suitable drop-in replacement for all responses as it is
# otherwise compatible with the default 'json' serialiser.
serializer_json_hintr <- function() {
  function(val, req, res, errorHandler) {
    tryCatch({
      res$setHeader("Content-Type", "application/json")
      res$body <- to_json(val)
      return(res$toResponse())
    }, error = function(e) {
      errorHandler(req, res, e)
    })
  }
}

json_verbatim <- function(x) {
  class(x) <- "json"
  x
}

to_json <- function(x) {
  jsonlite::toJSON(x, json_verbatim = TRUE)
}
