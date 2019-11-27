#' Get function to generate model options from Naomi template and input files
#'
#' This is used to build the model run options UI in the front end.
#'
#' @param req The request object.
#' @param res The response object.
#' @param shape The shape file.
#' @param survey The survey file.
#' @param programme The optional programme ART file.
#' @param anc The optional programme ANC file.
#'
#' @return Function to generate model options from input data.
#' @keywords internal
endpoint_model_options <- function(req, res, shape, survey, programme =  NULL, anc = NULL) {
  response <- with_success({
    ## Shape and survey must exist
    assert_file_exists(shape$path)
    assert_file_exists(survey$path)
    do_endpoint_model_options(shape, survey, programme, anc)
  })
  if (response$success) {
    response$value <- json_verbatim(response$value)
  } else {
    response$errors <- hintr_errors(list(
      "INVALID_OPTIONS" = response$message))
    res$status <- 400
  }

  hintr_response(response, "ModelRunOptions", include_version = TRUE)
}

#' Validate that set of model run options is okay to submit for a model run
#'
#' @param req The request object
#' @param res The response object
#' @param data The set of input data for the model run
#' @param options key-value list of selected options
#'
#' @return Response with data valid if true and errors if not valid
#' @keywords internal
endpoint_model_options_validate <- function(req, res, data, options) {
  response <- with_success({
    do_validate_model_options(data, options)
  })
  if (!response$success) {
    response$errors <- hintr_errors(list(
      "INVALID_OPTIONS" = response$message
    ))
    res$status <- 400
  }
  hintr_response(response, "ModelOptionsValidate")
}

endpoint_model_submit <- function(queue) {
  function(req, res, data, options, version) {
    model_submit <- function() {
      if (!is_current_version(version)) {
        stop("Trying to run model with old version of options. Update model run options")
      }
      queue$submit(data, options)
    }
    response <- with_success(model_submit())
    if (response$success) {
      response$value <- list(id = scalar(response$value))
    } else {
      if (!is_current_version(version)) {
        errors <- list("VERSION_OUT_OF_DATE" = response$message)
      } else {
        errors <- list("FAILED_TO_QUEUE" = response$message)
      }
      response$errors <- hintr_errors(errors)
      res$status <- 400
    }
    hintr_response(response, "ModelSubmitResponse")
  }
}

endpoint_model_status <- function(queue) {
  function(req, res, id) {
    response <- with_success(
      queue$status(id))
    if (response$success) {
      response$value <- prepare_status_response(response$value, id)
    } else {
      response$errors <- hintr_errors(
        list("FAILED_TO_RETRIEVE_STATUS" = response$message))
      res$status <- 400
    }
    hintr_response(response, "ModelStatusResponse")
  }
}

endpoint_model_result <- function(queue) {
  function(req, res, id) {
    response <- with_success(queue$result(id))
    if (is_error(response$value)) {
      response$success <- FALSE
      response$errors <- hintr_errors(
        list("MODEL_RUN_FAILED" = scalar(response$value$message))
      )
      response$value <- NULL
      res$status <- 400
    } else if (!response$success) {
      response$errors <- hintr_errors(
        list("FAILED_TO_RETRIEVE_RESULT" = response$message))
      res$status <- 400
    } else {
      response$value <- process_result(response$value)
    }
    hintr_response(response, "ModelResultResponse")
  }
}

is_error <- function(x) {
  inherits(x, "error")
}

#' Validate an baseline input file and return an indication of success and
#' if successful return the data required by UI.
#'
#' @param req The request as PlumberRequest object.
#' @param res The response as a PlumberResponse object.
#' @param type The type of file to validate: pjnz, shape, population, ANC,
#' survey or programme.
#' @param file File object containing path, filename and md5 hash.
#'
#' @return Validated JSON response with data and incidcation of success.
#' @keywords internal
endpoint_validate_baseline <- function(req, res, type, file) {
  validate_json_schema(req$postBody, "ValidateInputRequest")
  validate_func <- switch(type,
                          pjnz = do_validate_pjnz,
                          shape = do_validate_shape,
                          population = do_validate_population)
  response <- with_success({
    assert_file_exists(file$path)
    validate_func(file)
  })
  if (response$success) {
    response$value <- input_response(response$value, type, file)
  } else {
    response$errors <- hintr_errors(list("INVALID_FILE" = response$message))
    res$status <- 400
  }

  hintr_response(response, "ValidateInputResponse")
}

#' Validate survey and programme data including consistency with shape file
#' returning an indication of success and if successful return the data
#' required by UI.
#'
#' @param req The request as PlumberRequest object.
#' @param res The response as a PlumberResponse object.
#' @param type The type of file to validate: ANC, survey or programme.
#' @param path Path to the file to validate.
#' @param file File object containing path, filename and md5 hash.
#' @param shape Path to shape file for comparison.
#'
#' @return Validated JSON response with data and incidcation of success.
#' @keywords internal
endpoint_validate_survey_programme <- function(req, res, type, file, shape) {
  validate_json_schema(req$postBody, "ValidateSurveyAndProgrammeRequest")
  validate_func <- switch(type,
                          programme = do_validate_programme,
                          anc = do_validate_anc,
                          survey = do_validate_survey)
  # mrc-663
  shape <- file_object(shape)
  response <- with_success({
    assert_file_exists(file$path)
    assert_file_exists(shape$path)
    validate_func(file, shape)
  })
  if (response$success) {
    response$value <- input_response(response$value, type, file)
  } else {
    response$errors <- hintr_errors(list("INVALID_FILE" = response$message))
    res$status <- 400
  }

  hintr_response(response, "ValidateInputResponse")
}


input_response <- function(value, type, file) {
  ret <- list(hash = scalar(file$hash),
              type = scalar(type),
              data = value$data,
              filename = scalar(file$filename),
              filters = value$filters)
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
endpoint_validate_baseline_combined <- function(req, res, pjnz, shape, population) {
  validate_json_schema(req$postBody, "ValidateBaselineRequest")
  # TODO (mrc-663): we should require file objects here.
  if (!is.null(pjnz)) {
    pjnz <- file_object(pjnz)
  }
  if (!is.null(shape)) {
    shape <- file_object(shape)
  }
  if (!is.null(population)) {
    population <- file_object(population)
  }
  response <- with_success(do_validate_baseline(pjnz, shape, population))
  if (!response$success) {
    response$errors <- hintr_errors(list("INVALID_BASELINE" = response$message))
    res$status <- 400
  }
  hintr_response(response, "ValidateBaselineResponse")
}

#' Download spectrum digest file.
#'
#' Returns a function which returns bytes of zip.
#'
#' @param queue The queue used to retrieve download from task id.
#'
#' @return Bytes of zip file.
#' @keywords internal
endpoint_download_spectrum <- function(queue) {
  download(queue, "spectrum")
}

#' Download summary zip file.
#'
#' Returns a function which returns bytes of zip.
#'
#' @param queue The queue used to retrieve download from task id.
#'
#' @return Bytes of zip file.
#' @keywords internal
endpoint_download_summary <- function(queue) {
  download(queue, "summary")
}

download <- function(queue, type) {
  function(req, res, id) {
    response <- with_success(queue$result(id))
    if (is_error(response$value)) {
      response$success <- FALSE
      response$errors <- hintr_errors(
        list("MODEL_RUN_FAILED" = scalar(response$value$message))
      )
      response$value <- NULL
      res$status <- 400
      return(hintr_response(response, "ModelResultResponse"))
    } else if (!response$success) {
      response$errors <- hintr_errors(
        list("FAILED_TO_RETRIEVE_RESULT" = response$message))
      res$status <- 400
      return(hintr_response(response, "ModelResultResponse"))
    }

    path <- switch(type,
                   "spectrum" = response$value$spectrum_path,
                   "summary" = response$value$summary_path)
    out <- list(
      bytes = readBin(path, "raw", n = file.size(path)),
      id = id
    )
    out
  }
}

endpoint_plotting_metadata <- function(req, res, iso3) {
  response <- with_success(do_plotting_metadata(iso3))
  if (!response$success) {
    response$errors <- hintr_errors(
      list("FAILED_TO_GET_METADATA" = response$message))
    res$status <- 400
  }
  hintr_response(response, "PlottingMetadataResponse")
}

#' Format a hintr response and validate against schema.
#'
#' Returns the status, any errors occured and the data if successful.
#'
#' @param value List containing an indication of success, any errors and the
#' value to return.
#' @param schema The name of data subschema to validate response against.
#' @param include_version If TRUE the package version information is included
#' in the response.
#' @param as_json Logical, indicating if the response should be
#'   converted into a json string
#'
#' @return Formatted hintr response.
#' @keywords internal
hintr_response <- function(value, schema, include_version = FALSE,
                           as_json = TRUE) {
  if (value$success) {
    status <- "success"
  } else {
    status <- "failure"
  }
  if (is.null(value$errors)) {
    errors <- list()
  } else {
    errors = value$errors
  }
  response <- list(
    status = scalar(status),
    errors = errors,
    data = value$value)
  if (include_version) {
    response$version <- cfg$version_info
  }
  ret <- to_json(response)
  if (value$success) {
    validate_json_schema(ret, schema, query = "data")
  }
  validate_json_schema(ret, "Response")
  if (as_json) ret else response
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

endpoint_hintr_version <- function(req, res) {
  hintr_response(list(success = TRUE, value = cfg$version_info),
                 "HintrVersionResponse")
}

endpoint_hintr_worker_status <- function(queue) {
  function(req, res) {
    response <- with_success(lapply(queue$queue$worker_status(), scalar))
    hintr_response(response, "HintrWorkerStatus")
  }
}

endpoint_hintr_stop <- function(queue) {
  force(queue)
  function(req, res) {
    message("Stopping workers")
    queue$queue$worker_stop()
    message("Quitting hintr")
    quit(save = "no")
  }
}

endpoint_root <- function() {
  scalar("Welcome to hintr")
}

prepare_status_response <- function(value, id) {
  set_scalar <- function(x) {
    if (length(names(x)) > 1) {
      lapply(x, set_scalar)
    } else {
      scalar(x)
    }
  }

  response_value <- lapply(value[names(value) != "progress"], scalar)
  response_value$progress <- lapply(value$progress, set_scalar)
  response_value$id <- scalar(id)
  response_value
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

serializer_zip <- function(filename) {
  function(val, req, res, errorHandler) {
    tryCatch({
      res$setHeader("Content-Type", "application/octet-stream")
      short_id <- substr(val$id, 1, 5)
      res$setHeader("Content-Disposition",
                    sprintf('attachment; filename="%s_%s.zip"',
                            filename, short_id))
      res$body <- val$bytes
      return(res$toResponse())
    }, error = function(e) {
      errorHandler(req, res, e)
    })
  }
}

hintr_404_handler <- function(req, res) {
  res$status <- 404L
  detail <- sprintf("%s %s is not a valid hintr path",
                    req$REQUEST_METHOD, req$PATH_INFO)
  errors <- hintr_errors(list("NOT_FOUND" = detail))
  value <- list(success = FALSE,
                errors = errors)
  # We get no control over how plumber will serialise this - so we
  # can't return json or it gets wrapped as if it was a json string
  # (we ordinarily get around this by using json_verbatim = TRUE).  So
  # here we return the object that will be passed into
  # jsonlite::toJSON (all scalars being appropriately treated).
  hintr_response(value, as_json = FALSE)
}

# It's not possible to get the traceback at this point
hintr_error_handler <- function(req, res, error) {
  res$status <- 500L
  if (is.null(error$call)) {
    call <- "<call missing>"
  } else {
    call <- paste(deparse(error$call), collapse = " ")
  }
  detail <- sprintf(
    "Unexpected server error in '%s' : '%s' while doing '%s %s'",
    call, error$message, req$REQUEST_METHOD, req$PATH_INFO)
  api_log("ERROR: %s", detail)
  errors <- hintr_errors(list("SERVER_ERROR" = detail))
  # This seems at odds with the default handler present in the plumber
  # package, and it also seems entirely undocumented.
  value <- list(success = FALSE,
                errors = errors)
  res$body <- hintr_response(value, as_json = TRUE)
  # Setting a header with setHeader should work, but here messes it up
  res$headers[["Content-Type"]] <- "application/json"
  res
}
