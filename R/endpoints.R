api_build <- function(queue) {
  pr <- plumber::plumber$new()
  pr$handle("POST", "/validate/baseline-individual", endpoint_validate_baseline,
            serializer = serializer_json_hintr())
  pr$handle("POST", "/validate/baseline-combined", endpoint_validate_baseline_combined,
            serializer = serializer_json_hintr())
  pr$handle("POST", "/validate/survey-and-programme", endpoint_validate_survey_programme,
            serializer = serializer_json_hintr())
  pr$handle("POST", "/model/options", endpoint_model_options,
            serializer = serializer_json_hintr())
  pr$handle("POST", "/model/submit", endpoint_model_submit(queue),
            serializer = serializer_json_hintr())
  pr$handle("GET", "/model/status/<id>", endpoint_model_status(queue),
            serializer = serializer_json_hintr())
  pr$handle("GET", "/model/result/<id>", endpoint_model_result(queue),
            serializer = serializer_json_hintr())
  pr$handle("GET", "/meta/plotting/<iso3>", endpoint_plotting_metadata,
            serializer = serializer_json_hintr())
  pr$handle("GET", "/download/spectrum/<id>", endpoint_download_spectrum(queue),
            serializer = serializer_zip())
  pr$handle("HEAD", "/download/spectrum/<id>", endpoint_download_spectrum(queue),
            serializer = serializer_zip())
  pr$handle("GET", "/download/summary/<id>", endpoint_download_summary(queue),
            serializer = serializer_zip())
  pr$handle("HEAD", "/download/summary/<id>", endpoint_download_summary(queue),
            serializer = serializer_zip())
  pr$handle("GET", "/hintr/version", endpoint_hintr_version,
            serializer = serializer_json_hintr())
  pr$handle("GET", "/hintr/worker/status", endpoint_hintr_worker_status(queue),
            serializer = serializer_json_hintr())
  pr$handle("POST", "/hintr/stop", endpoint_hintr_stop(queue))
  pr$handle("GET", "/", api_root)

  pr$registerHook("preroute", api_log_start)
  pr$registerHook("postserialize", api_log_end)

  pr
}

api_run <- function(pr, port = 8888) {
  pr$run(host = "0.0.0.0", port = port) # nocov
}

api <- function(port = 8888, queue_id = NULL, workers = 2) {
  queue <- Queue$new(queue_id, workers) # nocov
  api_run(api_build(queue), port) # nocov
}

api_log_start <- function(data, req, res) {
  api_log("%s %s", req$REQUEST_METHOD, req$PATH_INFO)
}

api_log_end <- function(data, req, res, value) {
  if (is.raw(value$body)) {
    size <- length(value$body)
  } else {
    size <- nchar(value$body)
  }
  api_log("`--> %d (%d bytes)", value$status, size)
  value
}

# We can route this via some check for enabling/disabling logging later
api_log <- function(fmt, ...) {
  message(sprintf("[%s] %s", Sys.time(), sprintf(fmt, ...)))
}

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

  hintr_response(response, "ModelRunOptions")
}

endpoint_model_submit <- function(queue) {
  function(req, res, data, options) {
    response <- with_success(
      queue$submit(data, options))
    if (response$success) {
      response$value <- list(id = scalar(response$value))
    } else {
      response$errors <- hintr_errors(list("FAILED_TO_QUEUE" = response$message))
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
    readBin(path, "raw", n = file.size(path))
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
  if (value$success) {
    validate_json_schema(ret, schema, query = "data")
  }
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

endpoint_hintr_version <- function(req, res) {
  packages <- c("hintr", "naomi", "rrq")
  value <- lapply(packages, function(p)
    scalar(as.character(utils::packageVersion(p))))
  names(value) <- packages
  hintr_response(list(success = TRUE, value = value), "HintrVersionResponse")
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

api_root <- function() {
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

serializer_zip <- function() {
  function(val, req, res, errorHandler) {
    tryCatch({
      res$setHeader("Content-Type", "application/octet-stream")
      res$body <- val
      return(res$toResponse())
    }, error = function(e) {
      errorHandler(req, res, e)
    })
  }
}
