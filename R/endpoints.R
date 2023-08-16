input_response <- function(value, type, file) {
  ret <- list(hash = scalar(file$hash),
              type = scalar(type),
              data = value$data,
              filename = scalar(file$filename),
              fromADR = scalar(file$fromADR),
              resource_url = scalar(file$resource_url),
              filters = value$filters)
  if (!is.null(value$warnings)) {
    ret$warnings <- warnings_scalar(value$warnings)
  }
  validate_json_schema(to_json(ret), get_input_response_schema(type), "data")
  ret
}

model_options <- function(input) {
  input <- jsonlite::fromJSON(input)
  tryCatch({
    assert_file_exists(input$shape$path)
    assert_file_exists(input$survey$path)
    json_verbatim(
      do_endpoint_model_options(input$shape, input$survey,
                                input$programme, input$anc))
  }, error = function(e) {
    ## Get the cache for no reason as this seems to prevent flaky test failure
    ## due to caching, see https://buildkite.com/mrc-ide/hintr/builds/1619
    cache <- get_cache(NULL)
    hintr_error(e$message, "INVALID_OPTIONS")
  })
}

calibration_options <- function(iso3) {
  tryCatch({
    json_verbatim(
      get_controls_json("calibration", iso3, NULL, NULL))
  }, error = function(e) {
    hintr_error(e$message, "INVALID_CALIBRATION_OPTIONS")
  })
}

get_controls_json <- function(...) {
  naomi.options::get_controls_json(...)
}

root_endpoint <- function() {
  scalar(t_("WELCOME"))
}

validate_baseline <- function(input) {
  input <- jsonlite::fromJSON(input)
  validate_func <- switch(input$type,
                          pjnz = do_validate_pjnz,
                          shape = do_validate_shape,
                          population = do_validate_population)
  tryCatch({
    assert_file_exists(input$file$path)
    ## This does some validation of the data part of the response
    ## Is that right to do at this point or does porcelain have a way to validate
    ## subsets of the data?
    input_response(validate_func(input$file), input$type, input$file)
  },
  error = function(e) {
    hintr_error(e$message, "INVALID_FILE")
  })
}

validate_baseline_combined <- function(input) {
  input <- jsonlite::fromJSON(input)
  as_file_object <- function(x) {
    if (!is.null(x)) {
      file_object(x)
    } else {
      NULL
    }
  }
  tryCatch({
    do_validate_baseline(as_file_object(input$pjnz),
                         as_file_object(input$shape),
                         as_file_object(input$population))
  },
  error = function(e) {
    hintr_error(e$message, "INVALID_BASELINE")
  })
}

validate_survey_programme <- function(input, strict = TRUE) {
  input <- jsonlite::fromJSON(input)
  validate_func <- switch(input$type,
                          programme = do_validate_programme,
                          anc = do_validate_anc,
                          survey = do_validate_survey)
  tryCatch({
    shape <- file_object(input$shape)
    assert_file_exists(input$file$path)
    assert_file_exists(shape$path)
    input_response(
      validate_func(input$file, shape, strict), input$type, input$file)
  },
  error = function(e) {
    hintr_error(e$message, "INVALID_FILE")
  })
}

input_time_series <- function(type, input) {
  input <- jsonlite::fromJSON(input)
  get_time_series_data <- switch(
    type,
    programme = get_programme_time_series,
    anc = get_anc_time_series,
    hintr_error(t_("INVALID_TIME_SERIES_INPUT_TYPE",
                   list(types = paste(c("programme", "anc"), collapse = " or "),
                        type = type)),
                "INVALID_INPUT_TYPE"))
  tryCatch({
    assert_file_exists(input$data$shape$path)
    if (type == "anc") {
      file <- input$data$anc
    } else {
      file <- input$data$programme
    }
    assert_file_exists(file$path)
    get_time_series_data(file, input$data$shape)
  },
  error = function(e) {
    hintr_error(e$message, "FAILED_TO_GENERATE_TIME_SERIES")
  })
}

model_options_validate <- function(input) {
  input <- jsonlite::fromJSON(input)
  tryCatch({
    ## Update some labels to match what naomi requires
    ## TODO: Some of this is shared between model running and here so we
    ## should use use common code when we merge this back into hintr.
    ## This endpoint currently isn't called see mrc-592.
    data <- input$data
    data$art_number <- data$programme
    data$programme <- NULL
    data$anc_testing <- data$anc
    data$anc <- NULL
    data <- naomi:::format_data_input(data)
    valid <- validate_model_options(data, input$options)
    valid$valid <- scalar(valid$valid)
    valid$warnings <- warnings_scalar(valid$warnings)
    valid
  }, error = function(e) {
    hintr_error(e$message, "INVALID_OPTIONS")
  })
}

validate_model_options <- function(...) {
  naomi::validate_model_options(...)
}

submit_model <- function(queue) {
  function(input) {
    input <- jsonlite::fromJSON(input)
    if (!is_current_version(input$version)) {
      hintr_error(t_("MODEL_SUBMIT_OLD"), "VERSION_OUT_OF_DATE")
    }
    tryCatch(
      list(id = scalar(queue$submit_model_run(input$data, input$options))),
      error = function(e) {
        hintr_error(e$message, "FAILED_TO_QUEUE")
      }
    )
  }
}

queue_status <- function(queue) {
  check_orphan <- throttle(queue$queue$worker_detect_exited, 10)
  function(id) {
    no_error(check_orphan())
    tryCatch({
      out <- queue$status(id)
      prepare_status_response(out, id)
    },
    error = function(e) {
      hintr_error(e$message, "FAILED_TO_RETRIEVE_STATUS")
    })
  }
}

model_result <- function(queue) {
  function(id) {
    verify_result_available(queue, id)
    result <- queue$result(id)
    warnings <- list()
    if (!is.null(result$warnings)) {
      warnings <- warnings_scalar(result$warnings)
    }
    list(id = scalar(id),
         complete = scalar(TRUE),
         warnings = warnings)
  }
}

submit_calibrate <- function(queue) {
  function(id, input) {
    verify_result_available(queue, id)
    calibration_options <- jsonlite::fromJSON(input)
    if (!is_current_version(calibration_options$version)) {
      hintr_error(t_("CALIBRATE_SUBMIT_OLD"), "VERSION_OUT_OF_DATE")
    }
    tryCatch(
      list(id = scalar(queue$submit_calibrate(queue$result(id),
                                              calibration_options$options))),
      error = function(e) {
        hintr_error(e$message, "FAILED_TO_QUEUE")
      }
    )
  }
}

calibrate_result <- function(queue) {
  function(id) {
    verify_result_available(queue, id)
    process_result(queue$result(id))
  }
}

calibrate_metadata <- function(queue) {
  function(id) {
    verify_result_available(queue, id)
    result <- queue$result(id)
    output <- naomi::read_hintr_output(result$plot_data_path)
    metadata <- build_output_metadata(output)
    warnings <- list()
    if (!is.null(result$warnings)) {
      warnings <- warnings_scalar(result$warnings)
    }
    list(
      plottingMetadata = metadata,
      warnings = warnings
    )
  }
}

calibrate_data <- function(queue) {
  function(id) {
    verify_result_available(queue, id)
    result <- queue$result(id)
    output <- naomi::read_hintr_output(result$plot_data_path)
    list(data = select_data(output))
  }
}

calibrate_plot <- function(queue) {
  function(id) {
    verify_result_available(queue, id)
    data <- naomi::hintr_calibrate_plot(queue$result(id))
    ## Strip tibble class to work with helper functions which rely on
    ## converting to vector when selecting 1 column
    data <- as.data.frame(data)
    data[is.nan(data$mean), "mean"] <- 0
    is_ratio <- grepl("\\w+_ratio", data$data_type)
    data$indicator[is_ratio] <- paste0(data$indicator[is_ratio], "_ratio")
    data$spectrum_region_code <- as.character(data$spectrum_region_code)
    filters <- get_calibrate_plot_output_filters(data)
    list(
      data = data,
      plottingMetadata = list(
        barchart = list(
          indicators = get_barchart_metadata(data, "calibrate"),
          filters = filters,
          defaults = get_calibrate_barchart_defaults(filters)
        )
      )
    )
  }
}

comparison_plot <- function(queue) {
  function(id) {
    verify_result_available(queue, id)
    data <- naomi::hintr_comparison_plot(queue$result(id))
    ## Strip tibble class to work with helper functions which rely on
    ## converting to vector when selecting 1 column
    data <- as.data.frame(data)
    data <- data[, c("area_id", "area_name", "age_group", "sex",
                   "calendar_quarter", "indicator", "source", "mean",
                   "lower", "upper")]
    filters <- get_comparison_plot_filters(data)
    selections <- get_comparison_barchart_selections(data, filters)
    list(
      data = data,
      plottingMetadata = list(
        barchart = list(
          indicators = get_barchart_metadata(data, "comparison"),
          filters = filters,
          defaults = selections[[1]],
          selections = selections
        )
      )
    )
  }
}

verify_result_available <- function(queue, id, version = NULL) {
  task_status <- queue$queue$task_status(id)
  if (task_status == "COMPLETE") {
    result <- queue$result(id)
    naomi:::assert_model_output_version(result, version = version)
  } else if (task_status == "ERROR") {
    result <- queue$result(id)
    hintr_error(result$message, "MODEL_RUN_FAILED", job_id = scalar(id))
  } else if (task_status == "DIED") {
    hintr_error(t_("MODEL_RESULT_CRASH"), "MODEL_RUN_FAILED")
  } else if (task_status == "CANCELLED") {
    hintr_error(t_("MODEL_RUN_CANCELLED"), "MODEL_RUN_FAILED")
  } else { # ~= MISSING, PENDING, RUNNING
    hintr_error(t_("MODEL_RESULT_MISSING"), "FAILED_TO_RETRIEVE_RESULT")
  }
}

model_cancel <- function(queue) {
  function(id) {
    tryCatch({
      queue$cancel(id)
      json_null()
    },
    error = function(e) {
      hintr_error(e$message, "FAILED_TO_CANCEL")
    })
  }
}

plotting_metadata <- function(iso3 = NULL) {
  tryCatch(
    do_plotting_metadata(iso3),
    error = function(e) {
      hintr_error(e$message, "FAILED_TO_GET_METADATA")
    }
  )
}

download_submit <- function(queue) {
  function(id, type, input = NULL) {
    ## API path should be - separated but we
    ## use _ for names in naomi
    type <- gsub("-", "_", type, fixed = TRUE)
    version <- NULL
    if (type == "comparison") {
      version <- "2.7.16"
    }
    verify_result_available(queue, id, version)
    notes <- NULL
    state <- NULL
    if (!is.null(input)) {
      parsed_input <- jsonlite::fromJSON(input, simplifyVector = FALSE)
      if (!is.null(parsed_input$notes)) {
        notes <- format_notes(parsed_input$notes)
      }
      if (!is.null(parsed_input$state)) {
        ## Keep this as raw JSON because we want to write it straight out to
        ## the output zip and this way we can avoid unboxing problems
        ## from deserializing and reserializing the data
        state <- V8::v8()$call("(d) => JSON.stringify(d.state)",
                               V8::JS(paste0(input, collapse = "\n")))
      }
    }
    tryCatch(
      list(id = scalar(
        queue$submit_download(queue$result(id), type, notes, state))),
      error = function(e) {
        hintr_error(e$message, "FAILED_TO_QUEUE")
      }
    )
  }
}

download_result <- function(queue) {
  function(id) {
    tryCatch({
      res <- queue$result(id)
      if (is_error(res) || is.null(res$path)) {
        msg <- res$message
        if (is.null(msg)) {
          msg <- t_("FAILED_ADR_METADATA")
        }
        hintr_error(msg, "OUTPUT_GENERATION_FAILED")
      }
      filename <- switch(res$metadata$type,
                         spectrum = "naomi-output",
                         coarse_output = "coarse-output",
                         summary = "summary-report",
                         comparison = "comparison-report")
      ext <- switch(res$metadata$type,
                    spectrum = ".zip",
                    coarse_output = ".zip",
                    summary = ".html",
                    comparison = ".html")
      bytes <- readBin(res$path, "raw", n = file.size(res$path))
      bytes <- porcelain::porcelain_add_headers(bytes, list(
        "Content-Disposition" = build_content_disp_header(res$metadata$areas,
                                                          filename, ext),
        "Content-Length" = length(bytes)))
      bytes
    },
    error = function(e) {
      if (is_porcelain_error(e)) {
        stop(e)
      } else {
        hintr_error(e$message, "FAILED_TO_RETRIEVE_RESULT")
      }
    })
  }
}

build_content_disp_header <- function(areas, filename, ext) {
  sprintf('attachment; filename="%s"',
          paste0(paste(c(areas, filename, iso_time_str()), collapse = "_"),
                 ext))
}

download_model_debug <- function(queue) {
  function(id) {
    tryCatch({
      data <- queue$queue$task_data(id)
      func <- as.list(data$expr)[[1]]
      if (func == "hintr:::run_model") {
        files <- unique(unlist(lapply(data$objects$data, function(x) {
          if (!is.null(x)) {
            x$path
          }
        }), FALSE, FALSE))
        data$objects$data <- lapply(data$objects$data, function(x) {
          if (!is.null(x)) {
            list(path = basename(x$path), hash = x$hash, filename = x$filename)
          }
        })
      } else {
        ## Calibrate, file download requests have format of "hintr_output"
        ## object from naomi. They have files at plot_data_path
        ## and model_output_path
        files <- data$objects$model_output$model_output_path
        data$objects$model_output$model_output_path <- basename(
          data$objects$model_output$model_output_path)
        if (!is.null(data$objects$model_output$plot_data_path)) {
          ## This file only exists after calibrate has been run
          files <- c(files, data$objects$model_output$plot_data_path)
          data$objects$model_output$plot_data_path <- basename(
            data$objects$model_output$plot_data_path)
        }
      }
      tmp <- tempfile()
      path <- file.path(tmp, id)
      dir.create(path, FALSE, TRUE)

      data$sessionInfo <- utils::sessionInfo()

      path_files <- file.path(path, "files")
      dir.create(path_files)
      file_copy(files, file.path(path_files, basename(files)))
      saveRDS(data, file.path(path, "data.rds"))

      on.exit(unlink(tmp, recursive = TRUE))

      dest <- paste0(id, ".zip")
      withr::with_dir(tmp, zip::zipr(dest, id))

      path <- file.path(tmp, dest)
      bytes <- readBin(path, "raw", n = file.size(path))
      bytes <- porcelain::porcelain_add_headers(bytes, list(
        "Content-Disposition" =
          sprintf('attachment; filename="%s_%s_naomi_debug.zip"',
                  id, iso_time_str())))
      bytes
    },
    error = function(e) {
      if (is_porcelain_error(e)) {
        stop(e)
      } else {
        hintr_error(e$message, "INVALID_TASK")
      }
    })
  }
}

worker_status <- function(queue) {
  function() {
    lapply(queue$queue$worker_status(), scalar)
  }
}

hintr_stop <- function(queue) {
  force(queue)
  function() {
    message("Stopping workers")
    queue$queue$worker_stop()
    message("Quitting hintr")
    quit(save = "no")
  }
}

prepare_status_response <- function(value, id) {
  set_scalar <- function(x) {
    if (length(names(x)) > 1) {
      lapply(x, set_scalar)
    } else {
      scalar(x)
    }
  }

  json_or_scalar <- function(x) {
    if (inherits(x, "json")) {
      x
    } else {
      scalar(x)
    }
  }

  response_value <- lapply(value[names(value) != "progress"], json_or_scalar)
  response_value$progress <- lapply(unname(value$progress), set_scalar)
  response_value$id <- scalar(id)
  response_value
}

hintr_error <- function(message, error, status_code = 400L, ...) {
  key <- scalar(new_error_id())
  porcelain::porcelain_stop(message, error, errors = NULL,
                            status_code = status_code, key = key, ...)
}

new_error_id <- function() {
  ids::proquint(n_words = 3)
}

hintr_404_handler <- function(req, res) {
  ## Manually construct the response here
  res$status <- 404L
  message <- t_("ERROR_404",
                 list(verb = req$REQUEST_METHOD, path = req$PATH_INFO))
  list(
    status = scalar("failure"),
    errors = list(
      list(
        error = scalar("NOT_FOUND"),
        detail = scalar(message),
        key = scalar(new_error_id())
      )
    ),
    data = NULL
  )
}
