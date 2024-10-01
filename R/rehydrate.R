rehydrate <- function(output_zip) {
  files <- zip::zip_list(output_zip$path)
  if (!(PROJECT_STATE_PATH %in% files$filename)) {
    stop(t_("FAILED_ZIP_REHYDRATE_SUBMIT"))
  }
  con <- unz(output_zip$path, PROJECT_STATE_PATH)
  on.exit(close(con))
  state <- paste0(readLines(con), collapse = "\n")
  state <- json_verbatim(state)
  notes <- NULL
  if (NOTES_PATH %in% files$filename) {
    notes_con <- unz(output_zip$path, NOTES_PATH)
    notes <- paste0(readLines(notes_con), collapse = "\n")
    on.exit(close(notes_con), add = TRUE)
  }
  list(
    notes = scalar(notes),
    state = scalar(state)
  )
}

rehydrate_submit <- function(queue) {
  function(input) {
    tryCatch({
      input <- jsonlite::fromJSON(input)
      assert_file_exists(input$file$path)
      list(id = scalar(queue$submit_rehydrate(input$file)))
    }, error = function(e) {
      hintr_error(api_error_msg(e), "REHYDRATE_SUBMIT_FAILED")
    })
  }
}

rehydrate_result <- function(queue) {
  function(id) {
    res <- queue$result(id)
    validate_rehydrate_result(queue, res)
    res
  }
}

validate_rehydrate_result <- function(queue, res) {
  if (is_error(res)) {
    hintr_error(api_error_msg(res), "PROJECT_REHYDRATE_FAILED")
  }
  state <- jsonlite::fromJSON(res$state, simplifyVector = FALSE)

  ## Input files must exist on disk
  lapply(names(state$datasets), function(dataset_name) {
    data_path <- state$datasets[[dataset_name]]$path
    if (!is.null(queue$inputs_dir)) {
      data_path <- file.path(queue$inputs_dir, basename(data_path))
    }

    if (!file.exists(data_path)) {
      hintr_error(t_("REHYDRATE_MISSING_INPUT_FILE",
                     list(type = file_types_label(dataset_name))),
                  "PROJECT_REHYDRATE_FAILED")
    }
  })

  ## IDs must exist in redis
  if (!queue$exists(state$model_fit$id)) {
    hintr_error(t_("REHYDRATE_MODEL_FIT_ID_UNKNOWN"),
                "PROJECT_REHYDRATE_FAILED")
  }
  if (!queue$exists(state$calibrate$id)) {
    hintr_error(t_("REHYDRATE_CALIBRATE_ID_UNKNOWN"),
                "PROJECT_REHYDRATE_FAILED")
  }
  invisible(TRUE)
}

