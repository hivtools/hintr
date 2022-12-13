rehydrate_submit <- function(queue) {
  function(input) {
    tryCatch({
      input <- jsonlite::fromJSON(input)
      assert_file_exists(input$file$path)
      files <- zip::zip_list(input$file$path)
      if (PROJECT_STATE_PATH %in% files$filename) {
        id <- queue$submit_rehydrate_state(input$file)
      } else if ("hintr_output.rds" %in% files$filename) {
        ## This is not ideal that we do this sync but if we want to save
        ## outputs into output dir on disk then we need to create the result
        ## The worker cannot do this as it cannot connect to queue
        ## to run a new fit
        t <- tempfile()
        dir.create(t)
        hintr_output <- naomi::read_rehydrate_zip(input$file$path, t)
        hintr_output <- rehydrate_save_output(queue$results_dir, hintr_output)
        fit_id <- rehydrate_create_result(queue, hintr_output)
        calibrate_id <- rehydrate_create_result(queue, hintr_output)
        queue$queue$task_wait(fit_id, timeout = 3)
        queue$queue$task_wait(calibrate_id, timeout = 3)
        id <- queue$submit_rehydrate_files(hintr_output, fit_id, calibrate_id)
      } else {
        stop(t_("FAILED_ZIP_REHYDRATE_SUBMIT"))
      }
      list(id = scalar(id))
    }, error = function(e) {
      hintr_error(e$message, "REHYDRATE_SUBMIT_FAILED")
    })
  }
}

rehydrate_from_state <- function(output_zip) {
  con <- unz(output_zip$path, PROJECT_STATE_PATH)
  on.exit(close(con))
  state <- paste0(readLines(con), collapse = "\n")
  state <- json_verbatim(state)
  files <- zip::zip_list(output_zip$path)
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

rehydrate_from_files <- function(hintr_output, fit_id, calibrate_id,
                                 uploads_dir) {
  output <- naomi::read_hintr_output(hintr_output$model_output_path)

  inputs <- rehydrate_inputs(output, uploads_dir)
  fit <- rehydrate_fit(fit_id, output)
  calibrate <- rehydrate_calibrate(calibrate_id, output)
  version <- rehydrate_version(hintr_output$version)
  state <- list(
    datasets = inputs,
    model_fit = fit,
    calibrate = calibrate,
    version = version
  )
  ## Note no notes in output zip, could add this as an enhancement
  notes <- ""
  list(
    notes = scalar(notes),
    state = state
  )
}

rehydrate_inputs <- function(output, uploads_dir) {
  ## Build the inputs JSON to look like the format used by the web app
  ## This is a bit hacky but we are limited atm because the output zip
  ## does not include input files yet
  ## This will error in the web app if the input files are not found
  ## but is a bit delicate, next step will be to either include inputs
  ## in the zip and save them uploads dir or reconstruct them from outputs
  inputs <-  read_csv(output$info$inputs.csv)
  rehydrated <- lapply(seq_len(nrow(inputs)), function(row_no) {
    row <- inputs[row_no, ]
    filename <- row$filename
    path <- file.path(uploads_dir, filename)
    if (!file.exists(path)) {
      stop(t_("REHYDRATE_INPUT_MISSING",
              list(filename = filename,
                   hash = row$md5sum)))
    }
    list(
      path = scalar(path),
      filename = scalar(filename)
    )
  })
  files <- inputs$role
  ## Map from names used by Naomi to names used by interface
  files[files == "art_number"] <- "programme"
  files[files == "anc_testing"] <- "anc"
  names(rehydrated) <- files
  rehydrated
}

rehydrate_fit <- function(fit_id, output) {
  fit_options <- recursive_scalar(
    yaml::read_yaml(text = output$info$options.yml))
  list(
    options = fit_options,
    id = scalar(fit_id)
  )
}

rehydrate_calibrate <- function(calibrate_id, output) {
  calibration_options <- recursive_scalar(
    yaml::read_yaml(text = output$info$calibration_options.yml))
  list(
    options = calibration_options,
    id = scalar(calibrate_id)
  )
}

rehydrate_save_output <- function(results_dir, hintr_output) {
  path_results <- normalizePath(results_dir, mustWork = TRUE)
  model_output_path <- tempfile("model_output", tmpdir = path_results,
                                fileext = ".qs")
  plot_data_path <- tempfile("plot_data", tmpdir = path_results,
                             fileext = ".qs")
  file.copy(hintr_output$model_output_path, model_output_path)
  file.copy(hintr_output$plot_data_path, plot_data_path)
  hintr_output$model_output_path <- model_output_path
  hintr_output$plot_data_path <- plot_data_path
  hintr_output
}

rehydrate_create_result <- function(queue, hintr_output) {
  queue$submit(quote(identity(hintr_output)))
}

rehydrate_version <- function(naomi_version) {
  version <- cfg$version_info
  version$naomi <- scalar(as.character(naomi_version))
  version
}

rehydrate_result <- function(queue) {
  function(id) {
    res <- queue$result(id)
    if (is_error(res)) {
      hintr_error(res$message, "PROJECT_REHYDRATE_FAILED")
    }
    res
  }
}
