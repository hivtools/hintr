#' Submit a prerun to the web app
#'
#' This requires VPN or RDP access to work.
#'
#' This will take all model files and upload to a specified server and
#' output the model output zip which can be saved into the ADR or
#' uploaded into the Naomi app to view plots of model outputs.
#'
#' Can use this for countries which cannot get a fit to work via the app
#' you can prepare a model fit locally and then upload those outputs into
#' the app.
#'
#' @param inputs The model inputs, a named list of file paths including
#'   pjnz, shape, population, survey and optionally programme and anc.
#' @param model_output The `hintr_output` object from model fit
#' @param calibrate_output The `hintr_output` object from calibration
#' @param server The server URL to upload files to
#' @param port The port the API is running on
#' @param output_zip_path The path to save the output zip at, will use
#'   a tempfile by default
#'
#' @return Path to the generated output zip
#' @export
hintr_submit_prerun <- function(inputs, model_output, calibrate_output,
                                server = "http://naomi.unaids.org",
                                port = "8888",
                                output_zip_path = tempfile(fileext = ".zip")) {
  if (!naomi:::is_hintr_output(model_output)) {
    stop("Model output must be hintr_output object")
  }
  if (!naomi:::is_hintr_output(calibrate_output)) {
    stop("Calibrate output must be hintr_output object")
  }
  required <- c("pjnz", "shape", "population", "survey")
  optional <- c("programme", "anc")
  assert_names(inputs, required, optional)
  assert_files_exist(inputs)

  if (!is.null(port)) {
    url <- paste0(server, ":", port)
  } else {
    url <- server
  }

  uploaded_inputs <- lapply(names(inputs), function(name) {
    message(sprintf("Uploading input %s", name))
    input <- inputs[[name]]
    filename <- basename(input)
    res <- httr::POST(paste0(url, "/internal/upload/input/", filename),
                      body = httr::upload_file(input,
                                               "application/octet-stream"))
    httr::stop_for_status(res)
    httr::content(res)$data
  })
  names(uploaded_inputs) <- names(inputs)

  output_upload <- stats::setNames(
    c(model_output$model_output_path, calibrate_output$plot_data_path,
      calibrate_output$model_output_path),
    c("fit_model_output", "calibrate_plot_data", "calibrate_model_output"))
  uploaded_outputs <- lapply(names(output_upload), function(name) {
    message(sprintf("Uploading output %s", name))
    output <- output_upload[[name]]
    filename <- basename(output)
    res <- httr::POST(paste0(url, "/internal/upload/result/", filename),
                      body = httr::upload_file(output,
                                               "application/octet-stream"))
    httr::stop_for_status(res)
    httr::content(res)$data
  })
  names(uploaded_outputs) <- names(output_upload)

  message("File uploads complete, building state")
  prerun_body <- list(
    inputs = recursive_scalar(uploaded_inputs),
    outputs = recursive_scalar(uploaded_outputs)
  )
  res <- httr::POST(paste0(url, "/internal/prerun"),
                    body = prerun_body,
                    encode = "json")
  httr::stop_for_status(res)
  state <- httr::content(res)$data

  message("Creating model output zip")
  out <- naomi::hintr_prepare_spectrum_download(calibrate_output,
                                                output_zip_path)
  add_state_json(out$path, jsonlite::toJSON(recursive_scalar(state)))
  out$path
}

prerun <- function(queue) {
  function(input) {
    files <- jsonlite::fromJSON(input, simplifyVector = FALSE)
    all_files <- c(files$inputs, files$outputs)
    paths <- vapply(all_files, "[[", character(1), "path")
    missing_files <- all_files[!file.exists(paths)]
    if (length(missing_files) > 0) {
      msg <- vapply(names(missing_files), function(name) {
        file <- missing_files[[name]]
        sprintf(
          "File '%s' at path '%s' with original name '%s' does not exist.",
          name, file$path, file$filename)
      }, character(1))
      hintr_error(paste0(
        paste(msg, collapse = "\n"), "\n",
        "Make sure to upload them first with '/internal/upload/*' endpoints."),
        "PRERUN_MISSING_FILES")
    }

    model_fit_output <- naomi:::build_hintr_output(
      NULL,
      files$outputs$fit_model_output$path,
      warnings = NULL)
    calibrate_output <- naomi:::build_hintr_output(
      files$outputs$calibrate_plot_data$path,
      files$outputs$calibrate_model_output$path,
      warnings = NULL)
    prerun_build_state(queue, files$inputs, model_fit_output, calibrate_output)
  }
}

prerun_build_state <- function(queue, inputs, model_fit_output,
                               calibrate_output) {
  output <- naomi::read_hintr_output(calibrate_output$model_output_path)
  model_fit_options <- yaml::read_yaml(text = output$info$options.yml)
  calibration_options <- yaml::read_yaml(text =
                                           output$info$calibration_options.yml)
  packages <- utils::read.csv(text = output$info$packages.csv)

  inputs <- build_state_inputs(inputs)
  fit <- build_state_output(queue, model_fit_output, model_fit_options)
  calibrate <- build_state_output(queue, calibrate_output, calibration_options)
  version <- build_state_version(packages$version[packages$name == "naomi"])
  list(
    datasets = inputs,
    model_fit = fit,
    calibrate = calibrate,
    version = version
  )
}

build_state_inputs <- function(inputs) {
  lapply(inputs, function(input) {
    ## web app expects path like as uploads/file_name.csv
    ## if leading / is included then it takes "uploads" as the filename and
    ## errors
    path <- sub("^/", "", input$path)
    list(
      path = scalar(path),
      filename = scalar(input$filename)
    )
  })
}

build_state_output <- function(queue, output, options) {
  id <- create_result(queue, output)
  list(
    options = recursive_scalar(options),
    id = scalar(id)
  )
}

create_result <- function(queue, result) {
  task_id <- ids::random_id()
  rrq:::run_task_cleanup_success(queue$queue$con,
                                 r6_private(queue$queue)$keys,
                                 r6_private(queue$queue)$store,
                                 task_id,
                                 rrq:::TASK_COMPLETE,
                                 result)
  task_id
}

build_state_version <- function(naomi_version) {
  version <- cfg$version_info
  version$naomi <- scalar(naomi_version)
  version
}
