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
  rrq:::run_task_cleanup(queue$queue$con,
                         queue$queue$keys,
                         queue$queue$.__enclos_env__$private$store,
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
