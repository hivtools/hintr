prerun <- function(queue) {
  function(input) {
    files <- jsonlite::fromJSON(input, simplifyVector = FALSE)
    all_files <- c(files$inputs, files$outputs)
    lapply(c(names(all_files)), function(name) {
      file <- all_files[[name]]
      if (!file.exists(file$path)) {
        hintr_error(sprintf(paste0(
          "File '%s' at path '%s' with original name '%s' ",
          "does not exist. Make sure to upload it first ",
          "with '/internal/upload/*' endpoints."),
          name, file$path, file$filename),
          "PRERUN_MISSING_FILES")
      }
    })

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
  packages <- read.csv(text = output$info$packages.csv)

  inputs <- build_state_inputs(inputs)
  fit <- build_state_output(queue, model_fit_output, model_fit_options)
  calibrate <- build_state_output(queue, calibrate_output, calibration_options)
  version <- build_state_version(packages[packages$name == "naomi", "version"])
  state <- list(
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
    if (substring(input$path, 1, 1) == "/") {
      path <- substring(input$path, 2)
    }
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
  version$naomi <- scalar(as.character(naomi_version))
  version
}
