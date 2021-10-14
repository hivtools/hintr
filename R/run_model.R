run_model <- function(data, options, path_results, path_prerun = NULL,
                      language = NULL) {
  if (!is.null(language)) {
    reset_hintr <- traduire::translator_set_language(language)
    reset_naomi <-
      traduire::translator_set_language(language, package = "naomi")
    on.exit({
      reset_hintr()
      reset_naomi()
    })
  }

  if (use_mock_model()) {
    message("Starting mock model run")
    progress_start <- list(
      started = list(
        started = TRUE,
        complete = FALSE,
        name = t_("RUN_MODEL_MOCK_START")
      ),
      complete = list(
        started = FALSE,
        complete = FALSE,
        name = t_("RUN_MODEL_MOCK_FINISH")
      )
    )

    signalCondition(structure(progress_start,
                              class = c("progress", "condition")))
    Sys.sleep(5)
    progress_complete <- list(
      started = list(
        started = TRUE,
        complete = TRUE,
        name = t_("RUN_MODEL_MOCK_START")
      ),
      complete = list(
        started = TRUE,
        complete = FALSE,
        name = t_("RUN_MODEL_MOCK_FINISH"),
        helpText = "model running"
      )
    )
    signalCondition(structure(progress_complete,
                              class = c("progress", "condition")))
    output <- list(
      plot_data_path = NULL,
      model_output_path = system_file("output", "malawi_model_output.rds"),
      version = packageVersion("naomi"))
    class(output) <- "hintr_output"
    return(output)
  }

  if (!is.null(path_prerun)) {
    p <- PrerunModelResults$new(path_prerun)
    data <- naomi:::format_data_input(data)
    inputs <- naomi:::naomi_info_input(data)
    if (p$exists(inputs)) {
      message("Found prerun model results")
      return(p$get(inputs))
    }
  }

  path_results <- normalizePath(path_results, mustWork = TRUE)
  model_output_path <- tempfile("model_output", tmpdir = path_results,
                                fileext = ".rds")

  ## Fix some labels to match what naomi requires
  data$art_number <- data$programme
  data$programme <- NULL
  data$anc_testing <- data$anc
  data$anc <- NULL

  naomi::hintr_run_model(data, options, model_output_path, validate = FALSE)
}

select_data <- function(data) {
  columns <- c("area_id", "sex", "age_group", "calendar_quarter",
               "indicator", "mode", "mean", "lower", "upper")
  data[, columns]
}

process_result <- function(model_output) {
  output <- readRDS(model_output$plot_data_path)
  output_filters <- get_model_output_filters(output)
  metadata <- list(
    barchart = list(
      indicators = get_barchart_metadata(output),
      filters = output_filters,
      defaults = get_barchart_defaults(output, output_filters)
    ),
    choropleth = list(
      indicators = get_choropleth_metadata(output),
      filters = output_filters
    )
  )
  list(data = select_data(output),
       plottingMetadata = metadata,
       warnings = list(
         list(
           text = scalar("ART coverage greater than 100% for 10 age groups"),
           locations = list(scalar("model_calibrate"))
         ),
         list(
           text = scalar("Prevalence greater than 40%"),
           locations = list(scalar("model_calibrate"), scalar("review_output"))
         ),
         list(
           text = scalar("ART coverage greater tha 100%"),
           locations = list(scalar("review_output"))
         )
       ))
}

use_mock_model <- function() {
  Sys.getenv("USE_MOCK_MODEL") == "true"
}
