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

    signalCondition(structure(list(message = progress_start),
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
        name = t_("RUN_MODEL_MOCK_FINISH")
      )
    )
    signalCondition(structure(list(message = progress_complete),
                              class = c("progress", "condition")))
    return(list(output_path = system_file("output", "malawi_output.rds"),
         spectrum_path = system_file("output", "malawi_spectrum_download.zip"),
         summary_path = system_file("output", "malawi_summary_download.zip"),
         metadata = list(areas = "MWI")))
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
  output_path <- tempfile(tmpdir = path_results, fileext = ".rds")
  spectrum_path <- tempfile(tmpdir = path_results, fileext = ".zip")
  summary_path <- tempfile(tmpdir = path_results, fileext = ".zip")

  ## Fix some labels to match what naomi requires
  data$art_number <- data$programme
  data$programme <- NULL
  data$anc_testing <- data$anc
  data$anc <- NULL

  naomi::hintr_run_model(data, options, output_path, spectrum_path,
                         summary_path)
}

select_data <- function(data) {
  columns <- c("area_id", "sex", "age_group", "calendar_quarter",
               "indicator_id", "mode", "mean", "lower", "upper")
  data[, columns]
}

process_result <- function(model_output) {
  output <- readRDS(model_output$output_path)
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
       plottingMetadata = metadata)
}

use_mock_model <- function() {
  Sys.getenv("USE_MOCK_MODEL") == "true"
}
