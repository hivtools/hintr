run_model <- function(data, options, path_results, language = NULL) {
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

    if (!is.null(options$mock_model_trigger_error) &&
        options$mock_model_trigger_error == "true") {
      stop(t_("MOCK_MODEL_ERROR", list(option = "mock_model_trigger_error")))
    }

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
      model_output_path = system_file("output", "malawi_model_output.qs"),
      version = utils::packageVersion("naomi"),
      warnings = list(
        list(
          text = scalar(paste0("Zero population input for 8 population ",
                               "groups. Replaced with population 0.1.")),
          locations = list(scalar("model_fit"))
        )
      ))
    class(output) <- "hintr_output"
    return(output)
  }

  path_results <- normalizePath(path_results, winslash = "/", mustWork = TRUE)
  model_output_path <- tempfile("model_output", tmpdir = path_results,
                                fileext = ".qs")

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
  output <- naomi::read_hintr_output(model_output$plot_data_path)
  filters <- get_model_output_filters(output)
  metadata <- build_output_metadata(output, filters)
  table_metadata <- build_output_table_metadata(output, filters)
  warnings <- list()
  if (!is.null(model_output$warnings)) {
    warnings <- warnings_scalar(model_output$warnings)
  }
  list(data = select_data(output),
       plottingMetadata = metadata,
       tableMetadata = table_metadata,
       warnings = warnings)
}

build_output_metadata <- function(output, filters) {
  list(
    barchart = list(
      indicators = get_barchart_metadata(output),
      filters = filters,
      defaults = get_barchart_defaults(output, filters)
    ),
    choropleth = list(
      indicators = get_choropleth_metadata(output),
      filters = filters
    )
  )
}

build_output_table_metadata <- function(output, filters) {
  area_level_filter <- get_area_level_filter(output)
  filter_ids <- vcapply(filters, "[[", "id")
  sex_by_area_filters <- c(list(area_level_filter), filters[filter_ids != "area"])
  sex_by_area <- list(
    filters = sex_by_area_filters,
    defaults = list(
      id = scalar("sex_by_area"),
      label = scalar(t_("TABLE_SEX_BY_AREA")),
      column = list(
        id = scalar("sex"),
        label = scalar(t_("OUTPUT_FILTER_SEX"))
      ),
      row = list(
        id = scalar("area_id"),
        label = scalar(t_("OUTPUT_FILTER_AREA"))
      )
    )
  )
  sex_by_5_year_age_group <- list(
    filters = filters,
    defaults = list(
      id = scalar("sex_by_5_year_age_group"),
      label = scalar(t_("TABLE_SEX_BY_5_YEAR_AGE_GROUP")),
      column = list(
        id = scalar("sex"),
        label = scalar(t_("OUTPUT_FILTER_SEX"))
      ),
      row = list(
        id = scalar("age_group"),
        label = scalar(t_("OUTPUT_FILTER_AGE"))
      ),
      selected_filter_options = list(
        age = naomi::get_five_year_age_groups()
      )
    )
  )
  list(presets = list(sex_by_area, sex_by_5_year_age_group))
}


use_mock_model <- function() {
  Sys.getenv("USE_MOCK_MODEL") == "true"
}
