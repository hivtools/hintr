input_comparison <- function(input) {
  input <- jsonlite::fromJSON(input)

  withCallingHandlers({
    if (is.null(input$programme) && is.null(input$anc)) {
      stop("Cannot build input comparison plot without either programme or anc data")
    }
    if (!is.null(input$programme)) {
      assert_file_exists(input$programme$path)
    }
    if (!is.null(input$anc)) {
      assert_file_exists(input$anc$path)
    }
    assert_file_exists(input$shape$path)
    assert_file_exists(input$pjnz$path)
    data <- as.data.frame(
      naomi::prepare_spectrum_naomi_comparison(input$programme$path,
                                               input$anc$path,
                                               input$shape$path,
                                               input$pjnz$path))
    metadata <- build_input_comparison_metadata(data)
    list(
      data = data,
      metadata = metadata,
      warnings = list()
    )
  },
  error = function(e) {
    hintr_error(api_error_msg(e), "FAILED_TO_GENERATE_INPUT_COMPARISON")
  })
}

get_filter_options_translated <- function(data, key, values = NULL) {
  to_translate <- unique(data[[key]])
  if (!is.null(values)) {
    to_translate <- to_translate[to_translate %in% values]
  }
  lapply(to_translate, function(value) {
    list(id = scalar(value),
         label = scalar(t_(toupper(value))))
  })
}

get_filter_options <- function(data, key) {
  lapply(unique(data[[key]]), function(value) {
    list(id = scalar(value),
         label = scalar(to_upper_first(value)))
  })
}

build_input_comparison_metadata <- function(data) {
  indicator_metadata <- get_indicator_metadata("input_comparison", "barchart",
                                               list(iso3 = "default"))
  indicator_options <- read_wide_indicator_filters(data, "input_comparison")
  indicator_filter <- list(
    id = scalar("indicator"),
    column_id = scalar("indicator"),
    options = indicator_options
  )
  area_name_filter <- list(
    id = scalar("area_name"),
    column_id = scalar("area_name"),
    options = get_filter_options(data, "area_name")
  )
  year_filter <- list(
    id = scalar("year"),
    column_id = scalar("year"),
    options = get_year_filters(data)
  )
  anc_groups <- "anc_adult_female"
  art_groups <- c("art_children", "art_adult_both", "art_adult_female",
                  "art_adult_male")
  anc_group_filter <- list(
    id = scalar("anc_groups"),
    column_id = scalar("group"),
    options = get_filter_options_translated(data, "group", anc_groups)
  )
  art_group_filter <- list(
    id = scalar("art_groups"),
    column_id = scalar("group"),
    options = get_filter_options_translated(data, "group", art_groups)
  )
  data_source_filter <- list(
    id = scalar("data_source"),
    column_id = scalar("data_source"),
    options = get_filter_options(data, "data_source")
  )
  list(
    filterTypes = list(
      indicator_filter,
      area_name_filter,
      year_filter,
      anc_group_filter,
      art_group_filter,
      data_source_filter
    ),
    indicators = indicator_metadata,
    plotSettingsControl = list(
      inputComparisonBarchart = get_input_barchart_settings(indicator_options)
    )
  )
}

get_input_barchart_settings <- function(indicator_options) {
  anc_indicators <- c("anc_already_art", "anc_clients",
                      "anc_known_pos", "anc_tested" , "anc_tested_pos")
  programme_indicators <- "number_on_art"
  default_filter_ids <- c("indicator", "area_name", "year", "data_source")
  default_set_filters <- lapply(default_filter_ids, get_filter_from_id)
  indicator_control_options <- lapply(seq_along(indicator_options), function(idx) {
    id <- indicator_options[[idx]]$id
    label <- indicator_options[[idx]]$label
    if (id %in% anc_indicators) {
      group_id <- "anc_groups"
    } else {
      group_id <- "art_groups"
    }
    list(
      id = scalar(id),
      label = scalar(label),
      effect = list(
        setFilters = c(
          default_set_filters,
          list(list(
            filterId = scalar(group_id),
            label = scalar(t_("INPUT_COMPARISON_GROUP_LABEL")),
            stateFilterId = scalar("group")
          ))
        ),
        setFilterValues = list(
          indicator = list(id)
        )
      )
    )
  })

  x_axis_options <- list(
    list(
      id = scalar("year"),
      label = scalar("Year"), # not actually shown
      effect = list(
        setMultiple = "year"
      )
    )
  )
  disagg_options <- list(
    list(
      id = scalar("data_source"),
      label = scalar("Data source"), # not actually shown
      effect = list(
        setMultiple = "data_source",
        setFilterValues = list(
          data_source = c("spectrum", "naomi")
        )
      )
    )
  )

  list(
    defaultEffect = list(
      setFilters = default_set_filters
    ),
    plotSettings = list(
      list(
        id = scalar("indicator_control"),
        label = scalar(t_("OUTPUT_FILTER_INDICATOR")),
        options = indicator_control_options
      ),
      list(
        id = scalar("x_axis"),
        label = scalar(t_("OUTPUT_BARCHART_X_AXIS")),
        options = x_axis_options,
        value = scalar("year")
      ),
      list(
        id = scalar("disagg_by"),
        label = scalar(t_("OUTPUT_BARCHART_DISAGG_BY")),
        options = disagg_options,
        value = scalar("data_source")
      )
    )
  )
}
