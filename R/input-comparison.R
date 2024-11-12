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

get_filter_options_translated <- function(values = NULL) {
  lapply(values, function(value) {
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
  data_indicators <- unique(data$indicator)
  present_indicators <- indicator_metadata[indicator_metadata$indicator %in% data_indicators, ]
  indicator_options <- construct_filter(present_indicators, "indicator", "name")
  indicator_ids <- vcapply(indicator_options, "[[", "id")
  indicator_labels <- vcapply(indicator_options, "[[", "label")
  indicator_id_label_map <- setNames(indicator_labels, indicator_ids)
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
    options = get_year_filters(data, decreasing = FALSE)
  )
  art_groups <- c("art_adult_both", "art_adult_female", "art_adult_male",
                  "art_children")
  anc_groups <- "anc_adult_female"
  ordered_groups <- c(art_groups, anc_groups)
  group_ids <- unique(data$group)
  ordered_group_ids <- ordered_groups[ordered_groups %in% group_ids]
  group_filter <- list(
    id = scalar("group"),
    column_id = scalar("group"),
    options = get_filter_options_translated(ordered_group_ids)
  )

  list(
    filterTypes = list(
      indicator_filter,
      area_name_filter,
      year_filter,
      group_filter
    ),
    indicators = indicator_metadata,
    plotSettingsControl = list(
      inputComparisonBarchart = get_input_barchart_settings(
        indicator_ids,
        group_ids,
        indicator_id_label_map),
      inputComparisonTable = get_input_table_settings(indicator_id_label_map,
                                                      group_ids,
                                                      art_groups,
                                                      anc_groups)
    )
  )
}

get_input_barchart_settings <- function(indicator_ids,
                                        group_ids,
                                        indicator_id_label_map) {
  plot_types <- list(
    "number_on_art_adult_both" = list(
      indicator = "number_on_art",
      group = "art_adult_both",
      label = t_("NUMBER_ON_ART_ADULT_BOTH")
    ),
    "number_on_art_adult_female" = list(
      indicator = "number_on_art",
      group = "art_adult_female",
      label = t_("NUMBER_ON_ART_ADULT_FEMALE")
    ),
    "number_on_art_adult_male" = list(
      indicator = "number_on_art",
      group = "art_adult_male",
      label = t_("NUMBER_ON_ART_ADULT_MALE")
    ),
    "number_on_art_children" = list(
      indicator = "number_on_art",
      group = "art_children",
      label = t_("NUMBER_ON_ART_CHILDREN")
    ),
    "anc_already_art" = list(
      indicator = "anc_already_art",
      group = "anc_adult_female",
      label = indicator_id_label_map["anc_already_art"]
    ),
    "anc_clients" = list(
      indicator = "anc_clients",
      group = "anc_adult_female",
      label = indicator_id_label_map["anc_clients"]
    ),
    "anc_known_pos" = list(
      indicator = "anc_known_pos",
      group = "anc_adult_female",
      label = indicator_id_label_map["anc_known_pos"]
    ),
    "anc_tested" = list(
      indicator = "anc_tested",
      group = "anc_adult_female",
      label = indicator_id_label_map["anc_tested"]
    ),
    "anc_tested_pos" = list(
      indicator = "anc_tested_pos",
      group = "anc_adult_female",
      label = indicator_id_label_map["anc_tested_pos"]
    )
  )
  plot_type_options <- lapply(names(plot_types), function(plot_type) {
    type <- plot_types[[plot_type]]
    if (!(type$indicator %in% indicator_ids) || !(type$group %in% group_ids)) {
      return(NULL)
    }
    list(
      id = scalar(plot_type),
      label = scalar(type$label),
      effect = list(
        setFilterValues = list(
          indicator = type$indicator,
          group = type$group
        )
      )
    )
  })
  plot_type_options <- plot_type_options[!vlapply(plot_type_options, is.null)]

  default_filter_ids <- c("indicator", "area_name", "year", "group")
  default_set_filters <- lapply(default_filter_ids, get_filter_from_id)

  list(
    defaultEffect = list(
      setFilters = default_set_filters,
      setHidden = c("indicator", "year", "group"),
      setMultiple = c("year")
    ),
    plotSettings = list(
      list(
        id = scalar("plot_type"),
        label = scalar(t_("INPUT_TIME_SERIES_COLUMN_PLOT_TYPE")),
        options = plot_type_options
      )
    )
  )
}

get_input_table_settings <- function(indicator_id_label_map, group_ids,
                                     art_groups, anc_groups) {
  art_indicators <- "number_on_art"
  anc_indicators <- c("anc_already_art", "anc_clients", "anc_known_pos",
                      "anc_tested", "anc_tested_pos")
  indicator_to_control_option <- function(indicator_id) {
    if (indicator_id %in% art_indicators) {
      groups <- art_groups[art_groups %in% group_ids]
    } else if (indicator_id %in% anc_indicators) {
      groups <- anc_groups[anc_groups %in% group_ids]
    } else {
      hintr_error(sprintf("Unknown input table indicator '%s'.", indicator_id),
                  "UNKNOWN_INDICATOR")
    }
    list(
      id = scalar(indicator_id),
      label = scalar(indicator_id_label_map[[indicator_id]]),
      effect = list(
        setFilterValues = list(
          indicator = indicator_id,
          group = groups
        )
      )
    )
  }
  indicator_control_options <- lapply(names(indicator_id_label_map),
                                      indicator_to_control_option)
  default_filter_ids <- c("indicator", "area_name", "year", "group")
  default_set_filters <- lapply(default_filter_ids, get_filter_from_id)
  list(
    defaultEffect = list(
      setFilters = default_set_filters,
      setHidden = c("indicator", "year", "group"),
      customPlotEffect = list(
        row = c("area_name", "year"),
        column = "group"
      ),
      setMultiple = c("area_name", "year", "group")
    ),
    plotSettings = list(
      list(
        id = scalar("indicator_control"),
        label = scalar(t_("OUTPUT_FILTER_INDICATOR")),
        options = indicator_control_options
      )
    )
  )
}
