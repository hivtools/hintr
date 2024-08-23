do_plotting_metadata <- function(iso3) {
  metadata <- get_plotting_metadata(iso3)
  metadata <- metadata[metadata$data_type %in%
                         c("survey", "anc", "output", "programme"), ]
  metadata <- metadata[order(metadata$indicator_sort_order), ]
  lapply(split(metadata, metadata$data_type), build_data_type_metadata)
}

get_plotting_metadata <- function(...) {
  naomi::get_plotting_metadata(...)
}

build_data_type_metadata <- function(metadata) {
  lapply(split(metadata, metadata$plot_type), build_plot_type_metadata)
}

build_plot_type_metadata <- function(metadata) {
  list(indicators =
         lapply(metadata$indicator, function(indicator) {
           build_indicator_metadata(metadata[metadata$indicator == indicator, ])
         }))
}

build_indicator_metadata <- function(metadata) {
  if (nrow(metadata) != 1) {
    stop(t_("METADATA_BUILD_INDICATOR"))
  }
  list(
    indicator = scalar(metadata$indicator),
    value_column = scalar(metadata$value_column),
    indicator_column = scalar(metadata$indicator_column),
    indicator_value = scalar(metadata$indicator_value),
    name = scalar(metadata$name),
    min = scalar(metadata$min),
    max = scalar(metadata$max),
    colour = scalar(metadata$colour),
    invert_scale = scalar(metadata$invert_scale),
    scale = scalar(metadata$scale),
    accuracy = if (is.na(metadata$accuracy)) {
      json_null()
    } else {
      scalar(metadata$accuracy)
    },
    format = scalar(metadata$format)
  )
}

get_barchart_metadata <- function(output, data_type = "output") {
  metadata <- naomi::get_metadata()
  metadata <- metadata[
    metadata$data_type == data_type & metadata$plot_type == "barchart",
    c("indicator", "value_column", "error_low_column", "error_high_column",
      "indicator_column", "indicator_value", "indicator_sort_order",
      "name", "scale", "accuracy", "format")
  ]
  metadata[order(metadata$indicator_sort_order), ]
}

get_indicator_metadata <- function(data_type, plot_type, data) {
  iso3 <- get_country_iso3(data$area_id)
  metadata <- get_plotting_metadata(iso3)
  metadata <- metadata[
    metadata$data_type == data_type & metadata$plot_type == plot_type,
    c("indicator", "value_column", "error_low_column", "error_high_column",
      "indicator_column", "indicator_value", "indicator_sort_order",
      "name", "min", "max", "colour",
      "invert_scale", "scale", "accuracy", "format")
  ]
  metadata[order(metadata$indicator_sort_order), ]
}

get_country_iso3 <- function(area_ids) {
  sub("([A-Z]{3}).*", "\\1", area_ids[1])
}

get_output_plot_settings_control <- function(filter_types) {
  list(
    choropleth = get_choropleth_settings(filter_types),
    barchart = get_barchart_settings(filter_types),
    table = get_table_settings(filter_types),
    bubble = get_bubble_settings(filter_types)
  )
}

get_choropleth_settings <- function(filter_types) {
  filter_ids <- c("indicator", "detail", "area", "period", "sex", "age")
  detail_options <- get_filter_option_ids(filter_types, "detail")
  list(
    defaultEffect = list(
      setFilters = lapply(filter_ids, get_filter_from_id),
      setMultiple = "area",
      setFilterValues = list(
        indicator = c("prevalence"),
        detail = detail_options[length(detail_options)]
      )
    ),
    plotSettings = list()
  )
}

get_barchart_settings <- function(filter_types) {
  default_filter_ids <- c("indicator", "area", "period", "sex", "age")
  all_filter_ids <- c("indicator", "detail", "area", "period", "sex", "age")
  default_filters <- lapply(default_filter_ids, get_filter_from_id)
  all_filters <- lapply(all_filter_ids, get_filter_from_id)

  area_effect <- list(
    id = scalar("area"),
    label = scalar(get_label_for_id("area")),
    effect = list(
      setMultiple = "area",
      setFilters = all_filters
    )
  )
  age_effect <- list(
    id = scalar("age"),
    label = scalar(get_label_for_id("age")),
    effect = list(
      setMultiple = "age",
      setFilterValues = list(
        age = naomi::get_five_year_age_groups()
      )
    )
  )
  period_effect <- list(
    id = scalar("period"),
    label = scalar(get_label_for_id("period")),
    effect = list(
      setMultiple = "period",
      setFilterValues = list(
        period = get_filter_option_ids(filter_types, "period")
      )
    )
  )
  sex_effect <- list(
    id = scalar("sex"),
    label = scalar(get_label_for_id("sex")),
    effect = list(
      setMultiple = "sex",
      setFilterValues = list(
        sex = c("male", "female")
      )
    )
  )
  x_axis_or_disagg_by_options <- list(
    area_effect,
    period_effect,
    sex_effect,
    age_effect
  )

  list(
    defaultEffect = list(
      setFilters = default_filters,
      setFilterValues = list(
        indicator = c("prevalence"),
        period = get_filter_option_ids(filter_types, "period")[2],
        sex = c("both")
      )
    ),
    plotSettings = list(
      list(
        id = scalar("x_axis"),
        label = scalar(t_("OUTPUT_BARCHART_X_AXIS")),
        options = x_axis_or_disagg_by_options,
        value = scalar("age")
      ),
      list(
        id = scalar("disagg_by"),
        label = scalar(t_("OUTPUT_BARCHART_DISAGG_BY")),
        options = x_axis_or_disagg_by_options,
        value = scalar("sex")
      )
    )
  )
}

get_calibrate_plot_settings_control <- function(filter_types) {
  list(
    calibrate = get_calibration_plot_settings(filter_types)
  )
}

get_calibration_plot_settings <- function(filter_types) {
  calibrate_only_settings <- list(
    list(
      filterId = scalar("calibrate_indicator"),
      label = scalar(get_label_for_id("calibrate_indicator")),
      stateFilterId = scalar("indicator")
    )
  )
  filterIds <- c("spectrum_region", "type", "period", "sex", "age")
  list(
    defaultEffect = list(
      setFilters = c(calibrate_only_settings,
                     lapply(filterIds, get_filter_from_id)),
      setFilterValues = list(
        indicator = c("prevalence"),
        period = get_filter_option_ids(filter_types, "period")[2]
      ),
      setHidden = c(
        "type", "spectrum_region"
      )
    ),
    ## x-axis and disaggregate plot settings are hidden as users cannot
    ## change these in the calibrate plot
    plotSettings = list(
      list(
        id = scalar("x_axis"),
        label = scalar(t_("OUTPUT_BARCHART_X_AXIS")),
        options = list(get_x_axis_or_disagg_by_option("spectrum_region")),
        hidden = scalar(TRUE)
      ),
      list(
        id = scalar("disagg_by"),
        label = scalar(t_("OUTPUT_BARCHART_DISAGG_BY")),
        options = list(get_x_axis_or_disagg_by_option("type")),
        hidden = scalar(TRUE)
      )
    )
  )
}

get_comparison_plot_settings_control <- function(filter_types) {
  list(
    comparison = get_comparison_plot_settings(filter_types)
  )
}

get_comparison_plot_settings <- function(filter_types) {
  default_filter_ids <- c("source", "indicator", "area", "age", "period", "sex")
  all_filter_ids <- c("source", "indicator", "detail", "area")
  default_filters <- lapply(default_filter_ids, get_filter_from_id)
  all_filters <- lapply(all_filter_ids, get_filter_from_id)

  period_x_axis_effect <- list(
    id = scalar("period"),
    label = scalar(get_label_for_id("period")),
    effect = list(
      setMultiple = "period",
      setFilterValues = list(
        period = get_filter_option_ids(filter_types, "period")
      )
    )
  )
  sex_x_axis_effect <- list(
    id = scalar("sex"),
    label = scalar(get_label_for_id("sex")),
    effect = list(
      setMultiple = "sex",
      setFilterValues = list(
        sex = get_filter_option_ids(filter_types, "sex")
      )
    )
  )
  age_x_axis_effect <- list(
    id = scalar("age"),
    label = scalar(get_label_for_id("age")),
    effect = list(
      setMultiple = "age",
      setFilterValues = list(
        age = naomi::get_five_year_age_groups()
      )
    )
  )
  area_x_axis_effect <- list(
    id = scalar("area"),
    label = scalar(get_label_for_id("area")),
    effect = list(
      setMultiple = "area",
      setFilters = all_filters
    )
  )
  x_axis_settings <- list(
    area_x_axis_effect,
    period_x_axis_effect,
    sex_x_axis_effect,
    age_x_axis_effect
  )
  ## TODO: In current plot when you change indicator, it updates
  ## the filters. We could support this same behaviour by making
  ## indicator a plot control which updates the filter values
  ## including a hidden "indicator" filter which would be the value
  ## actually used for filtering the data. But let's check what we
  ## actually want to do. Would have to set the x-axis too, which I don't
  ## think we can support yet.
  list(
    defaultEffect = list(
      setFilters = default_filters,
      setFilterValues = list(
        indicator = c("prevalence"),
        period = get_filter_option_ids(filter_types, "period")[2]
      ),
      setHidden = c(
        "source"
      )
    ),
    ## disaggregate plot settings are not visible as users cannot
    ## change these in the comparison plot
    plotSettings = list(
      list(
        id = scalar("x_axis"),
        label = scalar(t_("OUTPUT_BARCHART_X_AXIS")),
        options = x_axis_settings,
        value = scalar("age")
      ),
      list(
        id = scalar("disagg_by"),
        label = scalar(t_("OUTPUT_BARCHART_DISAGG_BY")),
        options = list(get_x_axis_or_disagg_by_option("source")),
        hidden = scalar(TRUE)
      )
    )
  )
}

get_filter_option_ids <- function(filter_types, type) {
  selected <- NULL
  for (filter in filter_types) {
    if (filter$id == type) {
      selected <- filter
      break
    }
  }
  if (is.null(selected)) {
    stop(t_("MAPPING_NO_MATCHING", list(type = type)))
  }
  lapply(selected$options, function(opt) {
    opt$id
  })
}

get_x_axis_or_disagg_by_option <- function(id) {
  list(
    id = scalar(id),
    label = scalar(get_label_for_id(id)),
    effect = list(
      setMultiple = id
    )
  )
}

get_table_settings <- function(filter_types) {
  list(
    defaultEffect = list(
      setFilterValues = list(
        indicator = c("prevalence"),
        period = get_filter_option_ids(filter_types, "period")[2]
      )
    ),
    plotSettings = list(
      list(
        id = scalar("presets"),
        label = scalar(t_("OUTPUT_TABLE_PRESETS")),
        options = get_table_presets(filter_types)
      )
    )
  )
}

get_table_presets <- function(filter_types) {
  detail_options <- get_filter_option_ids(filter_types, "detail")
  list(
    list(
      id = scalar("sex_by_area"),
      label = scalar(t_("TABLE_SEX_BY_AREA")),
      effect = list(
        setFilters = lapply(
          c("indicator", "area", "detail", "period", "sex", "age"),
          get_filter_from_id),
        ## Hide the area ID as we want people to just select the detail level
        ## and see all rows within that level. Having the area filter available
        ## too makes this confusing, but we need area to exist for picking
        ## up the row labels
        setHidden = c("area"),
        setMultiple = c("sex", "area"),
        setFilterValues = list(
          detail = detail_options[length(detail_options)]
        ),
        customPlotEffect = list(
          row = c("area"),
          column = c("sex")
        )
      )
    ),
    list(
      id = scalar("sex_by_5_year_age_group"),
      label = scalar(t_("TABLE_SEX_BY_5_YEAR_AGE_GROUP")),
      effect = list(
        setFilters = lapply(c("indicator", "area", "period", "sex", "age"),
                            get_filter_from_id),
        setMultiple = c("sex", "age"),
        setFilterValues = list(
          age = naomi::get_five_year_age_groups()
        ),
        customPlotEffect = list(
          row = c("age"),
          column = c("sex")
        )
      )
    )
  )
}

get_bubble_settings <- function(filter_types) {
  indicators <- list(
    list(
      filterId = scalar("indicator"),
      label = scalar(t_("OUTPUT_BUBBLE_COLOUR_INDICATOR")),
      stateFilterId = scalar("colourIndicator")
    ),
    list(
      filterId = scalar("indicator"),
      label = scalar(t_("OUTPUT_BUBBLE_SIZE_INDICATOR")),
      stateFilterId = scalar("sizeIndicator")
    )
  )
  base_filter_ids <- lapply(c("detail", "area", "period", "sex", "age"),
                            get_filter_from_id)
  detail_options <- get_filter_option_ids(filter_types, "detail")
  list(
    defaultEffect = list(
      setFilters = c(indicators, base_filter_ids),
      setMultiple = "area",
      setFilterValues = list(
        colourIndicator = c("prevalence"),
        sizeIndicator = c("plhiv"),
        detail = detail_options[length(detail_options)]
      )
    ),
    plotSettings = list()
  )
}

get_filter_from_id <- function(id) {
  list(
    filterId = scalar(id),
    label = scalar(get_label_for_id(id)),
    stateFilterId = scalar(id)
  )
}

get_label_for_id <- function(id) {
  t_(
    switch(id,
      "area" = "OUTPUT_FILTER_AREA",
      "period" = "OUTPUT_FILTER_PERIOD",
      "sex" = "OUTPUT_FILTER_SEX",
      "age" = "OUTPUT_FILTER_AGE",
      "detail" = "OUTPUT_FILTER_AREA_LEVEL",
      "indicator" = "OUTPUT_FILTER_INDICATOR",
      "calibrate_indicator" = "OUTPUT_FILTER_INDICATOR",
      "type" = "OUTPUT_FILTER_TYPE",
      "spectrum_region" = "OUTPUT_FILTER_SPECTRUM_REGION",
      "source" = "OUTPUT_FILTER_DATA_TYPE"
    )
  )
}
