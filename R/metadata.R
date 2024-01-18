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
      "name", "scale", "accuracy", "format")]
  metadata[order(metadata$indicator_sort_order), ]
}

get_choropleth_metadata <- function(output) {
  iso3 <- get_country_iso3(output$area_id)
  metadata <- get_plotting_metadata(iso3)
  metadata <- metadata[
    metadata$data_type == "output" & metadata$plot_type == "choropleth",
    c("indicator", "value_column", "error_low_column", "error_high_column",
      "indicator_column", "indicator_value", "indicator_sort_order",
      "name", "min", "max", "colour",
      "invert_scale", "scale", "accuracy", "format")]
  metadata[order(metadata$indicator_sort_order), ]
}


get_country_iso3 <- function(area_ids) {
  sub("([A-Z]{3}).*", "\\1", area_ids[1])
}

get_plot_settings_control <- function() {
  list(
    choropleth = get_choropleth_settings(),
    barchart = get_barchart_settings(),
    table = get_table_settings(),
    bubble = get_bubble_settings()
  )
}

get_choropleth_settings <- function() {
  filter_ids <- c("indicator", "detail", "area", "period", "sex", "age")
  list(
    defaultFilterTypes = lapply(filter_ids, get_filter_from_id),
    plotSettings = list()
  )
}

get_barchart_settings <- function() {
  base_filter_ids <- c("area", "period", "sex", "age")
  all_filter_ids <- c("indicator", base_filter_ids)
  x_axis_or_disagg_by_options <- lapply(base_filter_ids, get_x_axis_or_disagg_by_option)
  list(
    defaultFilterTypes = lapply(all_filter_ids, get_filter_from_id),
    plotSettings = list(
      list(
        id = scalar("x_axis"),
        label = scalar(t_("OUTPUT_BARCHART_X_AXIS")),
        options = x_axis_or_disagg_by_options
      ),
      list(
        id = scalar("disagg_by"),
        label = scalar(t_("OUTPUT_BARCHART_DISAGG_BY")),
        options = x_axis_or_disagg_by_options
      )
    )
  )
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

get_table_settings <- function() {
  list(
    plotSettings = list(
      list(
        id = scalar("presets"),
        label = scalar(t_("OUTPUT_TABLE_PRESETS")),
        options = get_table_presets()
      )
    )
  )
}

get_table_presets <- function() {
  list(
    list(
      id = scalar("sex_by_area"),
      label = scalar(t_("TABLE_SEX_BY_AREA")),
      effect = list(
        setFilters = lapply(c("indicator", "detail", "period", "sex", "age"), get_filter_from_id),
        setMultiple = c("sex")
      )
    ),
    list(
      id = scalar("sex_by_5_year_age_group"),
      label = scalar(t_("TABLE_SEX_BY_5_YEAR_AGE_GROUP")),
      effect = list(
        setFilters = lapply(c("indicator", "area", "period", "sex", "age"), get_filter_from_id),
        setMultiple = c("sex", "age"),
        setFilterValues = list(
          age = naomi::get_five_year_age_groups()
        )
      )
    )
  )
}

get_bubble_settings <- function() {
  indicators <- list(
    list(
      filterId = scalar("indicator"),
      label = scalar(t_("OUTPUT_BUBBLE_SIZE_INDICATOR")),
      stateFilterId = scalar("sizeIndicator")
    ),
    list(
      filterId = scalar("indicator"),
      label = scalar(t_("OUTPUT_BUBBLE_COLOUR_INDICATOR")),
      stateFilterId = scalar("colourIndicator")
    )
  )
  base_filter_ids <- lapply(c("detail", "area", "period", "sex", "age"), get_filter_from_id)
  list(
    defaultFilterTypes = c(indicators, base_filter_ids),
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
      "indicator" = "OUTPUT_FILTER_INDICATOR"
    )
  )
}
