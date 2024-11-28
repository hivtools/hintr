population_pyramid_metadata <- function(data) {
  filter_types <- list(
    list(
      id = scalar("area"),
      column_id = scalar("area_id"),
      options = json_verbatim("null"),
      use_shape_regions = scalar(TRUE)
    ),
    list(
      id = scalar("area_level"),
      column_id = scalar("area_level"),
      options = json_verbatim("null"),
      use_shape_area_level = scalar(TRUE)
    ),
    list(
      id = scalar("calendar_quarter"),
      column_id = scalar("calendar_quarter"),
      options = get_quarter_filters(data, decreasing = FALSE)
    ),
    age_filter <- list(
      id = scalar("age"),
      column_id = scalar("age_group"),
      options = get_age_filters(data)
    ),
    sex_filter <- list(
      id = scalar("sex"),
      column_id = scalar("sex"),
      options = get_sex_filters(data)
    )
  )
  ## Use default here as country is only used for specific colours but
  ## population pyramid has the same colours for all countries
  indicator_metadata <- get_indicator_metadata("population", "pyramid",
                                               list(iso3 = "default"))
  control_options <- lapply(seq(nrow(indicator_metadata)), function(row_no) {
    list(
      id = scalar(indicator_metadata[row_no, "indicator"]),
      label = scalar(indicator_metadata[row_no, "name"]),
      effect = setNames(list(), list())
    )
  })
  list(
    filterTypes = filter_types,
    indicators = indicator_metadata,
    plotSettingsControl = list(
      population = list(
        defaultEffect = list(
          setFilters = list(
            list(
              filterId = scalar("area_level"),
              label = scalar(t_("OUTPUT_FILTER_AREA_LEVEL")),
              stateFilterId = scalar("area_level")
            ),
            list(
              filterId = scalar("area"),
              label = scalar(t_("OUTPUT_FILTER_AREA")),
              stateFilterId = scalar("area")
            ),
            list(
              filterId = scalar("calendar_quarter"),
              label = scalar(t_("OUTPUT_FILTER_PERIOD")),
              stateFilterId = scalar("calendar_quarter")
            )
          ),
          setMultiple = c("area")
        ),
        plotSettings = list(
          list(
            id = scalar("plot"),
            label = scalar(t_("INPUT_TIME_SERIES_COLUMN_PLOT_TYPE")),
            options = control_options
          )
        )
      )
    )
  )
}
