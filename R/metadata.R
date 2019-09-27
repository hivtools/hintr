do_plotting_metadata <- function(country) {
  metadata <- naomi::get_plotting_metadata(country)
  out <- lapply(unique(metadata$data_type), function(data_type) {
    data <- metadata[metadata$data_type == data_type, ]
    build_data_type_metadata(data)
  })
  names(out) <- unique(metadata$data_type)
  out
}

build_data_type_metadata <- function(metadata) {
  out <- lapply(unique(metadata$plot_type), function(plot_type) {
    data <- metadata[metadata$plot_type == plot_type, ]
    list(
      indicators = build_plot_type_metadata(data)
    )
  })
  names(out) <- unique(metadata$plot_type)
  out
}

build_plot_type_metadata <- function(metadata) {
  out <- lapply(unique(metadata$indicator), function(indicator) {
    data <- metadata[metadata$indicator == indicator, ]
    build_indicator_metadata(data)
  })
  names(out) <- unique(metadata$indicator)
  out
}

build_indicator_metadata <- function(metadata) {
  if(nrow(metadata) != 1) {
    stop("Expected only 1 row for indicator, data type, plot type combination.
Check each combination is unique in configuration.")
  }
  list(
    value_column = scalar(metadata$value_column),
    indicator_column = scalar(metadata$indicator_column),
    indicator_value = scalar(metadata$indicator_value),
    name = scalar(metadata$name),
    min = scalar(metadata$min),
    max = scalar(metadata$max),
    colour = scalar(metadata$colour),
    invert_scale = scalar(metadata$invert_scale)
  )
}
