#' Get model options declarative UI from naomi and input data
#'
#' Get's UI template from Naomi and substitutes any params within the
#' template with real values.
#'
#' @param shape A file object (path, hash, filename) corresponding to
#'   the input shape file
#' @param survey Path to survey file
#' @param programme Path to optional programme file
#' @param anc Path to optional ANC file
#'
#' @return The model options declarative JSON UI.
#' @keywords internal
do_endpoint_model_options <- function(shape, survey, programme, anc) {
  has_art <- !is.null(programme)
  has_anc <- !is.null(anc)

  ## General options
  json <- hintr_geojson_read(shape)
  iso3 <- get_geojson_iso3(json)
  regions <- get_region_filters(json)
  parent_region_id <- regions$id
  area_level_options <- get_level_options(json)
  time_options <- get_time_options()

  survey_data <- read_csv(survey$path)
  survey_mid_calendar_quarter <- survey_data$survey_mid_calendar_quarter
  if (!is.null(survey_mid_calendar_quarter) &&
      all(grepl("CY[[:digit:]]{4}Q[[:digit:]]", survey_mid_calendar_quarter))) {
    most_recent_survey_quarter <- scalar(max(survey_mid_calendar_quarter))
    ## Union most_recent_survey_quarter with times_list to ensure it is
    ## included in options
    most_recent_survey_qid <- naomi::calendar_quarter_to_quarter_id(
      most_recent_survey_quarter)
    mr_qlist <- quarter_id_to_json_list(most_recent_survey_qid)
    time_options <- union_time_list(time_options, mr_qlist, decreasing = TRUE)
  } else {
    ## Use the most recent time option
    most_recent_survey_quarter <- time_options[[1]]$id
  }

  metadata <- naomi::get_metadata()
  survey_prevalence_options <- get_survey_options(
    survey_data, metadata, "prevalence")
  survey_art_coverage_options <- get_survey_options(
    survey_data, metadata, "art_coverage")
  survey_recently_infected_options <- get_survey_options(
    survey_data, metadata, "recent_infected")

  ## ART options
  art_year_options <- NULL
  if (has_art) {
    art_year_options <- get_year_filters(read_csv(programme$path))
  }

  ## ANC options
  anc_year_options <- NULL
  anc_year1_default <- scalar("")
  anc_year2_default <- scalar("")
  if (has_anc) {
    anc_data <- as.data.frame(naomi::read_anc_testing(anc$path))
    anc_years <- get_years(anc_data)
    anc_year_options <- lapply(anc_years, function(year) {
      list(id = scalar(as.character(year)),
           label = scalar(as.character(year)))
    })
    if (2022 %in% anc_years) {
      anc_year2_default <- scalar(as.character(2022))
    }
    survey_year <- naomi::calendar_quarter_to_year(most_recent_survey_quarter)
    if (survey_year %in% anc_years) {
      anc_year1_default <- scalar(as.character(survey_year))
    }
  }

  options <- list(
    area_scope = list(regions),
    area_level = area_level_options,
    calendar_quarter_t1 = time_options,
    calendar_quarter_t2 = time_options,
    survey_prevalence = survey_prevalence_options$options,
    survey_art_coverage = survey_art_coverage_options$options,
    survey_recently_infected = survey_recently_infected_options$options,
    anc_clients_year2 = anc_year_options,
    anc_prevalence_year1 = anc_year_options,
    anc_prevalence_year2 = anc_year_options,
    anc_art_coverage_year1 = anc_year_options,
    anc_art_coverage_year2 = anc_year_options,
    psnu_level = area_level_options
  )

  values <- list(
    area_scope = parent_region_id,
    calendar_quarter_t1 = most_recent_survey_quarter,
    survey_prevalence = survey_prevalence_options$default,
    survey_art_coverage = survey_art_coverage_options$default,
    anc_prevalence_year1 = anc_year1_default,
    anc_prevalence_year2 = anc_year2_default,
    anc_art_coverage_year1 = anc_year1_default,
    anc_art_coverage_year2 = anc_year2_default
  )

  additional_control_groups <- NULL
  if (use_mock_model()) {
    additional_control_groups <- list(
      list(
        label = "Trigger mock model error",
        controls = list(
          list(
            name = "mock_model_trigger_error",
            type = "select",
            help_text = "Set TRUE to force the model fit to error",
            required = TRUE,
            options = list(list(id = "true", label = "Yes"),
                           list(id = "false", label = "No")),
            value = "false"
          )
        )
      )
    )
  }

  options <- get_controls_json(
    "model", iso3, options, values,
    config = list(include_art = has_art,
                  include_anc = has_anc,
                  additional_control_groups = additional_control_groups))
  options
}

get_level_options <- function(json) {
  levels <- lapply(json$features, function(feature) {
    level <- NULL
    if (as.logical(feature$properties$display)) {
      level <- list(
        id = scalar(as.character(feature$properties$area_level)),
        label = scalar(feature$properties$area_level_label)
      )
    }
    level
  })
  unique(levels)
}

get_time_options <- function() {
  start_date <- naomi::convert_quarter_id(2010, 1)
  current_quarter <- substr(quarters(Sys.Date()), 2, 2)
  end_date <- naomi::convert_quarter_id(as.integer(format(Sys.Date(), "%Y")),
                                        as.integer(current_quarter))
  times <- seq.int(end_date, start_date, -1)
  quarter_id_to_json_list(times)
}

quarter_id_to_json_list <- function(times) {
  ids <- naomi::quarter_id_to_calendar_quarter(times)
  labels <- naomi::quarter_year_labels(times)
  format <- function(id, label) {
    list(id = scalar(id), label = scalar(label))
  }
  Map(format, ids, labels, USE.NAMES = FALSE)
}

sort_time_json_list <- function(time_list, decreasing = TRUE) {
  ids <- time_list_ids(time_list)
  time_list[order(ids, decreasing = decreasing)]
}

time_list_ids <- function(time_list) {
  unlist(lapply(time_list, "[[", 1))
}

union_time_list <- function(times1, times2, decreasing = TRUE) {
  ids1 <- time_list_ids(times1)
  ids2 <- time_list_ids(times2)

  newidx <- !(ids2 %in% ids1)
  times_new <- c(times1, times2[newidx])

  sort_time_json_list(times_new, decreasing)
}

## Survey options
## Have to use the metadata to work out where within the output data these
## values can be located
get_survey_options <- function(survey_data, metadata, indicator) {
  indicator_data <- get_indicator_data(survey_data, metadata, "survey",
                                       indicator)
  if (nrow(indicator_data) == 0) {
    ## Gets serialised to JSON and requires an obj
    ## for options NULL -> {}
    ## a string for default values
    return(list(
      options = NULL,
      default = scalar("")
    ))
  }
  options <- get_survey_filters(indicator_data)
  option_default <- scalar("")
  if (!is.null(indicator_data$survey_mid_calendar_quarter)) {
    indicator_data$year <- naomi::calendar_quarter_to_year(
      indicator_data$survey_mid_calendar_quarter)
    latest_year <- max(indicator_data$year)
    defaults <- indicator_data[indicator_data$year == max(indicator_data$year),
                               "survey_id"]
    if (length(defaults) >= 1) {
      option_default <- scalar(defaults[[1]])
    }
  }
  list(
    options = options,
    default = option_default)
}

get_years <- function(data) {
  years <- unique(data$year)
  sort(years, decreasing = TRUE)
}
