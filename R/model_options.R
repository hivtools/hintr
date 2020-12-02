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
  options_template <- naomi::get_model_options_template(has_art, has_anc)
  options_stitched <- build_options_from_template(options_template)

  ## General options
  json <- hintr_geojson_read(shape)
  regions <- get_region_filters(json)
  parent_region_id <- regions$id
  area_level_options <- get_level_options(json)
  time_options <- get_time_options()

  ## Survey options
  ## Have to use the metadata to work out where within the output data these
  ## values can be located
  get_survey_options <- function(indicator) {
    get_survey_filters(get_indicator_data(survey, "survey", indicator))
  }

  survey_data <- read_csv(survey$path)
  survey_mid_calendar_quarter <- survey_data$survey_mid_calendar_quarter
  if (!is.null(survey_mid_calendar_quarter) &&
      all(grepl("CY[[:digit:]]{4}Q[[:digit:]]", survey_mid_calendar_quarter))) {
    most_recent_survey_quarter <- scalar(max(survey_mid_calendar_quarter))
    ## Union most_recent_survey_quarter with times_list to ensure it is
    ## included in options
    most_recent_survey_qid <- naomi::calendar_quarter_to_quarter_id(
      most_recent_survey_quarter)
    mr_qlist <- list(quarter_id_to_json_list(most_recent_survey_qid))
    time_options <- union_time_list(time_options, mr_qlist, decreasing = TRUE)
  } else {
    ## Use the most recent time option
    most_recent_survey_quarter <- time_options[[1]]$id
  }

  survey_prevalence_options <- get_survey_options("prevalence")
  survey_art_coverage_options <- get_survey_options("art_coverage")
  survey_recently_infected_options <- get_survey_options("recent_infected")

  ## ART options
  art_year_options <- NULL
  if (has_art) {
    art_year_options <- get_year_filters(read_csv(programme$path))
  }

  ## ANC options
  anc_year_options <- NULL
  if (has_anc) {
    anc_year_options <- get_year_filters(read_csv(anc$path))
  }

  params <- list(
    area_scope_options = list(regions),
    area_scope_default = parent_region_id,
    area_level_options = area_level_options,
    area_level_default = area_level_options[[length(area_level_options)]]$id,
    calendar_quarter_t1_options = time_options,
    calendar_quarter_t1_default = most_recent_survey_quarter,
    calendar_quarter_t2_options = time_options,
    survey_prevalence_options = survey_prevalence_options,
    survey_art_coverage_options = survey_art_coverage_options,
    survey_recently_infected_options = survey_recently_infected_options,
    anc_prevalence_year1_options = anc_year_options,
    anc_prevalence_year2_options = anc_year_options,
    anc_art_coverage_year1_options = anc_year_options,
    anc_art_coverage_year2_options = anc_year_options
  )
  build_json(options_stitched, params)
}


#' Build JSON from template and a set of params
#'
#' This wraps params in quotes and collapses any arrays into a single comma
#' separated list. Therefore only substitutes in string types for the time
#' being.
#'
#' @param options_template Template JSON of model run options
#' @param params List of named key value pairs for substituting from template.
#'
#' @return JSON built from template and params.
#' @keywords internal
#'
build_json <- function(options_template, params) {
  param_env <- list2env(params, parent = .GlobalEnv)
  tryCatch(
    glue::glue(options_template, .envir = param_env, .open = "<+", .close = "+>",
               .transformer = json_transformer),
    error = function(e) {
      e$message <- t_("MODEL_OPTIONS_FAIL", list(message = e$message))
      stop(e)
    }
  )
}

json_transformer <- function(text, envir) {
  res <- get(text, envir = envir, inherits = FALSE)
  to_json(res)
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
  lapply(times, quarter_id_to_json_list)
}

quarter_id_to_json_list <- function(time) {
  list(
    id = scalar(naomi::quarter_id_to_calendar_quarter(time)),
    label = scalar(naomi::quarter_year_labels(time))
  )
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


#' Stitch together separate sections of the options template
#'
#' @param options_template List of separate options sections
#'
#' @return The stiched together options template
#' @keywords internal
build_options_from_template <- function(options_template) {
  paste('{"controlSections": [ ',
        paste(options_template, collapse = ", ")
        , ']}', collapse = "")
}

#' Validate model options
#'
#' @param data The set of input data for the model run
#' @param options Key value list of model options
#'
#' @return TRUE if valid else throws an error
#' @keywords internal
do_validate_model_options <- function(data, options) {
  list(
    valid = scalar(naomi:::validate_model_options(data, options))
  )
}
