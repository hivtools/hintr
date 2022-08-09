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

  data_defaults <- get_data_defaults(shape, survey, programme, anc)
  hardcoded_defaults <- get_hardcoded_defaults(data_defaults$area_scope$value)

  defaults <- verify_option_defaults(data_defaults, hardcoded_defaults,
                                     options_stitched)
  params <- unlist(lapply(names(defaults), function(name) {
    x <- list()
    x[[paste0(name, "_default")]] <- defaults[[name]]$default
    if (!is.null(defaults[[name]]$options)) {
      x[[paste0(name, "_options")]] <- defaults[[name]]$options
    }
    x
  }), recursive = FALSE)
  build_json(options_stitched, params)
}

get_data_defaults <- function(shape, survey, programme, anc) {
  has_art <- !is.null(programme)
  has_anc <- !is.null(anc)
  json <- hintr_geojson_read(shape)
  regions <- get_region_filters(json)
  country_region_id <- read_geojson_iso3(shape)
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
    mr_qlist <- list(quarter_id_to_json_list(most_recent_survey_qid))
    time_options <- union_time_list(time_options, mr_qlist, decreasing = TRUE)
  } else {
    ## Use the most recent time option
    most_recent_survey_quarter <- time_options[[1]]$id
  }

  survey_prevalence_options <- get_survey_options(survey, "prevalence")
  survey_art_coverage_options <- get_survey_options(survey, "art_coverage")
  survey_recently_infected_options <- get_survey_options(survey,
                                                         "recent_infected")

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
    anc_data <- read_csv(anc$path)
    anc_years <- get_years(anc_data)
    anc_year_options <- lapply(anc_years, function(year) {
      list(id = scalar(as.character(year)),
           label = scalar(as.character(year)))
    })
    if (2021 %in% anc_years) {
      anc_year2_default <- scalar(as.character(2021))
    }
    survey_year <- naomi::calendar_quarter_to_year(most_recent_survey_quarter)
    if (survey_year %in% anc_years) {
      anc_year1_default <- scalar(as.character(survey_year))
    }
  }

  list(
    area_scope = list(
      options = list(regions),
      value = country_region_id
    ),
    area_level = list(
      options = area_level_options,
      value = area_level_options[[length(area_level_options)]]$id
    ),
    calendar_quarter_t1 = list(
      options = time_options,
      value = most_recent_survey_quarter
    ),
    calendar_quarter_t2 = list(
      options = time_options
    ),
    survey_prevalence = survey_prevalence_options,
    survey_art_coverage = survey_art_coverage_options,
    survey_recently_infected = survey_recently_infected_options,
    anc_prevalence_year1 = list(
      options = anc_year_options,
      value = anc_year1_default
    ),
    anc_prevalence_year2 = list(
      options = anc_year_options,
      value = anc_year2_default
    ),
    anc_art_coverage_year1 = list(
      options = anc_year_options,
      value = anc_year1_default
    ),
    anc_art_coverage_year2 = list(
      options = anc_year_options,
      value = anc_year2_default
    )
  )
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
    glue::glue(options_template, .envir = param_env, .open = '"<+',
               .close = '+>"', .transformer = json_transformer),
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

## Survey options
## Have to use the metadata to work out where within the output data these
## values can be located
get_survey_options <- function(survey, indicator) {
  indicator_data <- get_indicator_data(survey, "survey", indicator)
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
    value = option_default)
}

get_years <- function(data) {
  years <- unique(data$year)
  sort(years, decreasing = TRUE)
}

get_hardcoded_defaults <- function(iso3) {
  defaults <- naomi::get_country_option_defaults()
  iso3 <- toupper(iso3)
  if (!(iso3 %in% colnames(defaults))) {
    return(list())
  }
  to_option <- function(option) {
    list(
      value = scalar(option)
    )
  }
  lapply(setNames(defaults[, iso3], defaults[, "model_options"]),
         to_option)
}



## Defaults have names which match the name of the control in the options JSON
## But the default value in the template is <name>_default
## We need to check that the default from the csv
## Merge 2 lists, if name is present in both uses the value from the primary
verify_option_defaults <- function(data_defaults, hardcoded_defaults,
                                   options_json) {
  ## options come from data or template
  ## values come from data or hardcoded
  ## prefer hardcoded options
  ## Merge all 3 data, if a value in both data and hardcoded, prefer the hardcoded
  ## If a set of options in both data and template then error, that shouldn't happen
  ## Merge to list(option_name: list(options, data_value, hardcoded_value))
  ## Check hardcoded from options if so use that, if not fallback to data
  ## if not fallback to no value (do we need to update template for this?)

  all_options <- jsonlite::fromJSON(options_json, simplifyVector = FALSE)
  controls <- list()
  for (section in all_options$controlSections) {
    for (group in section$controlGroups) {
      for (control in group$controls) {
        controls <- c(controls, setNames(list(control), control$name))
      }
    }
  }

  all_names <- union(names(data_defaults), names(hardcoded_defaults))
  # if (!all(names(controls) %in% all_names)) {
  #   missing <- names(controls)[!(names(controls) %in% all_names)]
  #   stop(t_("All controls must have a default value, control %s missing value. Contact system admin.")
  # }

  defaults <- lapply(setNames(all_names, all_names), verify_default, controls,
                     data_defaults, hardcoded_defaults)
}

is_templated <- function(x) grepl('^<\\+\\w+\\+>$', x)

##' Select default value from hardcoded and input data and verify it
##'
##' Verify that (if set) hardcoded default value is in the set of possible
##' options otherwise fallback to data default otherwise fallback to no
##' selection
##'
##' @param name Name of the control to verify
##' @param controls Named list of controls from the options JSON
##' @param merged_defaults List containg merge of options from data and
##'   hardcoding in naomi.
##'
##' @return A named list of list containing any options and default values
##'   for controls.
##' @keywords internal
verify_default <- function(name, controls, data_defaults, hardcoded_defaults) {
  opts <- controls[[name]]$options
  if (isTRUE(is_templated(controls[[name]]$options))) {
    ## Options are templated so get get options from data
    opts <- vcapply(data_defaults[[name]]$options, "[[", "id")
  }
  if (!is.null(opts)) {
    is_valid <- function(x) !is_empty(x) && x %in% opts
  } else {
    is_valid <- function(x) !is_empty(x)
  }

  hardcoded_default <- hardcoded_defaults[[name]]$value
  data_default <- data_defaults[[name]]$value
  if (is_valid(hardcoded_default)) {
    default <- hardcoded_default
  } else if (is_valid(data_default)) {
    default <- data_default
  } else {
    default <- scalar("")
  }
  ret <- list(
    default = default
  )
  if (!is.null(opts)) {
    ret$options <- data_defaults[[name]]$options
  }
  ret
}
