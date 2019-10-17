#' Get model options declarative UI from naomi and input data
#'
#' Get's UI template from Naomi and substitutes any params within the
#' template with real values.
#'
#' @param options_template Naomi template
#' @param shape Path to shape file
#' @param survey Path to survey file
#' @param programme Path to optional programme file
#' @param anc Path to optional ANC file
#'
#' @return The model options declarative JSON UI.
#' @keywords internal
do_endpoint_model_options <- function(options_template, shape, survey,
                                      programme, anc) {
  regions <- read_geojson_regions(shape)
  parent_region <- regions[!grepl(".", regions, fixed = TRUE)]
  area_level_options <- read_level_labels(shape)
  art_quarter_options <- NULL
  if (!is.null(programme)) {
    art_quarter_options <-
      naomi::quarter_year_labels(read_quarters(programme))
  }
  ## We will need these when we move to full spec of UI - leaving in for now
  survey_options <- read_surveys(survey)
  if (!is.null(anc)) {
    anc_quarter_options <- naomi::quarter_year_labels(read_quarters(anc))
    ## TODO: anc quarter options not used as a param list but they will
    ## be needed when full naomi template is available. See mrc-574
  }

  params <- list(
    area_scope_options = regions,
    area_scope_default = parent_region,
    area_level_options = area_level_options,
    art_t1_options = art_quarter_options,
    art_t2_options = art_quarter_options
  )
  build_json(options_template, params)
}


#' Buld JSON from template and a set of params
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
    glue::glue(options_template, .envir = param_env, .open = "<", .close = ">",
               .transformer = collapse_and_quote_transformer),
    error = function(e) {
      e$message <- paste0(
        "Failed to construct model options from template and params:\n",
        e$message)
      stop(e)
    }
  )
}

collapse_and_quote_transformer <- function(text, envir) {
  res <- get(text, envir = envir, inherits = FALSE)
  res <- glue::glue_collapse(res, sep = '", "')
  paste0('"', res, '"')
}
