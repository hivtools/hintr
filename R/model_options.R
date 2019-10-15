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
  params <- list()
  regions <- read_geojson_regions(shape)
  params$area_scope_options <- regions
  parent_region <- regions[!grepl("\\.", regions)]
  if (length(parent_region) != 1) {
    stop(sprintf("Should have located one parent regions but found regions %s.",
                 collapse(regions)))
  }
  params$area_scope_default <- parent_region
  params$area_level_options <- read_level_labels(shape)
  if (!is.null(programme)) {
    art_quarter_options <-
      naomi::quarter_year_labels(read_quarters(programme))
    params$art_t1_options <- art_quarter_options
    params$art_t2_options <- art_quarter_options
  }
  ## We will need these when we move to full spec of UI - leaving in for now
  survey_options <- read_surveys(survey)
  if (!is.null(anc)) {
    anc_quarter_options <- naomi::quarter_year_labels(read_quarters(anc))
    ## TODO: write these into appropriate part of naomi template when avaialble
  }

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
  res <- eval(parse(text = text, keep.source = FALSE), envir)
  res <- glue::glue_collapse(res, sep = '", "')
  paste0('"', res, '"')
}
