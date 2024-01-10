# nocov start
# These are test helpers so skipping coverage checks, they should not
# be used by anything in the package

#' Build JSON from template and a set of params
#'
#' @param template Template JSON of payload
#' @param params List of named key value pairs for substituting from template.
#'
#' @return JSON built from template and params.
#' @keywords internal
#'
build_json <- function(template, params) {
  param_env <- list2env(params, parent = .GlobalEnv)
  glue::glue(template, .envir = param_env, .open = '"<+',
             .close = '+>"', .transformer = json_transformer)
}

json_transformer <- function(text, envir) {
  res <- get(text, envir = envir, inherits = FALSE)
  to_json(res)
}
# nocov end

setup_payload_submit <- function(version = NULL,
                                 include_anc_art = TRUE) {
  if (is.null(version)) {
    version <- to_json(cfg$version_info)
  }
  if (include_anc_art) {
    payload <- readLines(
      system_file("payload", "model_submit_payload.json"))
  } else {
    payload <- readLines(
      system_file("payload", "model_submit_payload_minimal.json"))
  }
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      version_info = version
    ))
}

setup_payload_calibrate <- function(version = NULL) {
  if (is.null(version)) {
    version <- to_json(cfg$version_info)
  }
  payload <- readLines(system_file("payload", "model_calibrate_payload.json"))
  build_json(
    paste0(payload, collapse = "\n"),
    list(
      version_info = version
    ))
}

setup_payload_download_request <- function(version = NULL,
                                           include_notes = TRUE,
                                           include_state = TRUE,
                                           include_pjnz = FALSE) {
  if (!any(include_notes, include_state, include_pjnz)) {
    stop("Must include one or more of notes, state or pjnz in payload")
  }
  payload <- list()
  path <- tempfile()
  if (include_notes) {
    notes <- paste0(readLines(
      system_file("payload", "spectrum_download_notes_payload.json")),
      collapse = "\n")
    payload <- c(
      payload,
      paste('"notes":', notes))
  }
  if (include_state) {
    if (is.null(version)) {
      version <- to_json(cfg$version_info)
    }
    state_payload <- readLines(
      system_file("payload", "spectrum_download_state_payload.json"))
    state_payload <- build_json(
      paste0(state_payload, collapse = "\n"),
      list(
        version_info = version
      )
    )
    payload <- c(payload,
                 paste('"state":', state_payload))
  }
  if (include_pjnz) {
    pjnz <- jsonlite::read_json(
      system_file("payload", "model_submit_payload.json"))$data$pjnz
    payload <- c(payload, paste(
      '"pjnz": ', jsonlite::toJSON(pjnz, auto_unbox = TRUE, null = "null")))
  }
  payload <- paste(payload, collapse = ",\n")
  payload <- paste("{\n", payload, "\n}")
  to_json(json_verbatim(payload))
}

#' Build JSON from template and a set of params
#'
#' @param naomi_output Calibrated naomi output
#'
#' @return Calibrated naomi output matched to MWI test data on `naomi.resources` to be used to generate the agyw tool.
#' @keywords internal
make_agyw_testfiles <- function(naomi_output){

  # Create naomi outputs align with testing data in naomi.resources:
  #   - Change iso3 to "MWI_demo"
  #   - Restrict outputs to admin2
  output <- naomi::read_hintr_output(naomi_output$model_output_path)

  # Areas
  meta_area_demo <- output$output_package$meta_area
  meta_area_demo[meta_area_demo$area_id == "MWI", "area_id"] = "MWI_demo"
  parent_area_id_replace <- meta_area_demo$parent_area_id == "MWI" &
    !is.na(meta_area_demo$parent_area_id)
  meta_area_demo[parent_area_id_replace, "parent_area_id"] = "MWI_demo"
  meta_area_demo <- meta_area_demo[meta_area_demo$area_level <= 2, ]

  # Indicators
  ind_demo <- output$output_package$indicators
  ind_replace <- ind_demo$area_id == "MWI" & !is.na(ind_demo$area_id)
  ind_demo[ind_replace, "area_id"] <- "MWI_demo"
  ind_demo <- ind_demo[ind_demo$area_id %in% meta_area_demo$area_id, ]

  # Options
  options_demo <- output$output_package$fit$model_options
  options_demo$area_scope <- "MWI_demo"
  options_demo$area_level <- 2

  # Save out demo output package
  demo <- output
  demo$output_package$indicators <- ind_demo
  demo$output_package$fit$model_options <- options_demo
  demo$output_package$meta_area <- meta_area_demo

  out_demo <- tempfile(fileext = ".qs")
  naomi:::hintr_save(demo, out_demo)

  # Add to existing hintr_test data
  naomi_output$model_output_path <- out_demo

  naomi_output
}
