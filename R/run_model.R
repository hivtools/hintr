#' Run the model
#'
#' This will prepare any data required by the model and call the model code.
#'
#' Expecting input data to be a list of paths
#' list(
#'   pjnz = "path/to/file",
#'   shape = "path/to/file",
#'   ...
#' )
#'
#' @param data Paths to different types of input data for the model.
#' @param parameters Any additional parameters for the model run.
#'
#' @return The job ID of the created model run.
#'
#' @keywords internal
run_model <- function(data, parameters) {
  ## TODO: Recover serialised data in redis from the supplied paths where
  ## possible and call the model with data and parameters and return result
  Sys.sleep(10)
  2
}
