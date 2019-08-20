run_model <- function(parameters) {
  if (!is.null(parameters$sleep)) {
    message("sleeping...")
    Sys.sleep(parameters$sleep)
  }
  2
}
