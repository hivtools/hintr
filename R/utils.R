system_file <- function(...) {
  system.file(..., mustWork = TRUE)
}

is_empty <- function(x) {
  is.null(x) || is.na(x) || length(x) == 0 || trimws(x) == ""
}
