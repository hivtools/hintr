system_file <- function(...) {
  tryCatch({
    system.file(..., mustWork = TRUE)
  }, error = function(e) {
    stop(sprintf("Failed to locate file from args\n%s",
                 paste(list(...), collapse = " ")))
  })
}

is_empty <- function(x) {
  is.null(x) || is.na(x) || length(x) == 0 || trimws(x) == ""
}
