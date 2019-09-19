system_file <- function(...) {
  tryCatch({
    system.file(..., mustWork = TRUE)
  }, error = function(e) {
    stop(sprintf("Failed to locate file from args\n%s",
                 paste(list(...), collapse = " ")))
  })
}

read_csv <- function(...) {
  utils::read.csv(..., stringsAsFactors = FALSE)
}

is_empty <- function(x) {
  is.null(x) || is.na(x) || length(x) == 0 || trimws(x) == ""
}

read_string <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

#' Collapse a vector into a human readable format for returning in messages
#'
#' Limits the returned message based on a character maximum.
#'
#' @param vector The vector to collapse
#' @param collapse Separator between each element after collapse
#' @param limit Max character length for message
#' @param end If length of message is over the limit this string will be
#' appended to the end
#'
#' @return The collapsed vector as a string.
#' @keywords internal
collapse <- function(vector, collapse = ", ", limit = 150, end = "...") {
  width <- nchar(vector)
  width[-1] <- width[-1] + nchar(collapse)
  too_long <- cumsum(width) >= limit
  if (any(too_long)) {
    vector <- vector[!too_long]
    if (!is.null(end) && end != "") {
      vector <- c(vector, end)
    }
  }
  out <- paste(vector, collapse = collapse)
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}
