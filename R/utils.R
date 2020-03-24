system_file <- function(...) {
  tryCatch({
    system.file(..., mustWork = TRUE, package = "hintr")
  }, error = function(e) {
    stop(sprintf("Failed to locate file from args\n%s",
                 paste(list(...), collapse = " ")))
  })
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

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, ..., FUN.VALUE = character(1))
}

# This utility will not be needed once all endpoints use file objects
# rather than paths, at which point this could be moved into the test
# helpers.
file_object <- function(path) {
  list(path = path,
       filename = path,
       hash = unname(tools::md5sum(path)))
}

file_copy <- function(from, to) {
  ok <- file.copy(from, to)
  err <- from[!ok]
  if (length(err) > 0L) {
    stop(sprintf("Copying %s failed", paste(err, collapse = ", ")))
  }
  invisible(ok)
}

# Run a command with a rate limiter - this is used to throttle the
# cleanup check.
throttle <- function(f, every) {
  last <- Sys.time() - every
  function() {
    now <- Sys.time()
    if (now - every > last) {
      last <<- now
      f()
    }
  }
}

no_error <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

read_binary <- function(path) {
  readBin(path, "raw", n = file.size(path))
}

iso_time_str <- function(time = Sys.time()) {
  strftime(time, "%Y%m%d-%H%M%S")
}
