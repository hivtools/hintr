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
#' @param vector The vector to collapse
#' @param collapse Separator between each element after collapse
#' @param limit Max number of items from vector to return in string
#' @param end If length of vector over the max a string to append to the end
#'
#' @return The collapsed vector as a string.
#' @keywords internal
collapse <- function(vector, collapse = ", ", limit = 10, end = ", ...") {
  truncated <- FALSE
  if (length(vector) > limit) {
    vector <- vector[seq_len(limit)]
    truncated <- TRUE
  }
  out <- paste0(vector, collapse = collapse)
  if (!is.null(end) && truncated) {
    out <- paste(out, end, sep = "")
  }
  out
}
