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

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, ..., FUN.VALUE = logical(1))
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

iso_time_str <- function(time = Sys.time()) {
  strftime(time, "%Y%m%d-%H%M")
}

schema_root <- function() {
  system_file("schema")
}

is_error <- function(x) {
  inherits(x, "error")
}

is_porcelain_error <- function(e) {
  inherits(e, "porcelain_error")
}

list_to_data_frame <- function(x) {
  to_data_frame <- function(...) {
    rbind.data.frame(..., make.row.names = FALSE, stringsAsFactors = FALSE)
  }
  do.call(to_data_frame, x)
}

#' Take "notes" from DownloadSubmitRequest schema and transform
#' into single string for passing to naomi
#'
#' @param notes The notes to parse
#'
#' @return The notes as a single string.
#' @keywords internal
format_notes <- function(notes) {
  format_single <- function(note) {
    sprintf("%s\n%s\n%s\n", note$name, note$updated, note$note)
  }
  project_notes <- format_single(notes$project_notes)
  version_notes <- lapply(notes$version_notes, format_single)
  sprintf("Project notes:\n\n%s\nVersion notes:\n\n%s",
          project_notes,
          paste0(version_notes, collapse = "\n"))
}

file_exists <- function(file) {
  !is.null(file) && file.exists(file)
}

assert_files_exist <- function(files) {
  for (file in files) {
    if (!file_exists(file)) {
      stop(sprintf("File %s does not exist", file))
    }
  }
  invisible(TRUE)
}

assert_names <- function(items, required, optional,
                         name = deparse(substitute(items))) {
  missing <- !(required %in% names(items))
  if (any(missing)) {
    missing <- paste(required[missing], collapse = ", ")
    stop(sprintf("Required item(s) %s are missing from %s", missing, name))
  }
  additional <- !(names(items) %in% c(required, optional))
  if (any(additional)) {
    additional <- paste(names(items)[additional], collapse = ", ")
    stop(sprintf("Unknown item(s) %s are included in %s", additional, name))
  }
  invisible(TRUE)
}

r6_private <- function(x) {
  x[[".__enclos_env__"]]$private
}
