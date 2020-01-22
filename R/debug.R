##' Get debug output from naomi
##'
##' @title Get debug output from naomi
##'
##' @param id The model run id to download. This will be printed at
##'   the top of the stack trace.
##'
##' @param server The url of the server.  The default is to use the
##'   production naomi/hint/hintr instance. It is not possible to use
##'   the staging instance.  You can change this if running locally.
##'
##' @param dest The destination for the downloaded data.  The actual
##'   data will be unpacked into a directory corresponding to the run
##'   id within this, so it is safe to use a common directory.
##'
##' @param verbose Add a progress bar
##'
##' @export
download_debug <- function(id, server = NULL, dest = tempfile(),
                           verbose = TRUE) {
  if (is.null(server)) {
    server <- "http://naomi.dide.ic.ac.uk:8888"
  }
  if (file.exists(file.path(dest, id))) {
    stop(sprintf("Path '%s' already exists at destination '%s'", id, dest))
  }
  url <- sprintf("%s/model/debug/%s", server, id)
  progress <- if (verbose) httr::progress() else NULL
  r <- httr::GET(url, progress)
  httr::stop_for_status(r)

  zip <- tempfile(fileext = ".zip")
  on.exit(unlink(zip))
  writeBin(httr::content(r, "raw"), zip)
  dir.create(dest, FALSE, TRUE)
  zip::unzip(zip, exdir = dest)

  file.path(dest, id)
}
