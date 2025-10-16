##' Get debug output from naomi
##'
##' This uses GitHub authentication to pull the debug file. You need to
##'   1. Make sure you are a member of the naomi-debug GitHub team
##'   2. Generate a GitHub personal access token. Create a new fine-grained
##'   token. Set the resource owner to "hivtools" organisation. Under
##'   "Permissions" add "Members" read-only permission. And generate the token.
##'   3. Pass the token to this function, fill in the interactive prompt or
##'   set as an environment variable "GITHUB_DEBUG_DOWNLOAD_TOKEN".
##'
##' @title Get debug output from naomi
##'
##' @param id The model run id to download. This will be printed below the
##'   error message.
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
download_debug <- function(
    id,
    server = NULL,
    dest = tempfile(),
    github_token = Sys.getenv("GITHUB_DEBUG_DOWNLOAD_TOKEN"),
    verbose = TRUE) {
  if (is.null(server)) {
    server <- "http://naomi.unaids.org"
  }
  if (file.exists(file.path(dest, id))) {
    stop(sprintf("Path '%s' already exists at destination '%s'", id, dest))
  }
  url <- sprintf("%s/model/debug/%s", server, id)
  progress <- if (verbose) httr::progress() else NULL
  if (github_token == "" || is.null(github_token) || is.na(github_token)) {
    if (requireNamespace("askpass", quietly = TRUE)) {
      github_token <- askpass::askpass("Enter GitHub token:")
    } else {
      stop(paste(
        "GitHub token not found. Please set env var",
        "GITHUB_DEBUG_DOWNLOAD_TOKEN or install the 'askpass' package",
        "to be prompted for input."
      ))
    }
  }
  r <- httr::GET(url,
                 progress,
                 httr::add_headers(Authorization = paste("Bearer", github_token)))
  httr::stop_for_status(r)

  zip <- tempfile(fileext = ".zip")
  on.exit(unlink(zip))
  writeBin(httr::content(r, "raw"), zip)
  dir.create(dest, FALSE, TRUE)
  zip::unzip(zip, exdir = dest)

  file.path(dest, id)
}

# In a separate function so we can mock it in tests
httr_get <- function(url, progress, github_token) {
  r <- httr::GET(
    url,
    progress,
    httr::add_headers(Authorization = paste("Bearer", github_token)))
  httr::stop_for_status(r)
  r
}
