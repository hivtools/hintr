free_port <- function(start, max_tries = 20) {
  force(start)
  force(max_tries)
  function() {
    port <- find_free_port(start, max_tries)
    start <<- start + 1
    port
  }
}

find_free_port <- function(start, max_tries = 20) {
  port <- seq(start, length.out = max_tries)
  for (p in port) {
    if (check_port(p)) {
      return(p)
    }
  }
  stop(sprintf("Did not find a free port between %d..%d",
               min(port), max(port)),
       call. = FALSE)
}

check_port <- function(port) {
  timeout <- 0.1
  con <- tryCatch(suppressWarnings(socketConnection(
    "localhost", port = port, timeout = timeout, open = "r")),
    error = function(e) NULL)
  if (is.null(con)) {
    return(TRUE)
  }
  close(con)
  FALSE
}

get_free_port <- free_port(9000)

# The callr package will create a supervised process with a finaliser
# that will clean up on garbage collection, so we don't need to
# explicitly clean up. However, it means that consecutive tests should
# not use the same port because a process might still be being cleaned
# up. So the default is to use an incrementing port number. This has
# proven more reliable in practice anyway. The approach above is the
# same used in vaultr.
hintr_server <- function(port = NULL, n_tries = 10, poll = 0.1) {
  skip_if_not_installed("callr")
  skip_if_not_installed("httr")

  port <- port %||% get_free_port()

  process <- callr::r_bg(function(port) hintr:::api(port),
                         args = list(port = port))
  url <- sprintf("http://localhost:%d", port)

  for (i in seq_len(n_tries)) {
    ok <- tryCatch({
      httr::stop_for_status(httr::GET(url))
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      return(list(process = process, url = url, port = port))
    }
    if (!process$is_alive()) {
      break
    }
    Sys.sleep(poll)
  }

  pr$kill()
  stop("Failed to start server")
}

response_to_json <- function(x) {
  jsonlite::fromJSON(httr::content(x, "text", encoding = "UTF-8"), FALSE)
}
