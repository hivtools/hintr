test_redis_available <- function() {
  available <- redux::redis_available()
  if (!available) {
    stop("Skipping test as redis is not available")
  }
  invisible(available)
}

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

hintr_server <- function(n_tries = 15, poll = 1, results_dir = tempdir()) {
  test_redis_available()

  queue_id <- paste0("hintr:", ids::random_id())
  port <- get_free_port()
  process <- callr::r_bg(
    function(port, queue_id, results_dir) {
      hintr:::api(port = port, queue_id = queue_id, results_dir = results_dir)
    },
    args = list(port = port, queue_id = queue_id, results_dir = results_dir))
  url <- sprintf("http://localhost:%d", port)

  for (i in seq_len(n_tries)) {
    message("Attempt ", i)
    ok <- tryCatch({
      httr::stop_for_status(httr::GET(url))
      TRUE
    }, error = function(e) FALSE)
    if (ok) {
      withr::defer_parent(process$kill())
      return(list(process = process, url = url, port = port,
                  queue_id = queue_id))
    }
    if (!process$is_alive()) {
      break
    }
    Sys.sleep(poll)
  }

  message("Is API process alive?")
  message(process$is_alive())
  message("API output:")
  tryCatch(
    message(process$read_output()), error = function(e) message("FAILED: ", e$message))
  message("API error:")
  tryCatch(
    message(process$read_error()), error = function(e) message("FAILED: ", e$message))
  process$kill()
  stop("Failed to start server")
}

setup_submit_payload <- function(version = NULL, include_anc_art = TRUE) {
  path <- tempfile()
  if (is.null(version)) {
    version <- hintr:::to_json(hintr:::cfg$version_info)
  }
  if (include_anc_art) {
    payload <- readLines("payload/model_submit_payload.json")
  } else {
    payload <- readLines("payload/model_submit_payload_minimal.json")
  }
  payload <- gsub("<version_info>", version, payload, fixed = TRUE)
  writeLines(payload, path)
  path
}

response_from_json <- function(x) {
  jsonlite::parse_json(httr::content(x, "text", encoding = "UTF-8"))
}

setwd("tests/testthat/")
server <- hintr_server()
payload <- setup_submit_payload()

## Run a model
r <- httr::POST(paste0(server$url, "/model/submit"),
                body = httr::upload_file(payload, type = "application/json"),
                encode = "json")
testthat::expect_equal(httr::status_code(r), 200)
response <- response_from_json(r)

## Wait for it to complete
testthat::try_again(35, {
  Sys.sleep(5)
  r <- httr::GET(paste0(server$url, "/model/status/", response$data$id))
  testthat::expect_equal(httr::status_code(r), 200)
  response <- response_from_json(r)
  testthat::expect_equal(response$status, "success")
  testthat::expect_equal(response$data$status, "COMPLETE")
})

## Start the download
r <- httr::GET(paste0(server$url, "/download/submit/spectrum/",
                      response$data$id))
response <- response_from_json(r)
testthat::expect_equal(httr::status_code(r), 200)

## Get download status
testthat::try_again(5, {
  Sys.sleep(5)
  status_res <- httr::GET(paste0(server$url, "/download/status/",
                                 response$data$id))
  testthat::expect_equal(httr::status_code(status_res), 200)
  status <- response_from_json(status_res)
  testthat::expect_equal(status$status, "success")
  testthat::expect_equal(status$data$done, TRUE)
  testthat::expect_equal(status$data$status, "COMPLETE")
})

## Get headers
headers <- httr::HEAD(paste0(server$url, "/download/result/",
                             response$data$id))
testthat::expect_equal(httr::status_code(headers), 200)
testthat::expect_equal(httr::headers(headers)$`content-type`,
             "application/octet-stream")
testthat::expect_match(httr::headers(headers)$`content-disposition`,
             'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')

size <- length(httr::content(headers))
testthat::expect_null(httr::headers(headers)$`content-length`)
testthat::expect_equal(size, 0)

## Can stream bytes
res <- httr::GET(paste0(server$url, "/download/result/", response$data$id))
testthat::expect_equal(httr::headers(res)$`content-type`, "application/octet-stream")
testthat::expect_match(httr::headers(res)$`content-disposition`,
             'attachment; filename="MWI_naomi-output_\\d+-\\d+.zip"')

size <- length(httr::content(res))
content_length <- as.numeric(httr::headers(res)$`content-length`)
testthat::expect_equal(size, content_length)
## It contains some content, won't be same length as precomputed
## model output as this is generated before calibration
testthat::expect_true(size > 100000)
