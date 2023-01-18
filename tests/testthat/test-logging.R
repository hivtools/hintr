test_that("Can log verbosely", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  logger <- porcelain::porcelain_logger("trace", path = tmp)
  queue <- test_queue(workers = 0)
  api <- api_build(queue, logger = logger)
  res <- api$request("GET", "/")
  lapply(readLines(tmp), jsonlite::fromJSON)

  dat <- jsonlite::stream_in(file(tmp), verbose = FALSE)
  expect_equal(nrow(dat), 4)
  expect_equal(dat$logger, rep("hintr", 4))
})

test_that("Can log to console", {
  logger <- porcelain::porcelain_logger("trace")
  queue <- test_queue(workers = 0)
  api <- api_build(queue, logger = logger)
  output <- capture_output_lines(res <- api$request("GET", "/"))
  expect_equal(length(output), 4)
})
