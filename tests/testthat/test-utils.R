test_that("can check for empty", {
  expect_true(is_empty(NULL))
  expect_true(is_empty(NA))
  expect_true(is_empty(character(0)))
  expect_true(is_empty(""))
  expect_true(is_empty("   "))
  expect_false(is_empty("test"))
})

test_that("system file returns useful message when file cannot be located ", {
  args <- list("testdata", "missing_file.txt")
  expect_error(system_file("testdata", "missing_file.txt"),
"Failed to locate file from args
testdata missing_file.txt")
})

test_that("collapse prepares vector for printing", {
  test_vector <- c("one", "two", "three", "four", "five", "six")
  expect_equal(collapse(test_vector),
               "one, two, three, four, five, six")
  expect_equal(collapse(test_vector, limit = 27),
               "one, two, three, four, ...")
  expect_equal(collapse(test_vector, limit = 28),
               "one, two, three, four, five, ...")
  expect_equal(collapse(test_vector, limit = 25, end = NULL),
               "one, two, three, four")
  expect_equal(collapse(test_vector, limit = 40),
               "one, two, three, four, five, six")
  expect_equal(collapse(test_vector, limit = 25, end = "etc."),
               "one, two, three, four, etc.")
  expect_equal(collapse(test_vector, collapse = " and ", limit = 13, end = NULL),
               "one and two")
  expect_equal(collapse(test_vector, limit = 1), "...")
})

test_that("file_copy reports error on failure", {
  expect_error(file_copy(tempfile(), tempfile()),
               "Copying .* failed")
})

test_that("throttle does not call functions very often", {
  f <- mockery::mock(1, 2)
  g <- throttle(f, 0.5)
  expect_equal(g(), 1)
  expect_null(g())
  mockery::expect_called(f, 1)
  Sys.sleep(0.6)
  expect_equal(g(), 2)
  mockery::expect_called(f, 2)
})

test_that("no_error swallows all errors", {
  f <- function(x) {
    if (x < 0) {
      stop("expected positive x")
    }
  }
  expect_silent(no_error(f(1)))
  expect_silent(no_error(f(-1)))
})

test_that("can convert list to data frame", {
  x <- list(
    list(x = 3, y = 5, z = "example"),
    list(x = 4, y = 2, z = "data"),
    list(x = 8, y = 3, z = "frame")
  )
  expect_equal(list_to_data_frame(x),
               data.frame(x = c(3, 4, 8),
                          y = c(5, 2, 3),
                          z = c("example", "data", "frame")))
})

test_that("notes can be formatted", {
  notes <- jsonlite::fromJSON(setup_payload_download_request(),
                              simplifyVector = FALSE)
  formatted <- format_notes(notes$notes)
  expect_equal(formatted, paste0(c(
    "Project notes:",
    "",
    "My project 123",
    "2022/05/17 12:34:21",
    "These are my project notes",
    "",
    "Version notes:",
    "",
    "Version 2",
    "2022/05/17 12:34:21",
    "Notes specific to this version",
    "",
    "Version 1",
    "2022/05/14 09:12:54",
    "Notes from the first version",
    ""), collapse = "\n"))
})

test_that("assert_files_exist", {
  t1 <- tempfile()
  file.create(t1)
  t2 <- tempfile()
  file.create(t2)
  expect_true(assert_files_exist(c(t1, t2)))
  expect_error(assert_files_exist(c(t1, "not a file")),
               "File not a file does not exist")
})

test_that("assert_names", {
  required <- c("one", "two")
  optional <- c("three", "four")
  input <- list(one = 1, two = 2, three = 3, four = 4)
  expect_true(assert_names(input, required, optional))

  input <- list(one = 1, two = 2)
  expect_true(assert_names(input, required, optional))

  input <- list(one = 1, three = 3, four = 4)
  expect_error(assert_names(input, required, optional),
               "Required item(s) two are missing from input",
               fixed = TRUE)

  input <- list(one = 1, two = 2, five = 5)
  expect_error(assert_names(input, required, optional),
               "Unknown item(s) five are included in input",
               fixed = TRUE)
})
