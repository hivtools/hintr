call_with_mocks_object <- function(code, mocks) {
    binded_code <- function(...) {
        with_mocked_bindings(code, ...)
    }
    do.call(binded_code, mocks)
}

get_mock_args_from_vector <- function(vector) {
  # mock args are lists of lists
  lapply(as.list(vector), list)
}

get_filter_mocks <- function() {
  list(
    get_filter_from_id = mock("filter_ref", cycle = TRUE),
    get_x_axis_or_disagg_by_option = mock("x_axis_or_disagg_by_option", cycle = TRUE)
  )
}

create_test_zip <- function() {
  t <- tempfile()
  dir.create(file.path(t), FALSE, TRUE)
  utils::write.csv(c(1, 2, 3), file.path(t, "myfile.csv"))
  zip_file <- tempfile()
  zip::zip(zip_file, "myfile.csv", root = t)
  readBin(zip_file, "raw", n = file.size(zip_file))
}

debug_download_response <- list(
  url = "https://example.com",
  status_code = 200,
  content = create_test_zip(),
  request = list(
    method = "GET",
    url = "https://example.com"
  )
)
class(debug_download_response) <- "response"

get_mock_debug_download <- function() {
  mockery::mock(
    debug_download_response,
    cycle = TRUE
  )
}
