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
