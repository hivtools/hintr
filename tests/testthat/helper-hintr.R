with_mock <- function(..., .parent = parent.frame()) {
  mockr::with_mock(..., .parent = .parent, .env = "hintr")
}

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
