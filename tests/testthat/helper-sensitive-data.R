skip_if_sensitive_data_missing <- function() {
  if (!file.exists(file.path("testdata", "sensitive"))) {
    skip("Sensitive data missing, check README for details.")
  }
}
