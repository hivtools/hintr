skip_if_sensitive_data_missing <- function() {
  skip_if_not(file.exists(file.path("testdata", "sensitive")),
              "Sensitive data missing, check README for details.")
}
