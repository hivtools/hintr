validate_json_schema <- function(data, schema) {
  if (validate_schemas()) {
    ## Get schema as file path
    ## TODO: Use jsonvalidate once referenced files are supported
    valid <- TRUE
  } else {
    valid <- TRUE
  }
  valid
}

validate_schemas <- function() {
  Sys.getenv("VALIDATE_JSON_SCHEMAS") == "true"
}

scalar <- function(val) {
  jsonlite::unbox(val)
}
