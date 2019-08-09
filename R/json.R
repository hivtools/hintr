validate_json_schema <- function(json, schema) {
  if (validate_schemas()) {
    valid <- validate(json, schema)
  } else {
    valid <- TRUE
  }
  valid
}

validate <- function(json, schema) {
  schema <- system.file("schema", paste0(schema, ".schema.json"),
                        package = "hintr")
  jsonvalidate::json_validate(json, schema, engine = "ajv")
}

validate_schemas <- function() {
  Sys.getenv("VALIDATE_JSON_SCHEMAS") == "true"
}

scalar <- function(val) {
  jsonlite::unbox(val)
}
