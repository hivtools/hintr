## Always validate schemas in tests
Sys.setenv("VALIDATE_JSON_SCHEMAS" = "true") # nolint
Sys.setenv("PORCELAIN_VALIDATE" = "true") # nolint

validate_baseline_input <- function(file_path, type) {
  sprintf(
    '{"type": "%s",
      "file": {
        "path": "%s",
        "hash": "12345",
        "filename": "original",
        "fromADR": false,
        "resource_url": "https://adr.unaids.org/file/123.csv"
      }
    }', type, file_path)
}

model_options_input <- function(shape, survey, programme, anc) {
  as_file_object <- function(x) {
    if (is.null(x)) {
      "null"
    } else {
      sprintf(
        '{
        "path": "%s",
        "hash": "12345",
        "filename": "original",
        "fromADR": false,
        "resource_url": "https://adr.unaids.org/file/123.csv"
      }', x)
    }
  }
  sprintf(
    '{"shape": %s,
      "survey": %s,
      "programme": %s,
      "anc": %s
    }',
    as_file_object(shape), as_file_object(survey),
    as_file_object(programme), as_file_object(anc)
  )
}

validate_baseline_all_input <- function(pjnz, shape, population) {
  quote <- function(x) {
    if (is.null(x)) {
      "null"
    } else {
      paste0('"', x, '"')
    }
  }
  sprintf(
    '{"pjnz": %s,
      "shape": %s,
      "population": %s
    }',
    quote(pjnz), quote(shape), quote(population)
  )
}

validate_programme_survey_input <- function(file_path, type, shape) {
  sprintf(
    '{"type": "%s",
      "file": {
        "path": "%s",
        "hash": "12345",
        "filename": "original",
        "fromADR": false,
        "resource_url": "https://adr.unaids.org/file/123.csv"
      },
      "shape": "%s"
    }', type, file_path, shape)
}

MockPlumberResponse <- R6::R6Class("PlumberResponse", list(
  body = NULL,
  status = 200,
  headers = list(),
  setHeader = function(name, value) {
    he <- list()
    he[[name]] <- value
    self$headers <- c(self$headers, he)
  },
  toResponse = function() {
    list(headers = self$headers, body = self$body)
  }
))
