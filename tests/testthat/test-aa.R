context("aa")

local({
  files <- dir(system_file("schema"), full.names = TRUE)
 # jsonvalidate::json_validator("/home/rob/projects/hintr/inst/schema/Filter.schema.json", engine = "ajv")
  jsonvalidate::json_validator("/home/rob/projects/hintr/inst/schema/ModelResultResponse.schema.json", engine = "ajv")
  success <- c()
  failure <- c()
   for (f in files) {
    # withCallingHandlers({
   #   jsonvalidate::json_validator(f, engine = "ajv")
      # success <- c(success, f)
    # },
    # error = function(e) {
    #   print("failed")
    #   print(f)
    #   print(sys.calls())
    # })
  }
  print("successes")
  print(success)
  print("Failures")
  print(failure)
})
