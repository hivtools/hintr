hintr_translator <- function() {
  traduire::translator(package = "hintr")
}

with_hintr_language <- function(language, expr) {
  reset <- traduire::translator_set_language(language, package = "hintr")
  on.exit(reset())
  force(expr)
}
