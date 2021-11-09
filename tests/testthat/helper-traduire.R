hintr_translator <- function() {
  traduire::translator(package = "hintr")
}

with_hintr_language <- function(language, expr) {
  reset <- traduire::translator_set_language(language, package = "hintr")
  naomi_reset <- traduire::translator_set_language(language, package = "naomi")
  on.exit(reset(), add = TRUE)
  on.exit(naomi_reset(), add = TRUE)
  force(expr)
}
