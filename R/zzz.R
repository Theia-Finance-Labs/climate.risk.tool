#' Package initialization
#'
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Declare global variables to avoid R CMD check warnings
  # This is needed for tidyverse/dplyr .data pronoun usage
  utils::globalVariables(".data")
}
