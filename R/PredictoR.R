################################################################################
# Predictor
################################################################################

################################################################################
# Functions
################################################################################

PredictoR <- function(x, ...) UseMethod("PredictoR")

PredictoR.default <- function(params) {
  this <- list()
  this$params <- params
  class(this) <- "PredictoR"
  return (this)
}

print.PredictoR <- function(object) {
  writeLines("PredictoR:")
  writeLines("params:")
  writeLines(capture.output(object$params))
}
