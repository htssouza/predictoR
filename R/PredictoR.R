################################################################################
# Predictor
################################################################################

################################################################################
# Local dependencies
################################################################################

source ("R/PredictoRParams.R")

################################################################################
# Functions
################################################################################

PredictoR <- function(x, ...) UseMethod("PredictoR")

PredictoR.PredictoRParams <- function(params) {
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

Predict.Predictor <- function(object) {

}
