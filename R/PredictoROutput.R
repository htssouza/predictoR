################################################################################
# PredictoR Output
################################################################################

################################################################################
# External dependencies
################################################################################

library(data.table)
library(logging)

################################################################################
# Functions
################################################################################

PredictoROutput <- function(x, ...) UseMethod("PredictoROutput")

PredictoROutput.PredictoRParams <- function(params, fits, prediction=NULL) {
  this <- list()
  this$params <- params
  this$fits <- fits
  this$prediction <- prediction
  class(this) <- "PredictoROutput"
  return (this)
}

print.PredictoROutput <- function(object) {
  writeLines("PredictoROutput:")
  writeLines("params:")
  writeLines(capture.output(object$params))
  writeLines("fits:")
  writeLines(capture.output(object$fits))
  writeLines("prediction:")
  writeLines(capture.output(object$prediction))
}
