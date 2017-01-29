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
  cat("PredictoR:")
  cat("\n params: ")
  cat(capture.output(object$params))
  cat("\n")
}
