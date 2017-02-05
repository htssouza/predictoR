################################################################################
# Test for the PredictorROutput Class
################################################################################

################################################################################
# External dependencies
################################################################################

library(data.table)
library(RUnit)

################################################################################
# Local dependencies
################################################################################

library(predictoR)

################################################################################
# Tests
################################################################################

test.PredictoROutput <- function() {
  params <- CreateSamplePredictoRParams()
  x1 <- PredictoROutput(params=params, fits=NULL, prediction=NULL)
  checkEquals(x1$params, params)
}

test.print.PredictoROutput <- function() {
  params <- CreateSamplePredictoRParams()
  x1 <- PredictoROutput(params=params, fits=NULL, prediction=NULL)
  print(x1)
}
