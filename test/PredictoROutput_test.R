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

source("R/PredictoR.R")
source("test/PredictoRParams_test.R")

################################################################################
# Tests
################################################################################

test.PredictoROutput1 <- function() {
  params <- CreateSamplePredictoRParams()
  x1 <- PredictoROutput(params=params, fits=NULL, prediction=NULL)
  checkEquals(x1$params, params)
}

test.PredictoROutput2 <- function() {
  params <- CreateSamplePredictoRParams()
  x1 <- PredictoROutput(params=params, fits=NULL)
  checkEquals(x1$params, params)
}

test.print.PredictoROutput <- function() {
  params <- CreateSamplePredictoRParams()
  x1 <- PredictoROutput(params=params, fits=NULL, prediction=NULL)
  print(x1)
}
