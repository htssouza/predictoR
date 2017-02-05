################################################################################
# Test for the PredictorR Class
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
source("test/PredictoRParams_test.R")

################################################################################
# Tests
################################################################################

test.PredictoR <- function() {
  params <- CreateSamplePredictoRParams()
  x1 <- PredictoR(params=params)
  checkEquals(x1$params, params)
}

test.print.PredictoRParams <- function() {
  params <- CreateSamplePredictoRParams()
  x1 <- PredictoR(params=params)
  print(x1)
}
