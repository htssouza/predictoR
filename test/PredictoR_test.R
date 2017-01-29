################################################################################
# Test for the PredictorR Class
################################################################################

################################################################################
# External dependencies
################################################################################

for (.requirement in c("data.table", "RUnit")) {
  if (! .requirement %in% rownames(installed.packages())) {
    install.packages(.requirement, repos="http://cran.rstudio.com/")
  }
}

library(data.table)
library(RUnit)

################################################################################
# Local dependencies
################################################################################

source ("R/PredictoR.R")
source ("test/PredictoRParams_test.R")

################################################################################
# Tests
################################################################################

test.PredictoR <- function () {
  params <- CreateSamplePredictoRParams()
  x1 <- PredictoR(params=params)
  checkEquals(x1$params, params)
}

test.print.PredictoRParams <- function () {
  params <- CreateSamplePredictoRParams()
  x1 <- PredictoR(params=params)
  print(x1)
}
