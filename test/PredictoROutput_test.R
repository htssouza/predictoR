################################################################################
# Test for the PredictorROutput Class
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
