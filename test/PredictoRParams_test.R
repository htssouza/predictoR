################################################################################
# Test for the PredictorRParams Class
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

source ("R/PredictoRParams.R")

################################################################################
# Functions
################################################################################

CreateSamplePredictoRParams <- function() {
  idColName <- "id"
  responseColName <- "response"
  featuresMetadata <- data.table(feature=c("name", "age"))
  modelsMetadata <- data.table(model=c("rpart"))
  y <- PredictoRParams(idColName=idColName,
                        responseColName=responseColName,
                        featuresMetadata=featuresMetadata,
                        modelsMetadata=modelsMetadata,
                        getTrainData=NULL,
                        getValidationData=NULL,
                        getTestData=NULL,
                        evaluate=NULL)
  return (y)
}

################################################################################
# Tests
################################################################################

test.PredictoRParams <- function () {
  idColName <- "id"
  responseColName <- "response"
  featuresMetadata <- data.table(feature=c("name", "age"))
  modelsMetadata <- data.table(model=c("rpart"))
  x1 <- PredictoRParams(idColName=idColName,
                        responseColName=responseColName,
                        featuresMetadata=featuresMetadata,
                        modelsMetadata=modelsMetadata,
                        getTrainData=NULL,
                        getValidationData=NULL,
                        getTestData=NULL,
                        evaluate=NULL)
  checkEquals(x1$idColName, idColName)
  checkEquals(x1$responseColName, responseColName)
  checkEquals(x1$featuresMetadata, featuresMetadata)
  checkEquals(x1$modelsMetadata, modelsMetadata)
}

test.print.PredictoRParams <- function () {
  x1 <- CreateSamplePredictoRParams()
  print(x1)
}
