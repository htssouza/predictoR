################################################################################
# Test for the PredictorRParams Class
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
                       buildFeature=NULL,
                       getTrainData=NULL,
                       getTestData=NULL,
                       evaluate=NULL)
  return (y)
}

################################################################################
# Tests
################################################################################

test.PredictoRParams <- function() {
  idColName <- "id"
  responseColName <- "response"
  featuresMetadata <- data.table(feature=c("name", "age"))
  modelsMetadata <- data.table(model=c("rpart"))
  x1 <- PredictoRParams(idColName=idColName,
                        responseColName=responseColName,
                        featuresMetadata=featuresMetadata,
                        modelsMetadata=modelsMetadata,
                        buildFeature=NULL,
                        getTrainData=NULL,
                        getTestData=NULL,
                        evaluate=NULL)
  checkEquals(x1$idColName, idColName)
  checkEquals(x1$responseColName, responseColName)
  checkEquals(x1$featuresMetadata, featuresMetadata)
  checkEquals(x1$modelsMetadata, modelsMetadata)
}

test.print.PredictoRParams <- function() {
  x1 <- CreateSamplePredictoRParams()
  print(x1)
}
