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

source("R/PredictoR.R")

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

test.PredictoRParams1 <- function() {
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

test.PredictoRParams2 <- function() {
  idColName <- "id"
  responseColName <- "response"
  featuresMetadata <- data.table(feature=c("name", "age"))
  modelsMetadata <- data.table(model=c("rpart"))
  x2 <- PredictoRParams(idColName=idColName,
                        responseColName=responseColName,
                        featuresMetadata=featuresMetadata,
                        modelsMetadata=modelsMetadata,
                        buildFeature=NULL,
                        getTrainData=NULL,
                        getTestData=NULL,
                        evaluate=NULL,
                        normalizeResponse=test.PredictoRParams1)
  checkEquals(x2$idColName, idColName)
  checkEquals(x2$responseColName, responseColName)
  checkEquals(x2$featuresMetadata, featuresMetadata)
  checkEquals(x2$modelsMetadata, modelsMetadata)
  checkEquals(x2$normalizeResponse, test.PredictoRParams1)
}

test.print.PredictoRParams <- function() {
  x1 <- CreateSamplePredictoRParams()
  print(x1)
}
