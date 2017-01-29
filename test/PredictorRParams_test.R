################################################################################
# Test for the PredictorRParams Classes
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
  print(x1)
}
