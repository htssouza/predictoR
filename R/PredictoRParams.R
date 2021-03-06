################################################################################
# PredictoR Params
################################################################################

################################################################################
# External dependencies
################################################################################

library(data.table)
library(logging)

################################################################################
# Predictor Params
#
# parameter descriptions:
#
#   idColName: name of id column
#
#   responseColName: name of response column
#
#   featuresMetadata:
#     data.table with feature metadata
#     example:
#       features <- data.table(feature=c("age", "height", "weight")
#
#   modelsMetadata:
#     data.table with model metadata
#     example:
#       models <- data.table(trainFactor=rep(".5", 7),
#                            folds=rep(5, 7),
#                            trainFolds=rep(4, 7),
#                            validationFolds=rep(1, 7),
#                            model=rep("rpart", 7),
#                            method=rep("class", 7),
#                            minsplit=c(1, 2, 3, 5, 10, 20, 30))
#
# function descriptions:
#
#   buildFeature(x, feature)
#     build feature for x dataset
#
#   getTrainData(sampleFactor, sampleSeed)
#     return data.table with train data
#
#   getTestData()
#     return da.table with train data
#
#   evaluate(prediction, expected)
#     return validation score (better the higher)
#
#   normalizeResponse(response)
#     return normalized response
#
################################################################################

################################################################################
# Functions
################################################################################

PredictoRParams <- function(x, ...) UseMethod("PredictoRParams")

PredictoRParams.default <- function(idColName,
                                    responseColName,
                                    featuresMetadata,
                                    modelsMetadata,
                                    buildFeature,
                                    getTrainData,
                                    evaluate,
                                    normalizeResponse=NULL,
                                    getTestData=NULL) {
  this <- list()
  this$idColName <- idColName
  this$responseColName <- responseColName
  this$featuresMetadata <- data.table(featuresMetadata)
  this$modelsMetadata <- data.table(modelsMetadata)
  this$buildFeature <- buildFeature
  this$getTrainData <- getTrainData
  this$evaluate <- evaluate
  this$normalizeResponse <- normalizeResponse
  this$getTestData <- getTestData
  class(this) <- "PredictoRParams"
  return (this)
}

print.PredictoRParams <- function(object) {
  writeLines("PredictoRParams:")
  writeLines("idColName:")
  writeLines(object$idColName)
  writeLines("responseColName:")
  writeLines(object$responseColName)
  writeLines("featuresMetadata:")
  writeLines(capture.output(object$featuresMetadata))
  writeLines("modelsMetadata:")
  writeLines(capture.output(object$modelsMetadata))
}
