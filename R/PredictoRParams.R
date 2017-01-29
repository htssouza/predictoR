################################################################################
# Predictor Params
################################################################################

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
#       models <- data.table(model=rep("rpart", 7),
#                            method=rep("class", 7),
#                            minsplit=c(1, 2, 3, 5, 10, 20, 30))
#
# function descriptions:
#
#   getTrainData(trainFactor, folds, trainFolds, features)
#     return data.table with train data
#
#   getValidationData(trainFactor, folds, validationFolds, features)
#     return data.table with validation data
#
#   getTestData(features)
#     return da.table with train data
#
#   evaluate(prediction, expected)
#     return validation score (better the higher)
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
                                    getValidationData,
                                    getTestData,
                                    evaluate) {
  this <- list()
  this$idColName <- idColName
  this$responseColName <- responseColName
  this$featuresMetadata <- featuresMetadata
  this$modelsMetadata <- modelsMetadata
  this$buildFeature <- buildFeature
  this$getTrainData <- getTrainData
  this$getValidationData <- getValidationData
  this$getTestData <- getTestData
  this$evaluate <- evaluate
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
