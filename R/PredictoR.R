################################################################################
# PredictoR
################################################################################

################################################################################
# External dependencies
################################################################################

library(data.table)
library(logging)

################################################################################
# Local dependencies
################################################################################

source ("R/PredictoR.randomForest.R")
source ("R/PredictoR.rpart.R")
source ("R/PredictoR.xgboost.R")
source ("R/PredictoRParams.R")
source ("R/PredictoROutput.R")

################################################################################
# Constants
################################################################################

.kFoldColName <- "_fold"

################################################################################
# Private Functions
################################################################################

PredictoR.BuildTrainValidationData <- function(object, sampleFactor, sampleSeed) {
  loginfo("PredictoR.BuildTrainValidationData: begin")
  loginfo("sampleFactor:")
  loginfo(sampleFactor)
  data <- object$params$getTrainData(sampleFactor = sampleFactor,
                                     sampleSeed = sampleSeed)
  data <- BuildFeatures(object, data)
  loginfo("PredictoR.BuildTrainValidationData: end")
  return (data)
}

PredictoR.BuildTrainData <- function(object, data, folds, trainFolds) {
  loginfo("PredictoR.BuildTrainData: begin")
  data[, eval(.kFoldColName) := ((.I %% folds) + 1)]
  data <- data[get(.kFoldColName) <= trainFolds]
  loginfo("PredictoR.BuildTrainData: end")
  return (data)
}

PredictoR.BuildValidationData <- function(object, data, trainFolds) {
  loginfo("PredictoR.BuildValidationData: begin")
  data <- data[get(.kFoldColName) > trainFolds]
  loginfo("PredictoR.BuildValidationData: end")
  return (data)
}

PredictoR.GetFormula <- function(object) {
  featureNames <- object$params$featuresMetadata[, feature]
  formulaText <- paste0(object$params$responseColName, " ~ ", paste0(featureNames, collapse=" + "))
  y <- as.formula(formulaText)
  return (y)
}

PredictoR.Fit <- function(object, modelMetadata, data) {
  loginfo("PredictoR.Fit: begin")
  if (modelMetadata$model == "randomForest") {
    return (PredictoR.Fit.randomForest(object, modelMetadata, data))
  } else if (modelMetadata$model == "rpart") {
    return (PredictoR.Fit.rpart(object, modelMetadata, data))
  } else if (modelMetadata$model == "xgboost") {
    return (PredictoR.Fit.xgboost(object, modelMetadata, data))
  }
  loginfo("PredictoR.Fit: end")
  return (NULL)
}

PredictoR.GetBestModelMetadata <- function (modelsMetadata) {
  sortedOutputs <- modelsMetadata[order(-score)]
  best <- sortedOutputs[1]
  if (is.na(best$score)) {
    return (NULL)
  }
  return (best)
}

################################################################################
# Public Functions
################################################################################

PredictoR <- function(x, ...) UseMethod("PredictoR")

PredictoR.PredictoRParams <- function(params) {
  this <- list()
  this$params <- params
  class(this) <- "PredictoR"
  return (this)
}

print.PredictoR <- function(object) {
  writeLines("PredictoR:")
  writeLines("params:")
  writeLines(capture.output(object$params))
}

BuildFeatures <- function(x, ...) UseMethod("BuildFeatures")

BuildFeatures.PredictoR <- function(object, data) {
  loginfo("PredictoR.BuildFeatures: begin")
  for(feature in object$params$featuresMetadata[, feature]) {
    if (! feature %in% colnames(data)) {
      featureData <- object$params$buildFeature(data, feature)
      if (! is.null(featureData)) {
        data[, eval(feature) := featureData]
        data <- data.table(data)
      }
    }
  }
  loginfo("PredictoR.BuildFeatures: end")
  return (data)
}

BuildTestData <- function(x, ...) UseMethod("BuildTestData")

BuildTestData.PredictoR <- function(object) {
  loginfo("PredictoR.BuildTestData: begin")
  if (! ("testDxata" %in% object)) {
    object$testData <- data.table(object$params$getTestData())
    object$testData <- BuildFeatures(object, object$testData)
  }
  loginfo("PredictoR.BuildTestData: end")
  return (object$testData)
}

PredictModel <- function(x, ...) UseMethod("PredictModel")

PredictModel.PredictoR <- function(object, modelMetadata, fit, data) {
  loginfo("PredictoR.PredictModel: begin")
  if (modelMetadata$model == "randomForest") {
    return (PredictoR.PredictModel.randomForest(object, modelMetadata, fit, data))
  } else if (modelMetadata$model == "rpart") {
    return (PredictoR.PredictModel.rpart(object, modelMetadata, fit, data))
  } else if (modelMetadata$model == "xgboost") {
    return (PredictoR.PredictModel.xgboost(object, modelMetadata, fit, data))
  }
  loginfo("PredictoR.PredictModel: end")
  return (NULL)
}

Execute <- function(x, ...) UseMethod("Execute")
Execute.PredictoR <- function(object) {
  loginfo("PredictoR.Execute: begin")

  # fits
  fits <- list()

  # set order (reuse data)
  object$params$modelsMetadata <- object$params$modelsMetadata[order(sampleFactor,
                                                                     sampleSeed,
                                                                     folds,
                                                                     trainFolds,
                                                                     model)]

  # add id and score on modelsMetadata
  modelsMetadata <- object$params$modelsMetadata
  modelsMetadata[, id := .I ]
  modelsMetadata[, score := as.numeric(NA) ]

  # iterate on each model metadata
  previousModelMetadata <- NULL
  for(modelMetadataId in (modelsMetadata[, id])) {
    modelMetadata <- modelsMetadata[id == modelMetadataId]

    loginfo("PredictoR.Execute: training and evaluating")
    loginfo(paste0("MODEL: ", modelMetadataId, " / ", nrow(modelsMetadata)))
    loginfo("modelMetadata:")
    loginfo(capture.output(modelMetadata))

    # build data, only if necessary
    needsToBuildData <- FALSE
    needsToBuildTrainAndValidation <- FALSE
    if (is.null(previousModelMetadata)) {
      needsToBuildData <- TRUE
      needsToBuildTrainAndValidation <- TRUE
    } else {
      if (previousModelMetadata$sampleFactor != modelMetadata$sampleFactor
          || previousModelMetadata$sampleSeed != modelMetadata$sampleSeed) {
            needsToBuildData <- TRUE
            needsToBuildTrainAndValidation <- TRUE
      }
      if (previousModelMetadata$folds != modelMetadata$folds
          || previousModelMetadata$trainFolds != modelMetadata$trainFolds) {
            needsToBuildTrainAndValidation <- TRUE
      }
    }
    if (needsToBuildData) {
      loginfo("PredictoR.Execute: needs to rebuild data")
      data <- PredictoR.BuildTrainValidationData(object, modelMetadata$sampleFactor, modelMetadata$sampleSeed)
    }
    if (needsToBuildTrainAndValidation) {
      loginfo("PredictoR.Execute: splitting train and validation")
      train <- PredictoR.BuildTrainData(object, data, modelMetadata$folds, modelMetadata$trainFolds)
      train.xgboost <- NULL
      validation <- PredictoR.BuildValidationData(object, data, modelMetadata$trainFolds)
      validation.xgboost <- NULL
      gc()
    }

    # train
    loginfo("Execute: fitting")
    if (modelMetadata$model == "xgboost") {
      if (is.null(train.xgboost)) {
        train.xgboost <- PredictoR.BuildXGBData(train, object, withLabel=TRUE)
      }
      fit <- PredictoR.Fit(object, modelMetadata, train.xgboost)
    } else {
      fit <- PredictoR.Fit(object, modelMetadata, train)
    }
    fits[[modelMetadataId]] <- fit

    # validate&evaluate
    loginfo("PredictoR.Execute: validation prediction")
    if (modelMetadata$model == "xgboost") {
      if (is.null(validation.xgboost)) {
        validation.xgboost <- PredictoR.BuildXGBData(validation, object, withLabel=FALSE)
      }
      validationResponse <- PredictModel(object, modelMetadata, fit, validation.xgboost)
    } else {
      validationResponse <- PredictModel(object, modelMetadata, fit, validation)
    }
    if (! is.null(object$params$normalizeResponse)) {
      validationResponse <- object$params$normalizeResponse(validationResponse)
    }
    loginfo("PredictoR.Execute: evaluation")
    validationScore <- object$params$evaluate(validationResponse, validation[, get(object$params$responseColName)])
    loginfo("score:")
    loginfo(capture.output(validationScore))
    modelsMetadata[id == modelMetadataId, score := validationScore]

    # save for next loop
    previousModelMetadata <- modelMetadata
  }

  prediction <- NULL
  bestModelMetada <- PredictoR.GetBestModelMetadata(modelsMetadata)
  if (! is.null(bestModelMetada)) {
    fit <- fits[[bestModelMetada$id]]
    loginfo("PredictoR.Execute: building test data")
    test <- BuildTestData(object)
    if (bestModelMetada$model == "xgboost") {
      test.xgboost <- PredictoR.BuildXGBData(test, object, withLabel=FALSE)
      predictionResponse <- PredictoR.PredictModel(object, bestModelMetada, fit, test.xgboost)
    } else {
      predictionResponse <- PredictoR.PredictModel(object, bestModelMetada, fit, test)
    }
    if (! is.null(object$params$normalizeResponse)) {
      predictionResponse <- object$params$normalizeResponse(predictionResponse)
    }

    prediction <- data.table(id = test[, get(object$params$idColName)],
                             response = predictionResponse)
    setnames(prediction, "id", object$params$idColName)
    setnames(prediction, "response", object$params$responseColName)
  }

  output <- PredictoROutput(object$params, fits, prediction)

  loginfo("PredictoR.Execute: end")
  return (output)
}
