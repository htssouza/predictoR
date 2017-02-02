################################################################################
# PredictoR
################################################################################

################################################################################
# External dependencies
################################################################################

for (.requirement in c("data.table", "logging")) {
  if (! .requirement %in% rownames(installed.packages())) {
    install.packages(.requirement, repos="http://cran.rstudio.com/")
  }
}

library(data.table)
library(logging)

################################################################################
# Local dependencies
################################################################################

source ("R/PredictoR.randomForest.R")
source ("R/PredictoR.rpart.R")
source ("R/PredictoRParams.R")
source ("R/PredictoROutput.R")

################################################################################
# Constants
################################################################################

kFoldColName <- "_fold"

################################################################################
# Functions
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

BuildFeatures <- function(object, data) {
  loginfo("BuildFeatures: begin")
  for(feature in object$params$featuresMetadata[, feature]) {
    if (! feature %in% colnames(data)) {
      featureData <- object$params$buildFeature(data, feature)
      if (! is.null(featureData)) {
        data[, eval(feature) := featureData]
        data <- data.table(data)
      }
    }
  }
  loginfo("BuildFeatures: end")
  return (data)
}

BuildTrainValidationData <- function(object, sampleFactor, sampleSeed) {
  loginfo("BuildTrainValidationData: begin")
  loginfo("sampleFactor:")
  loginfo(sampleFactor)
  data <- object$params$getTrainData(sampleFactor = sampleFactor,
                                     sampleSeed = sampleSeed)
  data <- BuildFeatures(object, data)
  loginfo("BuildTrainValidationData: end")
  return (data)
}

BuildTrainData <- function(object, data, folds, trainFolds) {
  loginfo("BuildTrainData: begin")
  data[, eval(kFoldColName) := ((.I %% folds) + 1)]
  data <- data[get(kFoldColName) <= trainFolds]
  loginfo("BuildTrainData: end")
  return (data)
}

BuildValidationData <- function(object, data, trainFolds) {
  loginfo("BuildValidationData: begin")
  data <- data[get(kFoldColName) > trainFolds]
  loginfo("BuildValidationData: end")
  return (data)
}

BuildTestData <- function(object) {
  loginfo("BuildTestData: begin")
  data <- data.table(object$params$getTestData())
  data <- BuildFeatures(object, data)
  loginfo("BuildTestData: end")
  return (data)
}

GetFormula <- function(object) {
  featureNames <- object$params$featuresMetadata[, feature]
  formulaText <- paste0(object$params$responseColName, " ~ ", paste0(featureNames, collapse=" + "))
  y <- as.formula(formulaText)
  return (y)
}

Fit <- function(object, modelMetadata, data) {
  loginfo("Fit: begin")
  if (modelMetadata$model == "randomForest") {
    return (Fit.randomForest(object, modelMetadata, data))
  }
  else if (modelMetadata$model == "rpart") {
    return (Fit.rpart(object, modelMetadata, data))
  }
  loginfo("Fit: end")
  return (NULL)
}

PredictModel <- function(object, modelMetadata, fit, validation) {
  loginfo("PredictModel: begin")
  if (modelMetadata$model == "randomForest") {
    return (PredictModel.randomForest(object, modelMetadata, fit, validation))
  }
  else if (modelMetadata$model == "rpart") {
    return (PredictModel.rpart(object, modelMetadata, fit, validation))
  }
  loginfo("PredictModel: end")
  return (NULL)
}

GetBestModelMetadata <- function (modelsMetadata) {
  sortedOutputs <- modelsMetadata[order(-score)]
  best <- sortedOutputs[1]
  if (is.na(best$score)) {
    return (NULL)
  }
  return (best)
}

Execute <- function(x, ...) UseMethod("Execute")
Execute.PredictoR <- function(object) {
  loginfo("Execute: begin")

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

    loginfo("Execute: training and evaluating")
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
      loginfo("Execute: needs to rebuild data")
      data <- BuildTrainValidationData(object, modelMetadata$sampleFactor, modelMetadata$sampleSeed)
    }
    if (needsToBuildTrainAndValidation) {
      loginfo("Execute: splitting train and validation again")
      train <- BuildTrainData(object, data, modelMetadata$folds, modelMetadata$trainFolds)
      validation <- BuildValidationData(object, data, modelMetadata$trainFolds)
      gc()
    }

    # train
    loginfo("Execute: fitting")
    fit <- Fit(object, modelMetadata, train)
    fits[[modelMetadataId]] <- fit

    # validate&evaluate
    loginfo("Execute: validation prediction")
    validationResponse <- PredictModel(object, modelMetadata, fit, validation)
    loginfo("Execute: evaluation")
    validationScore <- object$params$evaluate(validationResponse, validation[, get(object$params$responseColName)])
    loginfo("score:")
    loginfo(capture.output(validationScore))
    modelsMetadata[id == modelMetadataId, score := validationScore]

    # save for next loop
    previousModelMetadata <- modelMetadata
  }

  prediction <- NULL
  bestModelMetada <- GetBestModelMetadata(modelsMetadata)
  if (! is.null(bestModelMetada)) {
    fit <- fits[[bestModelMetada$id]]
    loginfo("Execute: building test data")
    test <- BuildTestData(object)
    predictionResponse <- PredictModel(object, bestModelMetada, fit, test)
    prediction <- data.table(id = test[, get(object$params$idColName)],
                             response = predictionResponse)
    setnames(prediction, "id", object$params$idColName)
    setnames(prediction, "response", object$params$responseColName)
  }

  output <- PredictoROutput(object$params, fits, prediction)

  loginfo("Execute: end")
  return (output)
}
