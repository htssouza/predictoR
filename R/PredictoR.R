################################################################################
# Predictor
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

source ("R/PredictoRParams.R")

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

GetFormula <- function(object) {
  loginfo("GetFormula: begin")
  featureNames <- object$params$featuresMetadata[, feature]
  formulaText <- paste0(object$params$responseColName, " ~ ", paste0(featureNames, collapse=" + "))
  y <- as.formula(formulaText)
  loginfo("GetFormula: end")
  return (y)
}

Fit.rpart <- function(object, modelMetadata, data) {
  loginfo("Fit.rpart: begin")
  library(rpart)
  control <- NULL
  if (! is.null(modelMetadata$minsplit)) {
    control <- rpart.control(minsplit=modelMetadata$minsplit)
  }
  fit <- rpart(GetFormula(object),
               data=data,
               method=modelMetadata$method,
               control=control)
  loginfo("Fit.rpart: end")
  return (fit)
}

Fit <- function(object, modelMetadata, data) {
  loginfo("Fit: begin")
  if (modelMetadata$model == "rpart") {
    return (Fit.rpart(object, modelMetadata, data))
  }
  loginfo("Fit: end")
  return (NULL)
}

BuildFeatures <- function(object, data) {
  loginfo("BuildFeatures: begin")
  for(feature in object$params$featuresMetadata[, feature]) {
    featureData <- object$params$buildFeature(data, feature)
    if (! is.null(featureData)) {
      if (! feature %in% colnames(data)) {
        data[, eval(feature) := featureData]
      }
    }
  }
  loginfo("BuildFeatures: end")
  return (data)
}

BuildTrainValidationData <- function(object, sampleFactor, sampleSeed, folds) {
  loginfo("BuildTrainValidationData: begin")
  loginfo("modelMetadata$sampleFactor:")
  loginfo(modelMetadata$sampleFactor)
  data <- object$params$getTrainData(sampleFactor = sampleFactor,
                                     sampleSeed = sampleSeed)
  if (! kFoldColName %in% colnames(data)) {
    data[, eval(kFoldColName) := ((.I %% folds) + 1)]
  }
  data <- BuildFeatures(object, data)
  loginfo("BuildTrainValidationData: end")
  return (data)
}

BuildTrainData <- function(object, data, trainFolds) {
  loginfo("BuildTrainData: begin")
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
  data <- object$params$getTestData()
  BuildFeatures(object, data)
  loginfo("BuildTestData: end")
}

Execute <- function(x, ...) UseMethod("Execute")
Execute.PredictoR <- function(object) {
  loginfo("Execute: begin")

  # add id and score on modelsMetadata
  modelsMetadata <- object$params$modelsMetadata
  modelsMetadata[, id := .I ]
  modelsMetadata[, score := as.numeric(NA) ]

  # iterate on each model metadata
  previousModelMetadata <- NULL
  for(modelMetadataIndex in 1:nrow(modelsMetadata)) {
    modelMetadata <- modelsMetadata[modelMetadataIndex]

    # build data, only if required
    needsToBuildData <- FALSE
    if (is.null(previousModelMetadata)) {
      needsToBuildData <- TRUE
    } else {
      if (previousModelMetadata$sampleFactor != modelMetadata$sampleFactor
          || previousModelMetadata$sampleSeed != modelMetadata$sampleSeed
          || previousModelMetadata$folds != modelMetadata$folds
          || previousModelMetadata$trainFolds != modelMetadata$trainFolds) {
            needsToBuildData <- TRUE
      }
    }
    if (needsToBuildData) {
      data <- BuildTrainValidationData(object, modelMetadata$sampleFactor, modelMetadata$sampleSeed, modelMetadata$folds)
      train <- BuildTrainData(object, data, modelMetadata$trainFolds)
      validation <- BuildValidationData(object, data, modelMetadata$trainFolds)
      gc()
    }

    fit <- Fit(object, modelMetadata, train)

    # validate
    # validationResponse <- PredictValidation(object, modelMetadata, fit, validation)
    # evaluation
    # validationScore <- Evaluate(validationResponse, validation[, get(object$params$responseColName)])
    # modelMetadata[, score: = validationScore]

    # save for next loop
    previousModelMetadata <- modelMetadata
  }
  loginfo("Execute: end")
}
