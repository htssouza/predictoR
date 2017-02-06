################################################################################
# PredictoR.xgboost
################################################################################

################################################################################
# External dependencies
################################################################################

library(data.table)
library(logging)

################################################################################
# Functions
################################################################################

PredictoR.BuildXGBData <- function(x, object, withLabel) {
  loginfo("Predictor.BuildXGBData: begin")
  library(xgboost)
  y <- matrix(nrow=nrow(x), ncol=nrow(object$params$featuresMetadata))
  colIndex <- 1
  for(feature in object$params$featuresMetadata[, feature]) {
    col <- x[, get(feature)]
    colClass <- class(col)
    if(colClass == "character") {
      stop("Character is not a valid type for xgboost")
    } else {
      y[, colIndex] <- as.numeric(col)
    }
    colIndex <- (colIndex + 1)
  }

  if (withLabel) {
    y <- xgb.DMatrix (as.matrix(y), label=as.numeric(as.character(x[, get(object$params$responseColName)])), missing=NaN)
  } else {
    y <- xgb.DMatrix (as.matrix(y), missing=NaN)
  }
  loginfo("Predictor.BuildXGBData: end")
  return (y)
}

Predictor.Fit.xgboost <- function(object, modelMetadata, dataWithLabel) {
  loginfo("Predictor.Fit.xgboost: begin")
  library(xgboost)
  fit <- xgboost(dataWithLabel,
                 objective=modelMetadata$objective,
                 nround=modelMetadata$nround)
  loginfo("Predictor.Fit.xgboost: end")
  return (fit)
}

Predictor.PredictModel.xgboost <- function(object, modelMetadata, fit, dataWithoutLabel) {
  loginfo("Predictor.PredictModel.xgboost: begin")
  library(xgboost)
  y <- predict(fit, dataWithoutLabel)
  loginfo("Predictor.PredictModel.xgboost: end")
  return (y)
}
