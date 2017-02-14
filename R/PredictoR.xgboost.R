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
  loginfo("PredictoR.BuildXGBData: begin")
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
  loginfo("PredictoR.BuildXGBData: end")
  return (y)
}

PredictoR.Fit.xgboost <- function(object, modelMetadata, dataWithLabel) {
  loginfo("PredictoR.Fit.xgboost: begin")
  library(xgboost)
  if (! is.null(modelMetadata$num_class)) {
    fit <- xgboost(dataWithLabel,
                   objective=modelMetadata$objective,
                   nrounds=modelMetadata$nrounds,
                   num_class=modelMetadata$num_class)
  } else {
    fit <- xgboost(dataWithLabel,
                   objective=modelMetadata$objective,
                   nrounds=modelMetadata$nrounds)
  }
  loginfo("PredictoR.Fit.xgboost: end")
  return (fit)
}

PredictoR.PredictModel.xgboost <- function(object, modelMetadata, fit, dataWithoutLabel) {
  loginfo("PredictoR.PredictModel.xgboost: begin")
  library(xgboost)
  if (! ("xgb.DMatrix" %in% class(dataWithoutLabel))) {
    dataWithoutLabel <- PredictoR.BuildXGBData(dataWithoutLabel, object, FALSE) 
  }
  y <- predict(fit, dataWithoutLabel)
  loginfo("PredictoR.PredictModel.xgboost: end")
  return (y)
}
