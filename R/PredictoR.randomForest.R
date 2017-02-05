################################################################################
# PredictoR.randomForest
################################################################################

################################################################################
# External dependencies
################################################################################

library(data.table)
library(logging)

################################################################################
# Functions
################################################################################

Predictor.Fit.randomForest <- function(object, modelMetadata, data) {
  loginfo("Predictor.Fit.randomForest: begin")
  library(randomForest)
  fit <- randomForest(Predictor.GetFormula(object),
               data=data,
               method=modelMetadata$method,
               ntree=modelMetadata$ntree)
  loginfo("Predictor.Fit.randomForest: end")
  return (fit)
}

Predictor.PredictModel.randomForest <- function(object, modelMetadata, fit, validation) {
  loginfo("Predictor.PredictModel.randomForest: end")
  library(randomForest)
  y <- predict(fit, validation, type=modelMetadata$method)
  loginfo("Predictor.PredictModel.randomForest: end")
  return (y)
}
