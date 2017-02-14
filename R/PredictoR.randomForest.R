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

PredictoR.Fit.randomForest <- function(object, modelMetadata, data) {
  loginfo("PredictoR.Fit.randomForest: begin")
  library(randomForest)
  fit <- randomForest(PredictoR.GetFormula(object),
               data=data,
               method=modelMetadata$method,
               ntree=modelMetadata$ntree)
  loginfo("PredictoR.Fit.randomForest: end")
  return (fit)
}

PredictoR.PredictModel.randomForest <- function(object, modelMetadata, fit, data) {
  loginfo("PredictoR.PredictModel.randomForest: begin")
  library(randomForest)
  y <- predict(fit, data, type=modelMetadata$method)
  loginfo("PredictoR.PredictModel.randomForest: end")
  return (y)
}
