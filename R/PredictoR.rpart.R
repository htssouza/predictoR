################################################################################
# PredictoR.rpart
################################################################################

################################################################################
# External dependencies
################################################################################

library(data.table)
library(logging)

################################################################################
# Functions
################################################################################

Predictor.Fit.rpart <- function(object, modelMetadata, data) {
  loginfo("Predictor.Fit.rpart: begin")
  library(rpart)
  control <- NULL
  if (! is.null(modelMetadata$minsplit)) {
    control <- rpart.control(minsplit=modelMetadata$minsplit)
  }
  fit <- rpart(Predictor.GetFormula(object),
               data=data,
               method=modelMetadata$method,
               control=control)
  loginfo("Predictor.Fit.rpart: end")
  return (fit)
}

Predictor.PredictModel.rpart <- function(object, modelMetadata, fit, validation) {
  loginfo("Predictor.PredictModel.rpart: begin")
  library(rpart)
  y <- predict(fit, validation, type=modelMetadata$method)
  loginfo("Predictor.PredictModel.rpart: end")
  return (y)
}
