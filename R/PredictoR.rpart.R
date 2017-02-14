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

PredictoR.Fit.rpart <- function(object, modelMetadata, data) {
  loginfo("PredictoR.Fit.rpart: begin")
  library(rpart)
  control <- NULL
  if (! is.null(modelMetadata$minsplit)) {
    control <- rpart.control(minsplit=modelMetadata$minsplit)
  }
  fit <- rpart(PredictoR.GetFormula(object),
               data=data,
               method=modelMetadata$method,
               control=control)
  loginfo("PredictoR.Fit.rpart: end")
  return (fit)
}

PredictoR.PredictModel.rpart <- function(object, modelMetadata, fit, data) {
  loginfo("PredictoR.PredictModel.rpart: begin")
  library(rpart)
  y <- predict(fit, data, type=modelMetadata$method)
  loginfo("PredictoR.PredictModel.rpart: end")
  return (y)
}
