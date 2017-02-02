################################################################################
# PredictoR.rpart
################################################################################

################################################################################
# External dependencies
################################################################################

for (.requirement in c("rpart")) {
  if (! .requirement %in% rownames(installed.packages())) {
    install.packages(.requirement, repos="http://cran.rstudio.com/")
  }
}

################################################################################
# Functions
################################################################################

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

PredictModel.rpart <- function(object, modelMetadata, fit, validation) {
  loginfo("PredictModel.rpart: begin")
  library(rpart)
  y <- predict(fit, validation, type=modelMetadata$method)
  loginfo("PredictModel.rpart: end")
  return (y)
}
