################################################################################
# PredictoR.randomForest
################################################################################

################################################################################
# External dependencies
################################################################################

for (.requirement in c("randomForest")) {
  if (! .requirement %in% rownames(installed.packages())) {
    install.packages(.requirement, repos="http://cran.rstudio.com/")
  }
}

library(data.table)
library(logging)
library(randomForest)

################################################################################
# Functions
################################################################################

Fit.randomForest <- function(object, modelMetadata, data) {
  loginfo("Fit.randomForest: begin")
  library(randomForest)
  fit <- randomForest(GetFormula(object),
               data=data,
               method=modelMetadata$method,
               ntree=modelMetadata$ntree)
  loginfo("Fit.randomForest: end")
  return (fit)
}

PredictModel.randomForest <- function(object, modelMetadata, fit, validation) {
  loginfo("PredictModel.randomForest: end")
  library(randomForest)
  y <- predict(fit, validation, type=modelMetadata$method)
  loginfo("PredictModel.randomForest: end")
  return (y)
}
