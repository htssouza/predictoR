#!/usr/bin/env Rscript

################################################################################
# Titanic (Kaggle) Predictor
################################################################################

################################################################################
# External dependencies
################################################################################

for (.requirement in c("data.table", "logging", "stringi")) {
  if (! .requirement %in% rownames(installed.packages())) {
    install.packages(.requirement, repos="http://cran.rstudio.com/")
  }
}

library(data.table)
library(logging)
library(stringi)

################################################################################
# Local dependencies
################################################################################

source ("R/PredictoR.R")

################################################################################
# Logging configuration
################################################################################

basicConfig()

################################################################################
# Globals
################################################################################

Globals <<- list()

################################################################################
# Constants (change may be required for your own environment)
################################################################################

# Paths
kTrainFileName <- "R/samples/titanic/train.csv"
kTestFileName <- "R/samples/titanic/test.csv"

# Used on feature engineering
kTitles <- c("capt", "col", "don", "dr", "major", "master", "miss", "mlle", "mr", "mrs", "rev")
kCabinLetters <- c("a", "b", "c", "d", "e", "f", "g", "t")

################################################################################
# General Functions
################################################################################

LoadFiles <- function() {
  loginfo("LoadFiles: begin")

  loginfo("LoadFiles: loading train CSV")
  Globals$train <<- fread(kTrainFileName)

  loginfo("LoadFiles: loading test CSV")
  Globals$test <<- fread(kTestFileName)

  loginfo("LoadFiles: end")
}

PreProcessDataSet <- function(x) {

  # convert to data.table if needed
  if (! is(x, "data.table")) {
    x <- data.table(x)
  }

  # all columns to lower case
  names(x) <- tolower(names(x))

  # all text to lower
  colsToConvert <- c()
  for(colName in names(x)) {
    if (! is.na(colName)) {
      if (!is.null(colName)) {
        col <- x[, get(colName)]
        colClass <- class(col)
        if(colClass == "character") {
          colsToConvert <- c(colsToConvert, colName)
        }
      }
    }
  }
  for(colName in colsToConvert) {
    x[, eval(colName) := tolower(get(colName))]
  }

  return (x)
}

PreProcess <- function() {
  loginfo("PreProcess: begin")
  Globals$train <<- PreProcessDataSet(Globals$train)
  Globals$test <<- PreProcessDataSet(Globals$test)
  loginfo("PreProcess: end")
}

################################################################################
# PredictoR Functions
################################################################################

BuildFeature <- function(x, ...) UseMethod("BuildFeature")
BuildFeature.data.table <- function(x, feature) {

  if ("lastname" == feature) {
    nameTokens <- strsplit(x[, name], ", ")
    return (sapply(nameTokens, FUN=function(a) { stri_trim(a[1]) }))
  }

  if (startsWith(feature, "title.")) {
    nameTokens <- strsplit(x[, name], ", ")
    firstName <- sapply(nameTokens, FUN=function(a) { stri_trim(a[2]) })
    title <- strsplit(feature, "\\.")[[1]][2]
    y <- ifelse(grepl(paste0(title, "\\."), firstName, ignore.case=TRUE), 1, 0)
    return (y)
  }

  if (startsWith(feature, "cabin.")) {
    cabinLetter <- strsplit(feature, "\\.")[[1]][2]
    y <- ifelse(grepl(cabinLetter, x[, cabin], ignore.case=TRUE), 1, 0)
    return (y)
  }

  return (NULL)
}

GetFeaturesMetadata <- function () {
  return (data.table(feature=c("pclass",
                               "lastname",
                               "sex",
                               "age",
                               "sibsp",
                               "parch",
                               "ticket",
                               "fare",
                               "cabin",
                               "embarked",
                               paste0("cabin.", kCabinLetters),
                               paste0("title.", kTitles))))
}

GetModelsMetadata <- function () {
  return (data.table(model=c("rpart",
                     method="class")))
}

GetTrainData <- function (trainFactor, folds, trainFolds, features) {
  return (NULL)
}

GetValidationData <- function (trainFactor, folds, validationFolds, features) {
  return (NULL)
}

GetTestData <- function () {
  return (Globals$test)
}

Evaluate <- function (prediction, expected) {
  return (NULL)
}

################################################################################
# Main Flow
################################################################################

Main <- function() {
  loginfo("Main: begin")

  loginfo("Main: loading files")
  LoadFiles()

  loginfo("Main: pre-processing files")
  PreProcess()

  loginfo("Main: creating PredictorRParams")
  predictoRParams <- PredictoRParams(idColName="passengerid",
                                     responseColName="survived",
                                     featuresMetadata=GetFeaturesMetadata(),
                                     modelsMetadata=GetModelsMetadata(),
                                     buildFeature=BuildFeature,
                                     getTrainData=GetTrainData,
                                     getValidationData=GetValidationData,
                                     getTestData=GetTestData,
                                     evaluate=Evaluate)
  loginfo(capture.output(predictoRParams))

  loginfo("Main: end")
}

Main()
