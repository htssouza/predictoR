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

################################################################################
# General Functions
################################################################################

LoadFiles <- function() {
  loginfo("LoadFiles: begin")

  loginfo("LoadFiles: loading train CSV")
  Globals$train <<- read.csv(kTrainFileName)

  loginfo("LoadFiles: loading test CSV")
  Globals$test <<- read.csv(kTestFileName)

  loginfo("LoadFiles: end")
}

# if source col has word, set targetCol with 1, otherwise 0
ExtractWordAsFlag <- function(x, ...) UseMethod("ExtractWordAsFlag")
ExtractWordAsFlag.data.table <- function(x, word, sourceCol, targetCol=NA) {
  if (is.na(targetCol)) targetCol <- word
  pattern <- word
  x[, eval(targetCol) := 0]
  x[grep(pattern, x[, get(sourceCol)], ignore.case=TRUE), eval(targetCol) := 1 ]
  x[, eval(sourceCol) := gsub(pattern, "", get(sourceCol), ignore.case=TRUE) ]
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
        if(colClass != "character") {
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
                               paste0("title.", kTitles))))
}

GetModelsMetadata <- function () {
  return (NULL)
}

AddFeatures <- function(x, ...) UseMethod("v")
AddFeatures.data.table <- function(x, features) {
  # # split last and first name
  # nameTokens <- strsplit(x[, name], ", ")
  # x[, firstname := sapply(nameTokens, FUN=function(a) { stri_trim(a[2]) }) ]
  # x[, lastname  := sapply(nameTokens, FUN=function(a) { stri_trim(a[1]) }) ]
  # x[, name := NULL ]
  #
  # # extract title from first name as flags
  # # convert words to flags
  # titles <- paste0("title.", kTitles)
  # for (title in titles) {
  #   ExtractWordAsFlag(x, paste0(title, "."), "firstname", paste0("is", title))
  # }
  # x[, firstname := stri_trim(firstname) ]
  #
  # # extract letter from cabin
  # x[, cabin := tolower(cabin) ]
  # cabinLetters <- c("a", "b", "c", "d", "e", "f", "g", "t")
  # for (cabinLetter in cabinLetters) {
  #   ExtractWordAsFlag(x, cabinLetter, "cabin", paste0("cabin", cabinLetter))
  # }
  # x[, cabin := stri_trim(cabin) ]
}

GetTrainData <- function (trainFactor, folds, trainFolds, features) {
  return (NULL)
}

GetValidationData <- function (trainFactor, folds, validationFolds, features) {
  return (NULL)
}

GetTestData <- function (features) {
  return (NULL)
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
                                     getTrainData=GetTrainData,
                                     getValidationData=GetValidationData,
                                     getTestData=GetTestData,
                                     evaluate=Evaluate)
  loginfo(capture.output(predictoRParams))

  loginfo("Main: end")
}

Main()
