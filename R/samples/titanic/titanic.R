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

BuildFeature <- function(x, ...) UseMethod("BuildFeature")
BuildFeature.data.table <- function(x, feature) {

  if ("firstname" == feature) {
    if (! "firstname" %in% colnames(x)) {
      nameTokens <- strsplit(x[, name], ", ")
      x[, firstname := sapply(nameTokens, FUN=function(a) { stri_trim(a[2]) }) ]
    }
  }

  if ("lastname" == feature) {
    nameTokens <- strsplit(x[, name], ", ")
    x[, lastname  := sapply(nameTokens, FUN=function(a) { stri_trim(a[1]) }) ]
  }

  if (startsWith(feature, "title.")) {
    title <- strsplit(feature, "\\.")[2]
    BuildFeature(x, "firstname")
    ExtractWordAsFlag(x, paste0(title, "."), "firstname", paste0("title.", title))
  }

  if (startsWith(feature, "cabin.")) {
    cabinLetter <- strsplit(feature, "\\.")[2]
    ExtractWordAsFlag(x, cabinLetter, "cabin", paste0("cabin.", cabinLetter))
  }
}

GetFeaturesMetadata <- function () {
  return (data.table(feature=c("pclass",
                               "firstname",
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

GetTrainData <- function (trainFactor, folds, trainFolds) {
  return (NULL)
}

GetValidationData <- function (trainFactor, folds, validationFolds) {
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
                                     buildFeature=BuildFeature,
                                     getTrainData=GetTrainData,
                                     getValidationData=GetValidationData,
                                     getTestData=GetTestData,
                                     evaluate=Evaluate)
  loginfo(capture.output(predictoRParams))

  loginfo("Main: end")
}

Main()
