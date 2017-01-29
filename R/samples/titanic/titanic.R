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
# Constants (change may be required for your own environment)
################################################################################

# Paths
kTrainFileName <- "R/samples/titanic/train.csv"
kTestFileName <- "R/samples/titanic/test.csv"

# Used on feature engineering
kTitles <- c("capt", "col", "don", "dr", "major", "master", "miss", "mlle", "mr", "mrs", "rev")
kCabinLetters <- c("a", "b", "c", "d", "e", "f", "g", "t")

################################################################################
# PredictoR Functions
################################################################################

BuildFeature <- function(x, ...) UseMethod("BuildFeature")
BuildFeature.data.table <- function(x, feature) {
  loginfo("BuildFeature: begin")
  loginfo("feature:")
  loginfo(feature)
  y <- NULL

  # local cache
  if(feature %in% colnames(x)) {
    y <- x[, get(feature)]
    loginfo("BuildFeature: end")
    return (y)
  }

  if ("lastname" == feature) {
    nameTokens <- strsplit(x[, name], ", ")
    y <- sapply(nameTokens, FUN=function(a) { stri_trim(a[1]) })
  }

  if (startsWith(feature, "title.")) {
    nameTokens <- strsplit(x[, name], ", ")
    firstName <- sapply(nameTokens, FUN=function(a) { stri_trim(a[2]) })
    title <- strsplit(feature, "\\.")[[1]][2]
    y <- ifelse(grepl(paste0(title, "\\."), firstName, ignore.case=TRUE), 1, 0)
  }

  if (startsWith(feature, "cabin.")) {
    cabinLetter <- strsplit(feature, "\\.")[[1]][2]
    y <- ifelse(grepl(cabinLetter, x[, cabin], ignore.case=TRUE), 1, 0)
  }

  loginfo("BuildFeature: end")
  return (y)
}

GetFeaturesMetadata <- function() {
  features <- data.table(feature=c("pclass",
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
                                   paste0("title.", kTitles)))
  return (features)
}

GetModelsMetadata <- function() {
  models <- data.table(sampleFactor=rep(.5, 7),
                       sampleSeed=rep(1994, 7),
                       folds=rep(5, 7),
                       trainFolds=rep(4, 7),
                       validationFolds=rep(1, 7),
                       model=rep("rpart", 7),
                       method=rep("class", 7),
                       minsplit=c(1, 2, 3, 5, 10, 20, 30))

  return (models)
}

PreProcess <- function(x) {

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

GetTrainData <- function(sampleFactor, sampleSeed) {
  set.seed(sampleSeed)
  loginfo("GetTrainData: begin")
  y <- fread(kTrainFileName)
  y <- PreProcess(y)
  sampleSize <- as.integer(nrow(y) * sampleFactor)
  y <- y[][][sample(.N, sampleSize)]
  loginfo("GetTrainData: end")
  return (y)
}

GetTestData <- function() {
  loginfo("GetTestData: begin")
  set.seed(sampleSeed)
  y <- fread(kTestFileName)
  y <- PreProcess(y)
  loginfo("GetTestData: end")
  return (y)
}

Evaluate <- function(prediction, expected) {
  return (NULL)
}

################################################################################
# Main Flow
################################################################################

Main <- function() {
  loginfo("Main: begin")

  loginfo("Main: creating PredictoRParams")
  predictoRParams <- PredictoRParams(idColName="passengerid",
                                     responseColName="survived",
                                     featuresMetadata=GetFeaturesMetadata(),
                                     modelsMetadata=GetModelsMetadata(),
                                     buildFeature=BuildFeature,
                                     getTrainData=GetTrainData,
                                     getTestData=GetTestData,
                                     evaluate=Evaluate)
  loginfo(capture.output(predictoRParams))

  loginfo("Main: creating PredictoR")
  predictoR <- PredictoR(predictoRParams)

  loginfo("Main: executing PredictoR")
  Execute(predictoR)

  loginfo("Main: end")
}

Main()
