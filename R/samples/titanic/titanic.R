#!/usr/bin/env Rscript

################################################################################
# Titanic (Kaggle) Predictor
################################################################################

################################################################################
# External dependencies
################################################################################

for (.requirement in c("data.table", "devtools", "logging", "stringi")) {
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

source("R/PredictoR.R")

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
kSubmissionFileName <- "R/samples/titanic/submission.csv"

# Used on feature engineering
kTitles <- c("capt", "col", "don", "dr", "major", "master", "miss", "mlle",
             "mr", "mrs", "rev")
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

  if (feature == "age2") {
    # fill missing ages using title than global mean
    y <- x[, age]
    for(title in paste0("title.", kTitles)) {
      y[ is.na(y) & x[, get(title) == 1]] <- mean( x[ get(title) == 1 & !is.na(age), age] )
    }
    # global
    y[ is.na(y) ] <- mean( x[ !is.na(age), age] )
  }

  if (feature == "lastnamecount") {
    # last name family count
    nameTokens <- strsplit(x[, name], ", ")
    lastname <- sapply(nameTokens, FUN=function(a) { stri_trim(a[1]) })
    lastnametbl <- data.table(lastname = lastname)
    lastnamecount <- lastnametbl[, list(lastnamecount=.N), by=lastname]
    tmp <- merge(x=lastnametbl[, list(lastname)],
                 y=lastnamecount,
                 by.x="lastname",
                 by.y="lastname")
    y <- tmp[, lastnamecount]
  }

  loginfo("BuildFeature: end")
  return (y)
}

GetFeaturesMetadata <- function() {
  features <- data.table(feature=c("pclass",
                                   "sex",
                                   "sibsp",
                                   "parch",
                                   "fare",
                                   paste0("cabin.", kCabinLetters),
                                   paste0("title.", kTitles),
                                   "age2",
                                   "lastnamecount"))
  return (features)
}

GetModelsMetadata <- function() {
  # individual scenarios
  sampleFactor <- 1
  sampleSeed <- 1994
  folds <- 100
  trainFolds <- c(25:75)

  # build all combinations for rpart
  minsplit <- ((1:10)*10)
  rpartModels <- CJ(sampleFactor=sampleFactor,
                    sampleSeed=sampleSeed,
                    folds=folds,
                    trainFolds=trainFolds,
                    model="rpart",
                    method="class",
                    minsplit=minsplit)

  # build all combinations for randomForest
  ntree <- ((1:10)*10)
  ranfomForestModels <- CJ(sampleFactor=sampleFactor,
                           sampleSeed=sampleSeed,
                           folds=folds,
                           trainFolds=trainFolds,
                           model="randomForest",
                           method="class",
                           ntree=ntree)

   # build all combinations for xgboost
   nround <- ((1:10)*10)
   xgboostModels <- CJ(sampleFactor=sampleFactor,
                            sampleSeed=sampleSeed,
                            folds=folds,
                            trainFolds=trainFolds,
                            model="xgboost",
                            objective="binary:logistic",
                            nround=nround)

  return (rbindlist(list(rpartModels,
                         ranfomForestModels,
                         xgboostModels), use.names=TRUE, fill=TRUE))
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

  # factorization
  x[, embarked := as.factor(embarked) ]
  x[, sex := as.factor(sex) ]
  if ("survived" %in% colnames(x)) {
    x[, survived := as.factor(survived) ]
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
  y <- fread(kTestFileName)
  y <- PreProcess(y)
  loginfo("GetTestData: end")
  return (y)
}

NormalizeResponse <- function(response) {
  if (is.factor(response)) {
    response[is.na(response)] <- as.factor(0)
  } else {
    response[response < .5 ] <- 0
    response[response >= .5 ] <- 1
    response <- as.factor(response)
  }
  return (response)
}

Evaluate <- function(prediction, expected) {
  return (length(expected[prediction == expected]) / length(expected))
}

################################################################################
# Main Flow
################################################################################

loginfo("Main: begin")

loginfo("Main: creating PredictoRParams")
predictoRParams <- PredictoRParams(idColName="passengerid",
                                   responseColName="survived",
                                   featuresMetadata=GetFeaturesMetadata(),
                                   modelsMetadata=GetModelsMetadata(),
                                   buildFeature=BuildFeature,
                                   getTrainData=GetTrainData,
                                   getTestData=GetTestData,
                                   evaluate=Evaluate,
                                   normalizeResponse=NormalizeResponse)
loginfo(capture.output(predictoRParams))

loginfo("Main: creating PredictoR")
predictoR <- PredictoR(predictoRParams)

loginfo("Main: executing PredictoR")
output <- Execute(predictoR)

loginfo("Main: saving submission")
write.csv(output$prediction, kSubmissionFileName, row.names=FALSE)

loginfo("Main: end")
