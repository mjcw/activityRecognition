tidyActivityRecognitionData <- function(){
  # 1. Merges the training and the test sets to create one data set
  allData <- mergeTrainingAndTestData()
  
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement
  columnNames <- names(allData)
  filter <- grepl("mean\\(\\)", columnNames) | 
            grepl("std\\(\\)", columnNames) | 
            grepl("activity", columnNames) | 
            grepl("subject", columnNames)
  dataSubset <- allData[,filter]
  
  # 3. Uses descriptive activity names to name the activities in the data set
  dataSubset$activity <- factor(dataSubset$activity, labels=activityLabels[,2])
  
  
  dataSubset
}


mergeTrainingAndTestData <- function(){
  trainingData <- readTrainingData()
  testData <- readTestData()
  
  combinedData <- rbind(trainingData, testData)
  
  combinedData
}

#' Reads the training data under the data/training directory
readTrainingData <- function(){
  trainingData <- readData(dataFile = "data/train/X_train.txt", 
                   subjectsFile = "data/train/subject_train.txt", 
                   activityLabelsFile = "data/train/y_train.txt")
  trainingData
}

#' Reads the test data under the data/test directory
readTestData <- function(){
  testData <- readData(dataFile = "data/test/X_test.txt", 
                           subjectsFile = "data/test/subject_test.txt", 
                           activityLabelsFile = "data/test/y_test.txt")
  testData
}

#' This function reads the data by combining dataset with the subjects and activities.
#' 
#' @param dataFile path of the data set file
#' @param subjectsFile path of the subjects file
#' @param activityLabelsFile path of the activity labels file 
#' 
#' @return data table with subjects, activity and features
readData <- function(dataFile, subjectsFile, activityLabelsFile){
  dataPoints <- read.table(dataFile)
  features <- read.table("data/features.txt")
  names(dataPoints) <- features[,2]
  
  # read subjects
  subjects <- read.table(subjectsFile)
  names(subjects) <- c("subjects")
  
  # read activity data
  activityData <- read.table(activityLabelsFile)
  names(activityData) <- c("activity")
  
  # join data by binding columns
  data <- cbind(subjects, activityData, dataPoints)
  
  # return training data
  data
}