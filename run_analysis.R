library(reshape2)

#' This function generates tidy data set from the Activity Recognition data set.
#' It stores the data in "tidyData.txt" file if outputFileName is not specified.
tidyActivityRecognitionData <- function(outputFileName="tidyData.txt"){
  
  # Read cleaned activity data. readActivityRecognitionData() performs Steps 1-4 of the assignment
  activityData <- readActivityRecognitionData()
  
  # 5. From the data set in step 4, creates a second, independent tidy data set with the 
  #    average of each variable for each activity and each subject.
  columnNames = names(activityData)
  variables = columnNames[getMeanAndStdColumnNames(columnNames)]
  dataMelt <- melt(activityData, id=c("subject", "activity"), measure.vars=variables)
  tidyData <- dcast(dataMelt, subject + activity ~ variable, mean)
  
  # write tidyData to file outputFileName
  write.table(tidyData, outputFileName, row.name=FALSE)
}

#' Reads Activity Recognition data set by merging training and test data sets.
#' This function then extracts only the mean and std column names and finally
#' replaces activity ids by activity names.
#' 
#' This function perfoms Steps 1-4 of the assignment
#' 
#' @return data table after performing the above listed operations.
readActivityRecognitionData <- function(){
  
  # 1. Merges the training and the test sets to create one data set
  allData <- mergeTrainingAndTestData()
  
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement
  filter <- getSubsetColumnNames(names(allData))
  dataSubset <- allData[,filter]
  
  # 3. Uses descriptive activity names to name the activities in the data set
  activityLabels = read.table("data/activity_labels.txt")
  dataSubset$activity <- factor(dataSubset$activity, labels=activityLabels[,2])
  
  # 4. Appropriately labels the data set with descriptive variable names
  # This is already taken care of in readData() function below, where descriptive
  # variable names are read from "data/features.txt" file and assigned to the data table
  
  dataSubset
}

#' Combines Training Data set with Test Data set
#' @return combined data set
mergeTrainingAndTestData <- function(){
  trainingData <- readTrainingData()
  testData <- readTestData()
  
  combinedData <- rbind(trainingData, testData)
  
  combinedData
}

#' Reads the training data under the data/training directory
#' @return Training data set
readTrainingData <- function(){
  trainingData <- readData(dataFile = "data/train/X_train.txt", 
                   subjectsFile = "data/train/subject_train.txt", 
                   activityLabelsFile = "data/train/y_train.txt")
  trainingData
}

#' Reads the test data under the data/test directory
#' @return Test data set
readTestData <- function(){
  testData <- readData(dataFile = "data/test/X_test.txt", 
                           subjectsFile = "data/test/subject_test.txt", 
                           activityLabelsFile = "data/test/y_test.txt")
  testData
}

#' This function reads the data by combining dataset with the subjects and activities.
#' It also assigns column names using data/features.txt file
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
  names(subjects) <- c("subject")
  
  # read activity data
  activityData <- read.table(activityLabelsFile)
  names(activityData) <- c("activity")
  
  # join data by binding columns
  data <- cbind(subjects, activityData, dataPoints)
  
  # return training data
  data
}


#' Uses regex to get a filter for column names with "mean()", "std()", "activity" and "subject"
#' 
#' @param columnNames names of all the columns
#' @return filter with just the columns that have "mean()", "std()", "activity" and "subject" in its name
getSubsetColumnNames <- function(columnNames){
  filter <- getMeanAndStdColumnNames(columnNames) | 
    grepl("activity", columnNames) | 
    grepl("subject", columnNames)
  filter 
}

#' Uses regex to get  a filter for column names with "mean()" and "std()"
#' 
#' @param columnNames names of all the columns
#' @return filter with just the columns that have "mean()" and "std()" in its name
getMeanAndStdColumnNames <- function(columnNames){
  filter <- grepl("mean\\(\\)", columnNames) | 
    grepl("std\\(\\)", columnNames) 
  filter
}
