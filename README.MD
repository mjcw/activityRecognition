Tidy data for Activiy Recognition data set
=========
This course project uses the data collected from the accelerometers from the Samsung Galaxy S smartphone for Activity Recognition and generates a tidy data set.

```run_analysis.R``` script uses the data under the data/ folder to generate a tidy data set. This tidy data set is in wide-format. It contains columns for "subject", "activity", and one each for all the columns whose name contains "mean()" or "std()". The output tidy data satisfies the following properties:

1. Each variable measures (columns with "mean()" and "std()" in name) have their own column.
2. Each different observation of the variable ("subject" and "activity" key) is in a different row.
3. There is one table for each kind of variable.

## Files includes
* run_analysis.R: Script that reads Activity Recognition data and generates a tidy data set.
* data folder: Contains data collected from the accelerometers from the Samsung Galaxy S smartphone.
* CodeBook.MD: Code book describing the variables.
* README.MD: This file.

## Using the script
To run the script use the following:

```
> setwd("<directoryOfTheRepo>")
> source('run_analysis.R')
> tidyActivityRecognitionData("tidy.txt")
> data <- read.table("tidy.txt", header=TRUE)
> View(data)
```

## Working of the script

Main function of the script is ```tidyActivityRecognitionData()``` in run_analysis.R. This function performs the following steps:

1. First it reads the Activity Recognition data set using the function ```readActivityRecognitionData```, 
2. Uses ```melt``` of ```reshape2``` to create a long skinny data set with "subject" and "activity" as key,
3. User ```dcast``` function to find average of each variable for each activity and each subject,
4. Write the tidy data to a file.

The ```readActivityRecognitionData()``` function performs the following operations:

1. Reads the training set and test set using ```mergeTrainingAndTestData()```. ```mergeTrainingAndTestData()``` uses ```rbind``` to combine the rows,
2. Extracts only the measurements on the mean and standard deviation for each measurement,
3. Replaces activity ids by activity labels.

```readData()``` is the heart of reading the data set. It reads the feature data set, subject data, and activity data. It then assign column names to each and combines the data using ```cbind```. It is called by ```readTrainingData()``` and ```readTestData()``` to read the training data and test data respectively -- both the functions pass appropriate files names to ```readData()``` to perform the task.







