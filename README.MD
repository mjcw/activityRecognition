Tidy data for Activiy Recognition data set
=========
This course project uses the data collected from the accelerometers from the Samsung Galaxy S smartphone for Activity Recognition and generates a tidy data set.

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

Main function of the script is ```tidyActivityRecognitionData()```



