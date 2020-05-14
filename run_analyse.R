##opening data formatting packages.

library(data.table)
library(dplyr)

##reading supported metadata

featureNames <- read.table("features.txt")
activityLabels <- read.table("activity_labels.txt")

##format data train as per subject, activity, feature

## reading train data

subjectTrain <- read.table("train/subject_train.txt", header = FALSE)
activityTrain <- read.table("train/y_train.txt", header = FALSE)
featureTrain <- read.table("train/x_train.txt", header = FALSE)

## reading test data

subjectTest <- read.table("test/subject_test.txt", header = FALSE)
activityTest <- read.table("test/y_test.txt", header = FALSE)
featureTest <- read.table("test/x_test.txt", header = FALSE)

#merging both data sets based on same merits 

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featureTrain, featureTest)

## nameing the coloumns in features from the feture names variable 

colnames(features) <- t(featureNames[2])

## adding activity and subjects to coloumn names in features 

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features, activity, subject)

##Part 2
## extracting mean and standard deviation for each mesurement

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

#Adding activity and subject columns
requiredColumns <- c(columnsWithMeanSTD, 562, 563)

#Look at the number of variables in completeData
dim(completeData)
extractedData <- completeData[,requiredColumns]

#Look at the number of variables in extractedData
dim(extractedData)

##Part 3
##using activity names for description

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6) {
      extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

##setting the variable to factor 

extractedData$Activity <- as.factor(extractedData$Activity)

##Part 4
##Appropriately labling the dataset

names(extractedData)

##assigning appropriate names to the variables 

names(extractedData) <- gsub("Acc", "Accelorometer", names(extractedData))
names(extractedData) <- gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData) <- gsub("BodyBody", "Body", names(extractedData))
names(extractedData) <- gsub("Mag", "Magnitude", names(extractedData))
names(extractedData) <- gsub("^t", "Time", names(extractedData))
names(extractedData) <- gsub("^f", "Frequency", names(extractedData))
names(extractedData) <- gsub("tbody", "TimeBody", names(extractedData))
names(extractedData) <- gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData) <- gsub("angle", "Angle", names(extractedData))
names(extractedData) <- gsub("gravity", "Gravity", names(extractedData))

##cheacking names 

names(extractedData)

##Part 5
##Creating an independent data set along with average for every activity and subject

extractedData$Subject <- as.factor(extractedData$Subject)

extractedData <- data.table(extractedData)

##creating a tidy data set 

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)

##Ordering the data set 

tidyData <- tidyData[order(tidyData$Subject, tidyData$Activity), ]

##writing the tidy data into a text file 

write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

