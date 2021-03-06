---
title: "CodeBook"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

This document is used to decribe the content, structure and layout of the data collection. This code book provides a brief understanding of the code used for 'run_analyse.R' and 'Tidy.txt'. 

Following is the data set used for these analyses:

The data is titled "Human Activity Recognition Using Smartphone". The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The data can be downloaded from the following link :

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


The Data Input has the following Files 

1. X_train.txt : Contains variable features for training
2. X_test.txt : Contains variable features for testing
3. Y_train.txt : Contains the activities corresponding to X_train.txt. 
4. Y_test.txt : Contains the activities corresponding to X_test.txt.
5. subject_train.txt contains information on the subjects from whom data is collected.
6. subject_test.txt contains information on the subjects from whom data is collected.
7. activity_labels.txt contains metadata on the different types of activities.
8. features.txt contains the name of the features in the data sets.

Following are the transformations made to the variables for the data. 

* X_train.txt is read into featuresTrain
* Y_train.txt is read into activityTrain
* X_train.txt is read into featuresTrain.
* y_train.txt is read into activityTrain.
* subject_train.txt is read into subjectTrain.
* X_test.txt is read into featuresTest.
* y_test.txt is read into activityTest.
* subject_test.txt is read into subjectTest.
* features.txt is read into featureNames.
* activity_labels.txt is read into activityLabels.
* All subject files are merged into subject
* All activities are merged into activity
* All features are merged into features
* Names of all features are set in featureNames
* completeData is a merger of subject, activity and features
* requiredColumns is indices of columns that contain std or mean, activity and subject
* extractedData is created with data from columns in requiredColumns.
* Activity coloumn in extractedData is updated with activityLabels. 
* Following accronyms are updated 
** Acc : Accelorometer
** Gyro : Gyroscope
** Mag : Magnitude 
** t : Time 
** f : Frequency
* tidyData is created as a set with average for each activity and subject of extractedData
* tidyData is written into Tidy.txt.