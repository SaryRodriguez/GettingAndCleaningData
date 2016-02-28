setwd("~/Coursera/Getting and Cleaning Data")
if(!file.exists("Proyecto")){
  dir.create("Proyecto")
}

#download files
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Proyecto/data.zip")
unzip('./Proyecto/data.zip')

#load files
xTrain <- read.table('./UCI HAR Dataset/train/X_train.txt')
xTest <- read.table('./UCI HAR Dataset/test/X_test.txt')
subjTrain <- read.table('./UCI HAR Dataset/train/subject_train.txt')
subjTest <- read.table('./UCI HAR Dataset/test/subject_test.txt')
yTrain <- read.table('./UCI HAR Dataset/train/y_train.txt')
yTest <- read.table('./UCI HAR Dataset/test/y_test.txt')
features <- read.table('./UCI HAR Dataset/features.txt')

## merge data
colnames(xTrain) <- t(features[2])
colnames(xTest) <- t(features[2])
x <- rbind(xTrain, xTest)
duplicated(colnames(x))
x <- x[, !duplicated(colnames(x))]
subj <- rbind(subjTrain, subjTest)
y <- rbind(yTrain, yTest)

#Extracts only the measurements on the mean and standard deviation for each measurement.

meanSD <- grep("-mean\\(\\)|-std\\(\\)", features[, 2]) #both mean and sd
x.mean.sd <- x[, meanSD]

mean <- grep("mean()", names(x), value = FALSE, fixed = TRUE)
InstrumentMean <- x[mean]
STD <- grep("std()", names(x), value = FALSE)
InstrumentSTD <- x[STD]
InstrumentSTDMatrix <- x[STD]


#Uses descriptive activity names to name the activities in the data set
xTrain$activities <- yTrain[, 1]
xTrain$participants <- subjTrain[, 1]
xTest$activities <- yTest[, 1]
xTest$participants <- subjTest[, 1]
data <- rbind(xTrain, xTest)
data$activities <- as.character(data$activities)
data$activities[data$activities == 1] <- "Walking"
data$activities[data$activities == 2] <- "Walking Upstairs"
data$activities[data$activities == 3] <- "Walking Downstairs"
data$activities[data$activities == 4] <- "Sitting"
data$activities[data$activities == 5] <- "Standing"
data$activities[data$activities == 6] <- "Laying"
data$activities <- as.factor(data$activities)


#Appropriately labels the data set with descriptive variable names.
#^t-time; ^f-frequency; Acc-Accelerator; Gyro-Gyroscope; Mag-Magnitude
names(data) <- gsub("Acc", "Accelerator", names(data))
names(data) <- gsub("Mag", "Magnitude", names(data))
names(data) <- gsub("Gyro", "Gyroscope", names(data))
names(data) <- gsub("^t", "time", names(data))
names(data) <- gsub("^f", "frequency", names(data))

data$participants <- as.character(data$participants)
for (i in 1:30){
  data$participants[data$participants==i]<- paste("participant",i, sep=" ")
}
data$participants <- as.factor(data$participants)


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(data.table)
dataTable <- data.table(data)
data <- dataTable[, lapply(.SD, mean), by = 'participants,activities']
