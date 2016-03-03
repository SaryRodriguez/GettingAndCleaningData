###LIBRARIES
library(data.table)
library(dplyr)

##SET WORKING DIRECTORY
setwd("~/Coursera/Getting and Cleaning Data")
if(!file.exists("Proyecto")){
  dir.create("Proyecto")
}

#DOWNLOAD FILES 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Proyecto/data.zip")
unzip('./Proyecto/data.zip')


#LOADING FILES

xTrain <- read.table('./UCI HAR Dataset/train/X_train.txt', header=FALSE) #FEATURES TRAIN
xTest <- read.table('./UCI HAR Dataset/test/X_test.txt', header=FALSE)  #FEATURES TEST

subjTrain <- read.table('./UCI HAR Dataset/train/subject_train.txt', header=FALSE) 
subjTest <- read.table('./UCI HAR Dataset/test/subject_test.txt', header=FALSE)

yTrain <- read.table('./UCI HAR Dataset/train/y_train.txt', header=FALSE) #ACTIVITY TRAIN 
yTest <- read.table('./UCI HAR Dataset/test/y_test.txt', header=FALSE) #ACTIVITY TEST

features <- read.table('./UCI HAR Dataset/features.txt') #FEATURE NAMES
activityLabels <- read.table('./UCI HAR Dataset/activity_labels.txt', header=FALSE)



## 1. MERGE THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET 

x <- rbind(xTrain, xTest) #FEATURES
subj <- rbind(subjTrain, subjTest) #SUBJECT
y <- rbind(yTrain, yTest) #ACTIVITY
#Naming columns
colnames(x) <- t(features[2])
colnames(y) <- "Activity"
colnames(subj) <- "Subject"
data <- cbind(x,y,subj)


#2. EXTRAXTS ONLY THE MEASUREMENTS ON THE MEAN AND THE STANDARD DEVIATION FOR EACH MEASUREMENT


meanSD <- grep("-mean\\(\\)|-std\\(\\)", names(data), ignore.case=TRUE)
#Add activity and subject columns 
requiredColumns <- c(meanSD, 562, 563)
data <- data[,requiredColumns]


#3. USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET


data$Activity <- as.character(data$Activity)
data$Activity[data$Activity == 1] <- "Walking"
data$Activity[data$Activity == 2] <- "Walking Upstairs"
data$Activity[data$Activity == 3] <- "Walking Downstairs"
data$Activity[data$Activity == 4] <- "Sitting"
data$Activity[data$Activity == 5] <- "Standing"
data$Activity[data$Activity == 6] <- "Laying"
data$Activity <- as.factor(data$Activity)



#4. APPROPRIATELY LABELS THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES 
names(data) <- gsub("Acc", "Accelerator", names(data))
names(data) <- gsub("Mag", "Magnitude", names(data))
names(data) <- gsub("Gyro", "Gyroscope", names(data))
names(data) <- gsub("^t", "time", names(data))
names(data) <- gsub("^f", "frequency", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))
names(data)<-gsub("tBody", "TimeBody", names(data))
names(data)<-gsub("-freq()", "Frequency", names(data), ignore.case = TRUE)


data$participants <- as.character(data$participants)
for (i in 1:30){
  data$participants[data$participants==i]<- paste("participant",i, sep=" ")
}
data$participants <- as.factor(data$participants)


#5.FROM THE DATA SET IN STEP 4, CREATES A SECOND, INDEPENDENT TIDY DATA SET WITH THE AVERAGE OF EACH
#VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT

data$Subject <- as.factor(data$Subject)
data <- data.table(data)

tidyData <- aggregate(. ~Subject + Activity, data, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)