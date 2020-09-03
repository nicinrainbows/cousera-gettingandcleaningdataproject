# 1. Merge the training and test sets to create one data set
# 2. Extract only the measurements on mean & st dev for each measurement
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set w descriptive variable names
# 5. From data set in step #4, create a 2nd, independent tidy data set w the
#     average of each variable for each activity & each subject.

library(dplyr)
library(datasets)

## Download the data set into the folder and unzip it
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")
unzip(zipfile="./data/Dataset.zip",exdir="./data")

## Ensuring that all the files are there
list.files(file.path("./data","UCI HAR Dataset"))
list.files(file.path("./data","UCI HAR Dataset/test"))
list.files(file.path("./data","UCI HAR Dataset/train"))

mainPath <- file.path("./data", "UCI HAR Dataset")
testPath <- file.path("./data","UCI HAR Dataset/test")
trainPath <- file.path("./data","UCI HAR Dataset/train")

## Extract, read & analyze data in features files
dataFeaturesTrain <- read.table(file.path(trainPath, "X_train.txt"), header = FALSE)
dataFeaturesTest <- read.table(file.path(testPath, "X_test.txt"), header = FALSE)
## both should be numeric
str(dataFeaturesTrain)
str(dataFeaturesTest)

## Extract, read & analyze data in subject files
dataSubjectTrain <- read.table(file.path(trainPath, "subject_train.txt"), header = FALSE)
dataSubjectTest <- read.table(file.path(testPath, "subject_test.txt"), header = FALSE)
## both should be integer
str(dataSubjectTrain)
str(dataSubjectTest)

## Extract, read & analyze data in activity files
dataActivityTrain <- read.table(file.path(trainPath, "y_train.txt"), header = FALSE)
dataActivityTest <- read.table(file.path(testPath, "y_test.txt"), header = FALSE)
## both should be integer
str(dataActivityTrain)
str(dataActivityTest)

#### 1. Merge the training and test sets to create one data set ####

## Concatenate all rows in the 3 variables
dataFeatures <- rbind(dataFeaturesTest, dataFeaturesTrain)
dataSubject <- rbind(dataSubjectTest, dataSubjectTrain)
dataActivity <- rbind(dataActivityTest, dataActivityTrain)

## Set column names
names(dataSubject) <- c("Subject")
names(dataActivity) <- c("Activity")
## For dataFeatures, first read features.txt before setting the name
dataFeaturesName <- read.table(file.path(mainPath, "features.txt"), header = FALSE)
names(dataFeatures) <- dataFeaturesName$V2

## Concatenate all columns from the 3 combined data sets
dataMerge <- cbind(cbind(dataSubject, dataActivity), dataFeatures)


#### 2. Extract only the measurements on mean & st dev for each measurement ####
theNames <- grep("-(mean|std).*",dataFeaturesName$V2)
## Assuming the data subset requires the subject & activity too...
theNamesData <- dataFeatures[,theNames]
filteredData <- cbind(cbind(dataSubject, dataActivity), theNamesData)


#### 3. Use descriptive activity names to name the activities in the data set ####
## Read the activity file then add a column for labels
activityTable <- read.table(file.path(mainPath, "activity_labels.txt"), header = FALSE)
filteredData$Activity <- factor(filteredData$Activity, activityTable$V1,
                                activityTable$V2)


#### 4. Appropriately label the data set w descriptive variable names ####

names(filteredData) <- gsub("^t", "time", names(filteredData))
names(filteredData) <- gsub("^f", "frequency", names(filteredData))
names(filteredData) <- gsub("Acc", "Accelerometer", names(filteredData))
names(filteredData) <- gsub("Gyro", "Gyroscope", names(filteredData))
names(filteredData) <- gsub("Mag", "Magnitude", names(filteredData))


#### 5. create a 2nd, independent tidy data set ####
#### w/ the average of each variable for each activity & each subject. ####

secondData <- filteredData %>%
                 group_by(Activity, Subject) %>%
                 summarise_all(mean)

write.table(secondData, file = "newTidyData.txt", row.name = FALSE)