library(dplyr)
library(tidyr)
library(data.table)

#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.

#download file url
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#will create a 'data' folder, download/unzip the file, set directory to the unzipped file
if (!file.exists("data")) {
    dir.create("data")
    setwd(paste0(getwd(), "/data"))
    download.file(fileurl, "UCI.zip")
    unzip("UCI.zip")
    setwd(paste0(getwd(), "/UCI HAR Dataset"))
}

#variable names for the x_test & y_test filesets
features <- read.table("features.txt")

#create the test data
subject_test <- read.table("test/subject_test.txt")
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")

#create the train data
subject_train <- read.table("train/subject_train.txt")
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")

df1 <- rbind(x_test, x_train)
df2 <- rbind(subject_test, subject_train)
df3 <- rbind(y_test, y_train)

#set headers
names(df1) <- as.character(features$V2)
names(df2) <- "subjectID"
names(df3) <- "activityLabel"

#merge the test and train data together
mergedf <- cbind(df1, df2, df3)

#get dataframes with mean/std in the variable name
meandata <- mergedf[grepl("mean|std", names(mergedf))]
subjactivity <- subset(mergedf, select = c("subjectID", "activityLabel"))

#data frame of means and STD
meanstd <- cbind(subjactivity, meandata)

#apply activity label
meanstd$activityLabel <- as.factor(meanstd$activityLabel)
activity <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
levels(meanstd$activityLabel) <- activity

#change column names 
colnames(meanstd) <- sub("tBody", "TimeBody", colnames(meanstd))
colnames(meanstd) <- sub("fBody", "FreqBody", colnames(meanstd))
colnames(meanstd) <- gsub("[-]", ".", colnames(meanstd))
colnames(meanstd) <- gsub("[()]", "", colnames(meanstd))

#average of each variable by activity and subject
meanstd <- meanstd %>% 
                group_by(activityLabel, subjectID) %>% 
                        summarize_all(funs(mean))