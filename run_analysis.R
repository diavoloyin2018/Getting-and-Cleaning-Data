install.packages("reshape2")
install.packages("dplyr")
library(reshape2)
library(dplyr)

setwd("E:/Data Science Specialisation/Clean Data")

zipurl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if(!file.exists("raw data.zip")){
  
  download.file(zipurl,"raw data.zip",method = "curl")
  
}

unzip("raw data.zip")

##1, Merges the training and the test sets to create one data set.

trainSubjects<-read.table("UCI HAR Dataset/train/subject_train.txt")
trainValues<-read.table("UCI HAR Dataset/train/X_train.txt")
trainActivity<-read.table("UCI HAR Dataset/train/Y_train.txt")

testSubjects<-read.table("UCI HAR Dataset/test/subject_test.txt")
testValues<-read.table("UCI HAR Dataset/test/X_test.txt")
testActivity<-read.table("UCI HAR Dataset/test/Y_test.txt")

humanActivity<-rbind(
  cbind(trainSubjects,trainValues,trainActivity),
  cbind(testSubjects,testValues,testActivity)
)

head(humanActivity)

##2, Extracts only the measurements on the mean and standard deviation for each measurement.

features<-read.table("UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)
activities<-read.table("UCI HAR Dataset/activity_labels.txt",stringsAsFactors = FALSE)

colnames(humanActivity)<-c("Subject",features[,2],"Activity")

col_keep<-grepl("Subject|Activity|mean|std", colnames(humanActivity))

humanActivity<-humanActivity[,col_keep]

str(humanActivity)

##3, Uses descriptive activity names to name the activities in the data set

humanActivity$Activity<-factor(humanActivity$Activity,levels = activities[,1],labels = activities[,2])


##4, Appropriately label the data set with descriptive variable names


cols<-colnames(humanActivity)

cols<-gsub('[\\(\\)-]','',cols)
cols <- gsub("^f", "frequencyDomain", cols)
cols <- gsub("^t", "timeDomain", cols)
cols <- gsub("Acc", "Accelerometer", cols)
cols <- gsub("Gyro", "Gyroscope",cols)
cols <- gsub("Mag", "Magnitude", cols)
cols <- gsub("Freq", "Frequency", cols)
cols <- gsub("mean", "Mean", cols)
cols <- gsub("std", "StandardDeviation", cols)
cols <- gsub("BodyBody", "Body", cols)

colnames(humanActivity)<-cols

##5, From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

humanActivityMeans <- humanActivity %>% 
  group_by(Subject, Activity) %>%
  summarise_all(funs(mean))

write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
