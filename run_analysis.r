###################### Peer-graded Assignment: Getting and Cleaning Data Coure Project #######################

############################################# Get Data #######################################################
# First Load necessary libraries
library(dplyr)
# Download data set for assignment  
filename = "Coursera_DS3_Final.zip"
# Check archieve
if (!file.exists(filename)){
  fileURL = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  
# Check folder
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# name codes in the zip folder for future analysis 
features = read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities = read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

############## Step 1: Merges the training and the test sets to create one data set.##########################
# First Bind by rows for like data, then bind across all columns 
X = rbind(x_train = read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions), read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions))
Y = rbind(read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code"), read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code"))
Subject = rbind(read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject"), read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject"))
Merged_Data = cbind(Subject, Y, X)

####### Step 2: Extracts only the measurements on the mean and standard deviation for each measurement.#######
# create tidy data by only holding what data is necessary for further analysis
TidyData = Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

######## Step 3: Uses descriptive activity names to name the activities in the data set. ####################
TidyData$code = activities[TidyData$code, 2]

############ Step 4: Appropriately labels the data set with descriptive variable names.######################
# Name the variables so they will be easier for human readability 
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))


########### Step 5: From the data set in step 4, creates a second, independent tidy data set ################
###########      with the average of each variable for each activity and each subject.       ################

FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)

########### Check the final data for names, type, df, and head first few rows. ###############################
str(FinalData)
