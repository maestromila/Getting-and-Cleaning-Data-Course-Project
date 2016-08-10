###########################################################################
## Name script: run_analysis.R                                           ##
## Peer Graded Assignment: Getting and Cleaning Data Course Project      ##
## Date: 09-08-2016                                                      ##    
## Author: M. Hadziomerovic                                              ##  
## Version: final 1.0                                                    ##  
###########################################################################

# Set working dir
setwd("~/Kaggle/Coursera/Course 3 Getting and cleaning data")

# Clean up the workspace
rm(list = ls())

# Load library
library("dplyr")

# Read the data from zip file
xTrain        <- read.table(file = "UCI HAR Dataset/train/X_train.txt", header = FALSE) 
yTrain        <- read.table(file = "UCI HAR Dataset/train/y_train.txt", header = FALSE)
xTest         <- read.table(file = "UCI HAR Dataset/test/X_test.txt", header = FALSE)
yTest         <- read.table(file = "UCI HAR Dataset/test/y_test.txt", header = FALSE)
subjecttrain  <- read.table(file = "UCI HAR Dataset/train/subject_train.txt", header = FALSE) 
subjecttest   <- read.table(file = "UCI HAR Dataset/test/subject_test.txt", header = FALSE)
features      <- read.table(file = "UCI HAR Dataset/features.txt", header = FALSE)
activity      <- read.table(file = "UCI HAR Dataset/activity_labels.txt", header = FALSE)

# Label column names 
colnames(activity)      <- c('activityId','activityType')
colnames(subjecttrain)  <- "subjectId"
colnames(subjecttest)   <- "subjectId"
colnames(xTrain)        <- features[,2]
colnames(yTrain)        <- "activityId"
colnames(xTest)         <- features[,2]
colnames(yTest)         <- "activityId"

# Combine train data
dt_train <- cbind(xTrain, yTrain, subjecttrain)
# Combine test data
dt_test  <- cbind(xTest, yTest, subjecttest)
# 1. Merge the training and the test sets to create one data set.
final_data <- rbind(dt_train, dt_test)

# 2. Extract only the measurements on the mean and standard deviation for each measurement.
sub_data_mean_std <- final_data[grepl("activity", colnames(final_data)) | 
                                grepl("subject", colnames(final_data)) |
                                grepl("mean()", colnames(final_data)) & !grepl("meanFreq", colnames(final_data)) |
                                grepl("std()", colnames(final_data)) == TRUE]


# 3. Use descriptive activity names to name the activities in the data set
sub_data_mean_std <- left_join(sub_data_mean_std, activity, by = "activityId")

# 4. Appropriately label the data set with descriptive variable names.
# Remove parentheses
names(sub_data_mean_std) <- gsub('\\(|\\)',"",names(sub_data_mean_std), perl = TRUE)
# Make clearer names
names(sub_data_mean_std) <- gsub('Acc',"Acceleration",names(sub_data_mean_std))
names(sub_data_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(sub_data_mean_std))
names(sub_data_mean_std) <- gsub('Gyro',"AngularSpeed",names(sub_data_mean_std))
names(sub_data_mean_std) <- gsub('Mag',"Magnitude",names(sub_data_mean_std))
names(sub_data_mean_std) <- gsub('^t',"TimeDomain.",names(sub_data_mean_std))
names(sub_data_mean_std) <- gsub('^f',"FrequencyDomain.",names(sub_data_mean_std))
names(sub_data_mean_std) <- gsub('\\.mean',".Mean",names(sub_data_mean_std))
names(sub_data_mean_std) <- gsub('\\.std',".StandardDeviation",names(sub_data_mean_std))
names(sub_data_mean_std) <- gsub('Freq\\.',"Frequency.",names(sub_data_mean_std))
names(sub_data_mean_std) <- gsub('Freq$',"Frequency",names(sub_data_mean_std))

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
sub_avg_by_act_sub <- aggregate(sub_data_mean_std[,1:66],by=list(sub_data_mean_std$activityId, sub_data_mean_std$subjectId), mean)
write.table(sub_avg_by_act_sub, file = "sub_avg_by_act_sub.txt")

