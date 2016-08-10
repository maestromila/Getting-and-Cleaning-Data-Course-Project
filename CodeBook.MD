# Getting and Cleaning Data Course Project

CodeBook contains additional information about the variables, data and transformations used in the Peer Graded Assignment: Getting and Cleaning Data Course Project.

# Data
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) 
wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, 
we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 
The experiments have been video-recorded to label the data manually. The obtained dataset has been 
randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% 
the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled 
in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, 
which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body 
acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter 
with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables 
from the time and frequency domain.

A full description of the data used in this assigment can found at http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Data can be dowonloaded at: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# Step 1. Merge the training and the test sets to create one data set.
After working directory is set and workspace has been cleaned, data is read into a several tables.
Data is read from:
features.txt
activity_labels.txt
subject_train.txt
x_train.txt
y_train.txt
subject_test.txt
x_test.txt
y_test.txt

Then column names are labled and data is merged into one data set.

# Step 2. Extract only the measurements on the mean and standard deviation for each measurement.
With grepl function the measurements on the mean and standard deviation are extraced and stored into a new dataframe.

# Step 3. Use descriptive activity names to name the activities in the data set
With function left join, data set created in step 2 is joined with activity dataframe.

# Step 4. Appropriately label the data set with descriptive variable names.
With function gsub variable names are changed.

# Step 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
Aggregate and write.table functions are used to create a second tidy data set.








