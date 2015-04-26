## Code Book
# Introduction
Code Book describes the variables, the data, and transformartions performed to clean up the the data.

Basicly there is one script called run_analysis.R to process the data. It performs the 5 steps described here or in the course project's definition:
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Steps
* There are seperate files called x_train, x_test..etc. First, all of the data is read and merged into one data set using cbind(column bind) and rbind(row bind) functions. To do that, it's created a traing_data and test_data variables that includes only information related to it's name. For instance, training data consists of training data sets such as x_train, y_train, subject id, activity id. Then it's merged into one data set variable called data. After that, it's labeled appropriate name using the features file.  
* Then it's extracted the only mean and standard deviation for each measurement. Grepl function is used in this matter. The usage of it is for regular expression. It's extracted the feature including the name of (features names with labels that contain "mean" or "std"..etc) std (standard deviation), mean (average), id (activity id) and subject. ID is used for step 3 for joining between the data with the activity data. 
* In step 3, data is joined with the activity data for getting activity names corresponding activity id.
* Then, it's dropped the id column because it's not necessary for the end user. The remaining columns are already labeled.
* It's calculated the the average of each variable and generated a variable called result based on this second data set. It's written into a file named tidy_data.txt.


# Variables
x_train, y_train, subject_train, x_test, y_test, subject_test, features, activity_labels are used for reading files that related to file name.
training_data: training data set
test_data: test data set
data: one merged data set
colNum: number of feature's column.
labels: Data set labels
data_agg: Extracted data that variable with labels contain mean or std.
result: The second data set waiting for writing a file.


# Data Set Description
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz.
Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).
Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals).
These signals were used to estimate variables of the feature vector for each pattern:
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

The set of variables that were estimated (and kept for this assignment) from these signals are:
mean(): Mean value
std(): Standard deviation

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean

Note: features are normalized and bounded within [-1,1].

Result data set contains 10299 observations with 81 variables. 
Subject: An identifier od the subject id.
Activity: Activity label such as WALKING, WALKING_UPSTAIRS etc.
tBodyAcc-mean()-X: TimeDomain.BodyAcceleration.X Direction Mean
tBodyAcc-mean()-Y: TimeDomain.BodyAcceleration.Y Direction Mean
tBodyAcc-mean()-Z: TimeDomain.BodyAcceleration.Z Direction Mean
tBodyAcc-std()-X: TimeDomain.BodyAcceleration.X Direction Standard Deviation
tBodyAcc-std()-Y...
tBodyAcc-std()-Z
tGravityAcc-mean()-X
tGravityAcc-mean()-Y
tGravityAcc-mean()-Z
tGravityAcc-std()-X
tGravityAcc-std()-Y
tGravityAcc-std()-Z
tBodyAccJerk-mean()-X
tBodyAccJerk-mean()-Y
tBodyAccJerk-mean()-Z
tBodyAccJerk-std()-X
tBodyAccJerk-std()-Y
tBodyAccJerk-std()-Z
tBodyGyro-mean()-X
tBodyGyro-mean()-Y
tBodyGyro-mean()-Z
tBodyGyro-std()-X
tBodyGyro-std()-Y
tBodyGyro-std()-Z
tBodyGyroJerk-mean()-X
tBodyGyroJerk-mean()-Y
tBodyGyroJerk-mean()-Z
tBodyGyroJerk-std()-X
1tBodyGyroJerk-std()-Y
1tBodyGyroJerk-std()-Z
tBodyAccMag-mean()
tBodyAccMag-std()
tGravityAccMag-mean()
tGravityAccMag-std()
tBodyAccJerkMag-mean()
tBodyAccJerkMag-std()
tBodyGyroMag-mean()
tBodyGyroMag-std()
tBodyGyroJerkMag-mean()
tBodyGyroJerkMag-std()
fBodyAcc-mean()-X
fBodyAcc-mean()-Y
fBodyAcc-mean()-Z
fBodyAcc-std()-X
fBodyAcc-std()-Y
fBodyAcc-std()-Z
fBodyAcc-meanFreq()-X
fBodyAcc-meanFreq()-Y
fBodyAcc-meanFreq()-Z
fBodyAccJerk-mean()-X
fBodyAccJerk-mean()-Y
fBodyAccJerk-mean()-Z
fBodyAccJerk-std()-X
fBodyAccJerk-std()-Y
fBodyAccJerk-std()-Z
fBodyAccJerk-meanFreq()-X
fBodyAccJerk-meanFreq()-Y
fBodyAccJerk-meanFreq()-Z
fBodyGyro-mean()-X
fBodyGyro-mean()-Y
fBodyGyro-mean()-Z
fBodyGyro-std()-X
fBodyGyro-std()-Y
fBodyGyro-std()-Z
fBodyGyro-meanFreq()-X
fBodyGyro-meanFreq()-Y
fBodyGyro-meanFreq()-Z
fBodyAccMag-mean()
fBodyAccMag-std()
fBodyAccMag-meanFreq()
fBodyBodyAccJerkMag-mean()
fBodyBodyAccJerkMag-std()
fBodyBodyAccJerkMag-meanFreq()
fBodyBodyGyroMag-mean()
fBodyBodyGyroMag-std()
fBodyBodyGyroMag-meanFreq()
fBodyBodyGyroJerkMag-mean()
fBodyBodyGyroJerkMag-std()
fBodyBodyGyroJerkMag-meanFreq()

mean(): Mean value
std(): Standard deviation 