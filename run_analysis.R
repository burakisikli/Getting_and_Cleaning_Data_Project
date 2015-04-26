library(plyr)

## Step 1
## Merge the training and test sets to create one data set
# Load raw data
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")
x_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")
features <- read.table("features.txt", colClasses = c("character"))
activity_labels <- read.table("activity_labels.txt", col.names = c("id", "Activity"))

# Binding the data
training_data <- cbind(cbind(x_train, y_train), subject_train)
test_data <- cbind(cbind(x_test, y_test), subject_test)
data <- rbind(training_data, test_data)

# Label the features
colNum <- ncol(features)
labels <- rbind(rbind(features, c(colNum+1, "id")), c(colNum+2, "Subject"))[,2]
names(data) <- labels


## Step 2
## Extracts only the measurements on the mean and standard deviation for each measurement. 
# Extract the columns containing the name of mean, std, id, Subject. 
# ID is necessary for the join in Step 3
data_agg <- data[,grepl("mean|std|id|Subject", names(data))]


## Step 3
## Uses descriptive activity names to name the activities in the data set
# Label the activity  
data_agg <- join(data_agg, activity_labels, by = "id", match = "first")


## Step 4
## Appropriately labels the data set with descriptive variable names.
# Drop the id column, remaining columns are already named
data_agg <- data_agg[, !(colnames(data_agg) %in% c("id"))]


## Step 5
## From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
# Calculate the the average of each variable
result = ddply(data_agg, c("Subject","Activity"), numcolwise(mean))
# Write to the file
write.table(result, file = "tidy_data.txt",  row.name=FALSE)