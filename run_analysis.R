library(dplyr)

filename <- "getdata-projectfiles-UCI HAR Dataset.zip"

## Download and unzip the dataset
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
  download.file(fileURL, filename, method="curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

##load all data sources
feature <- read.table("UCI HAR Dataset/features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
training_set <- read.table("UCI HAR Dataset/train/X_train.txt")
training_labels <- read.table("UCI HAR Dataset/train/y_train.txt")
test_set <- read.table("UCI HAR Dataset/test/X_test.txt")
test_labels <- read.table("UCI HAR Dataset/test/y_test.txt")
subject_training <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")

##Extracts only the measurements on the mean and standard deviation for each measurement.
featureMeanStd <- grep("(mean|std)\\(\\)", feature[,2])
featureMeanStd.names <- feature[featureMeanStd,2]

##Uses descriptive activity names to name the activities in the data set
training_labels <- merge(x=training_labels,y=activity_labels, by = "V1", all.x = TRUE)
test_labels <- merge(x=test_labels,y=activity_labels, by = "V1", all.x = TRUE)

##merge subject, actvity, and the data set for training data
train_data <- cbind(subject_training, training_labels, training_set[featureMeanStd])

##merge subject, actvity, and the data set for test data
test_data <- cbind(subject_test, test_labels, test_set[featureMeanStd])

##merge training and test data
all_data <- rbind(train_data,test_data)

##Appropriately labels the data set with descriptive variable names
featureMeanStd.names <- gsub("[-()]","",featureMeanStd.names)
featureMeanStd.names <- gsub("mean","Mean",featureMeanStd.names)
featureMeanStd.names <- gsub("std","Std",featureMeanStd.names)
colnames(all_data) <- c("subject","activity", "activity_desc", featureMeanStd.names)


##Calculate average value for all measurement group by activity and subject
avg_all_data <- all_data %>%
  select(-activity) %>%
  group_by(activity_desc, subject) %>%
  summarise_each(funs(mean))

##Export to file
write.table(avg_all_data, row.names = FALSE, file = "tidy.txt")
