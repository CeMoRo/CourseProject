## Reading files

features <- read.table("features.txt")

subject_test <- read.table("subject_test.txt")

y_test <- read.table("y_test.txt")

test <- read.table("X_test.txt")

subject_train <- read.table("subject_train.txt")

y_train <- read.table("y_train.txt")

train <- read.table("X_train.txt")

## Putting column names from the features dataset

test <- `colnames<-`(test, features$V2)

train <- `colnames<-`(train, features$V2)

## Adding activity label column

test <- cbind(y_test, test)

colnames(test)[1] <- "activity"

train <- cbind(y_train, train)

colnames(train)[1] <- "activity"

## Adding subject column

test <- cbind(subject_test, test)

colnames(test)[1] <- "subject"

train <- cbind(subject_train, train)

colnames(train)[1] <- "subject"

## 1. Merges the training and the test sets to create one data set.

merged_data <- rbind(test, train, deparse.level = 0, make.row.names = FALSE)


## 2. Extracts only the measurements on the mean and standard deviation 
##     for each measurement.

meancols <- c(grep("mean", colnames(merged_data)))
stdcols <- c(grep("std", colnames(merged_data)))
mean_std <- merged_data[, c(1, 2, meancols, stdcols)]

## 3. Uses descriptive activity names to name the activities in the data set

mean_std$actdescr <- ifelse(mean_std$activity == 1, "WALKING", 
                            ifelse(mean_std$activity==2, "WALKING_UPSTAIRS", 
                                   ifelse(mean_std$activity==3, "WALKING_DOWNSTAIRS", 
                                          ifelse(mean_std$activity==4, "SITTING", 
                                                 ifelse(mean_std$activity==5, "STANDING",
                                                        ifelse(mean_std$activity==6, "LAYING", 0))))))

mean_std <- mean_std[,c(82,2,1,3:81)]


## 4. Appropriately labels the data set with descriptive variable names.

descriptive_names <- gsub("tBodyAccJerkMag", "acc_jerk_magnitude", names(mean_std))

descriptive_names <- gsub("tBodyGyroJerkMag", "gyro_jerk_magnitude", descriptive_names) 

descriptive_names <- gsub("fBodyAccJerkMag", "fourier_acc_jerk_mag", descriptive_names) 

descriptive_names <- gsub("fBodyGyroJerkMag", "fourier_gyro_jerk_magnitude", descriptive_names) 

descriptive_names <- gsub("tBodyAccMag", "acc_magnitude", descriptive_names) 
  
descriptive_names <- gsub("tBodyAccJerk", "acc_jerk", descriptive_names) 

descriptive_names <- gsub("tBodyGyroJerk", "gyro_jerk", descriptive_names) 

descriptive_names <- gsub("tGravityAccMag", "acc_gravity_magnitude", descriptive_names) 

descriptive_names <- gsub("tBodyGyroMag", "gyro_magnitude", descriptive_names) 

descriptive_names <- gsub("fBodyAccJerk", "fourier_acc_jerk", descriptive_names) 

descriptive_names <- gsub("fBodyAccMag", "fourier_acc_magnitude", descriptive_names) 

descriptive_names <- gsub("fBodyGyroMag", "fourier_gyro_magnitude", descriptive_names) 

descriptive_names <- gsub("tBodyAcc", "acc_raw", descriptive_names)

descriptive_names <- gsub("tGravityAcc", "acc_grav_raw", descriptive_names)

descriptive_names <- gsub("fBodyAcc", "fourier_acc_raw", descriptive_names)

descriptive_names <- gsub("fBodyGyro", "fourier_gyro_raw", descriptive_names)


colnames(mean_std) <- descriptive_names

## 5. From the data set in step 4, creates a second, independent tidy data set with the 
##  average of each variable for each activity and each subject.

library(dplyr)

grup <- group_by_at(as.tbl(mean_std), vars(subject, activity))

tabl <- summarise_all(grup, funs(mean))


data<- tabl[,-3]

data$actdescr <- ifelse(data$activity == 1, "WALKING", 
                            ifelse(data$activity==2, "WALKING_UPSTAIRS", 
                                   ifelse(data$activity==3, "WALKING_DOWNSTAIRS", 
                                          ifelse(data$activity==4, "SITTING", 
                                                 ifelse(data$activity==5, "STANDING",
                                                        ifelse(data$activity==6, "LAYING", 0))))))

data <- data[,c(82,1:81)]

data <- data[,c(1,2,82,3:81)]



