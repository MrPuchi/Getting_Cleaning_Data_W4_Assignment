# Getting and Cleaning Data Course Project
# Juan Zacarias
# 7/18/2018

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
#    of each variable for each activity and each subject.

#-----------------------------------------------------------------------------------------------------
library(dplyr)
# 1. Merges the training and the test sets to create one data set.

setwd("F:/R Practice/UCI HAR Dataset")

features<-data.table::fread("features.txt", header = FALSE)
activity_labels <- data.table::fread("activity_labels.txt", header = FALSE, col.names = c("activityID", "activityType"))



subject_train <- data.table::fread('./train/subject_train.txt',header = FALSE, col.names = 'subjectID')
X_train <- data.table::fread('./train/X_train.txt',header = FALSE, col.names = features$V2)
Y_train <- data.table::fread('./train/Y_train.txt',header = FALSE, col.names = 'activityID')




training_set<- cbind(subject_train,X_train, Y_train)
names(training_set)

subject_test <- data.table::fread('./test/subject_test.txt',header = FALSE, col.names = 'subjectID')
X_test <- data.table::fread('./test/X_test.txt',header = FALSE, col.names = features$V2)
Y_test <- data.table::fread('./test/Y_test.txt',header = FALSE, col.names = 'activityID')

test_set<- cbind(subject_test,X_test, Y_test)

merged_set <- rbind(training_set, test_set)

cols <- colnames(merged_set)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

exc <- (grepl("activity..",cols) | grepl("subject..",cols)|grepl("-mean..",cols) & !grepl("-meanFreq..",cols) 
          & !grepl("mean..-",cols)| grepl("-std..",cols) & !grepl("-std()..-",cols) )

names(merged_set[,exc,with=FALSE])

merged_set2<-merged_set[,exc,with=FALSE]
merged_set2

# 3. Uses descriptive activity names to name the activities in the data set

merged_set3 <- merge(merged_set2,activity_labels,by='activityID',all.x=TRUE)
activity_labels[,2][match(merged_set3$activityID,activity_labels$activityID)] 

cols2<- colnames(merged_set3)

# 4. Appropriately labels the data set with descriptive variable names.

for (i in 1:length(cols2)) 
{
  cols2[i] <- gsub("\\()","",cols2[i])
  cols2[i] <- gsub("-std$","StdDev",cols2[i])
  cols2[i] <- gsub("-mean","Mean",cols2[i])
  cols2[i] <- gsub("^(t)","time",cols2[i])
  cols2[i] <- gsub("^(f)","freq",cols2[i])
  cols2[i] <- gsub("([Gg]ravity)","Gravity",cols2[i])
  cols2[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",cols2[i])
  cols2[i] <- gsub("[Gg]yro","Gyro",cols2[i])
  cols2[i] <- gsub("AccMag","AccMagnitude",cols2[i])
  cols2[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",cols2[i])
  cols2[i] <- gsub("JerkMag","JerkMagnitude",cols2[i])
  cols2[i] <- gsub("GyroMag","GyroMagnitude",cols2[i])
}

merged_set3$activityID <-activity_labels[,2][match(merged_set3$activityID,activity_labels$activityID)]

colnames(merged_set3) <- cols2

merged_set4 <- merged_set3[,-21]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average 
#    of each variable for each activity and each subject.

merged_set4 <- group_by(merged_set4,activityID,subjectID)

tidyData<- summarize_all(merged_set4,funs(mean))
print(tidyData)

write.table(tidyData, './FinalTidyData.txt',row.names=FALSE,sep='\t')





