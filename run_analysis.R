##Download data##
library(data.table)
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCIHARDataset.zip')){
  download.file(fileurl,'./UCIHARDataset.zip', mode = 'wb')
  unzip("UCIHARDataset.zip", exdir = getwd())
}

##Read and Convert Data##
sx_features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
sx_features <- as.character(sx_features[,2])

sx_data.train.x <- read.table('./UCI HAR Dataset/train/X_train.txt')
sx_data.train.activity <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
sx_data.train.subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

sx_data.train <-  data.frame(sx_data.train.subject, sx_data.train.activity, sx_data.train.x)
names(sx_data.train) <- c(c('subject', 'activity'), sx_features)

sx_data.test.x <- read.table('./UCI HAR Dataset/test/X_test.txt')
sx_data.test.activity <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
sx_data.test.subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

sx_data.test <-  data.frame(sx_data.test.subject, sx_data.test.activity, sx_data.test.x)
names(sx_data.test) <- c(c('subject', 'activity'), sx_features)

##1. Merges the Training and Testing Sets into 1 data set called sx_data.all##
sx_data.all <- rbind(sx_data.train, sx_data.test)

##2. Extracts only the measurements on the mean and standard deviation for each measurement.##
mean_std.select <- grep('mean|std', sx_features)
sx_data.sub <- sx_data.all[,c(1,2,mean_std.select + 2)]

##3. Uses descriptive activity names to name the activities in the data set##
sx_activity.labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
sx_activity.labels <- as.character(sx_activity.labels[,2])
sx_data.sub$activity <- sx_activity.labels[sx_data.sub$activity]

##4. Appropriately labels the data set with descriptive variable names.##
sx_name.new <- names(sx_data.sub)
sx_name.new <- gsub("[(][)]", "", sx_name.new)
sx_name.new <- gsub("^t", "TimeDomain_", sx_name.new)
sx_name.new <- gsub("^f", "FrequencyDomain_", sx_name.new)
sx_name.new <- gsub("Acc", "Accelerometer", sx_name.new)
sx_name.new <- gsub("Gyro", "Gyroscope", sx_name.new)
sx_name.new <- gsub("Mag", "Magnitude", sx_name.new)
sx_name.new <- gsub("-mean-", "_Mean_", sx_name.new)
sx_name.new <- gsub("-std-", "_StandardDeviation_", sx_name.new)
sx_name.new <- gsub("-", "_", sx_name.new)
names(sx_data.sub) <- sx_name.new

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.##

sx_data.tidy <- aggregate(sx_data.sub[,3:81], by = list(activity = sx_data.sub$activity, subject = sx_data.sub$subject),FUN = mean)
write.table(x = sx_data.tidy, file = "sx_data_tidy.txt", row.names = FALSE)
