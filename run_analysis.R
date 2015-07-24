library(dplyr)
#Step 1 - Merges the training and the test sets to create one data set
#Read in the train files
x_train<-read.table('train/X_train.txt')
subject_train <- read.table('train/subject_train.txt')
y_train <- read.table('train/y_train.txt')

#Combine the Subject and Activity into the train measurement table
x_train$Subject <- subject_train[,1]
x_train$Activity <- y_train[,1]

#Read in the test files
x_test<-read.table('test/X_test.txt')
subject_test <- read.table('test/subject_test.txt')
y_test <- read.table('test/y_test.txt')

#Combine the Subject and Activity into the test measurement table
x_test$Subject <- subject_test[,1]
x_test$Activity <- y_test[,1]

#Combine the two train and test data set
data <- rbind(x_train, x_test)

#Step 2 - Extract only the measurements on the mean and std for each measurement
#Read in the feature (measurement) names
features <- read.table('features.txt')
#Select only features that have mean or std in the name
selectedFeatures <- grep('mean|std', features[,2])
selectedData <- select(data, selectedFeatures, ncol(data)-1, ncol(data))
#selectedData <- data[,selectedFeatures]
#selectedData$Subject = data$Subject
#selectedData$Activity = data$Activity

#Step 3 - Use descriptive activity names
labels <- read.table('activity_labels.txt')
selectedData <- mutate(selectedData, Activity = factor(labels[Activity,2]))

#Step 4 - Appropriately labels the data set with descriptive variable names.
#We use gsub to remove brackets in the variable names
names(selectedData)[1: length(selectedFeatures)] <- gsub("[\\(\\)]", "", features[selectedFeatures, 2])

#Step 5 - Creat a tidy data set with the average of each variable for each activity and each subject.
cleanData <- selectedData %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))
#Note: we can also use aggregate function instead of summarise_each

#write to an output file for submission
write.table(cleanData, "clean.txt", row.name = FALSE)







