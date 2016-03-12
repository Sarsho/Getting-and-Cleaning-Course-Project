######################################################
## Coursera: Getting and Cleaning Data, wk 4 Assignment
##
## COllect data, create a tidy data set, perform basic descriptive stats, and upload
## to github. See assignment description for full details.
## data are found here, along with description
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## 
## Please upload the tidy data set created in step 5 of the instructions. Please upload 
## your data set as a txt file created with write.table() using row.name=FALSE (do not 
## cut and paste a dataset directly into the text box, as this may cause errors saving 
## your submission).
## data downloaded "Mon Feb 22 22:07:05 2016"

## setting up a variable for the original working directory to reset at the end of the script
owd <- getwd()

## setting the working directory to where the test data are saved
setwd("C:/Users/Austin/Desktop/Analytics Programs/Coursera/Cleaning Data/Course Project/UCI HAR Dataset/test")
list.files()

## loading the dply package which will be used for the majority of the tiding operations
library(dplyr)

## creating the dplyr data.frame/table_df class object for the test data set
X_test <- tbl_df(read.csv("X_test.txt", header = FALSE, sep = ""))
y_test <- tbl_df(read.csv("y_test.txt", header = FALSE, sep = ""))
sub_test <- tbl_df(read.csv("subject_test.txt", header = FALSE, sep = ""))

str(X_test)
View(X_test)

str(y_test)
View(y_test)
quantile(sub_test$V1)

## Changing the working directory to the location of the training data set
setwd("C:/Users/Austin/Desktop/Analytics Programs/Coursera/Cleaning Data/Course Project/UCI HAR Dataset/train")
      list.files()

## creating the dplyr data.frame/table_df class object for the training data set
X_train <- tbl_df(read.csv("X_train.txt", header = FALSE, sep = ""))
y_train <- tbl_df(read.csv("y_train.txt", header = FALSE, sep = ""))
sub_train <- tbl_df(read.csv("subject_train.txt", header = FALSE, sep = ""))

str(X_train)
View(X_train)
View(sub_train)
quantile(sub_train$V1)

str(y_train)
View(y_train)


## Resetting the working directory to get the variable names and activity lists 
setwd("C:/Users/Austin/Desktop/Analytics Programs/Coursera/Cleaning Data/Course Project/UCI HAR Dataset")
      list.files()

## generating vectors of the activity names and variable names to add to the acc_data table
activity <- tbl_df(read.csv("activity_labels.txt", header = FALSE, sep = ""))
variables <- tbl_df(read.csv("features.txt", header = FALSE, sep = "", 
                             stringsAsFactors = FALSE))

## renaming the column names for join functions; in activity 
activity <- rename(activity, activity = V1)
activity <- rename(activity, activity_name = V2)
activity$activity <- as.factor(activity$activity)

activity
head(variables, 10)
tail(variables, 10)

## merge the X_test and X_train data sets by row binding through dplyr. 
## x are the independant variables
x <- bind_rows(X_test, X_train)

## renaming the columns in 'x' data frame to the variables from the features.txt file
## but first the column names and set as characters class names
oldnames <- as.character(names(x))
newnames <- variables$V2
      names(x)[names(x)%in% oldnames] <- newnames

head(x[,1:10],10)

## merging y's, and s's to form 1 vector each which is then merged to the x data sets by 
## dplyr column bind to create the original full data set, 
## y is the activity list, it is the dependant varaible. s are the study subject code.
y <- bind_rows(y_test, y_train)
y <- rename(y, activity = V1)
y$activity <- as.factor(y$activity)

s <- bind_rows(sub_test, sub_train)
s <- rename(s, subject = V1)
s$subject <- as.factor(s$subject)
quantile(as.integer(s$subject))

## removing duplicate columns, subsetting the variables that contain 'mean()' and 'std()' 
acc_data <- x[ , !duplicated(colnames(x))]
acc_data <- select(acc_data, -contains("-meanFreq"))
acc_stat_m <- select(acc_data, matches(".-mean()."))
acc_stat_s <- select(acc_data, matches(".std."))

## column binding to create the reduced data frame for final analysis
acc_stat <- bind_cols(y, s, acc_stat_m, acc_stat_s)
str(acc_stat)


## adding the activity name variable and then fixing all the variable names so they
## can be renamed by the variables vector
acc_stat <- left_join(acc_stat, activity, by = "activity")
acc_stat <- acc_stat[, c(69, 1:68)]
names(acc_stat)

str(acc_stat)
View(acc_stat)
head(acc_stat[,1:5], 20)
head(acc_stat[,65:69],20)
tail(acc_stat[,65:69], 20)

## final tiding operation using melt() to transpose the signal variables to a single column
## of 'signal' for the id variable, and the value column as 'measurment'
library(reshape2)

acc_stat_long <- melt(acc_stat, variable.name = "signal", value.name = "measurement")
head(acc_stat_long)
View(acc_stat_long)
str(acc_stat_long)

## grouping the final tidy data by activity name, subject and signal and then generating
## the mean for each signal, this is the final output
avg_stat <- group_by(acc_stat_long, activity_name, subject, signal) %>% 
      summarise(average = mean(measurement))

View(avg_stat)
str(avg_stat)

## creating a txt file with write.table() using row.name=FALSE for upload to the coursera
## assignment site.
setwd("C:/Users/Austin/Desktop/Analytics Programs/Coursera/Cleaning Data/Course Project")
list.files()

write.table(avg_stat, file = "signal_avg.txt", sep = ",", row.names = FALSE)

setwd(owd)

