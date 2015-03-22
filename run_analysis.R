
##   The data linked to from the course website represent data collected from the 
## accelerometers from the Samsung Galaxy S smartphone. A full description is available 
## at the site where the data was obtained: 
##    http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
##
## Here are the data for the project: 
##    https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
##
## You should create one R script called run_analysis.R that does the following. 
##   (1) Merges the training and the test sets to create one data set.
##   (2) Extracts only the measurements on the mean and standard deviation for each measurement. 
##   (3) Uses descriptive activity names to name the activities in the data set
##   (4) Appropriately labels the data set with descriptive variable names. 
##   (5) From the data set in step 4, creates a second, independent tidy data set with the 
##       average of each variable for each activity and each subject.


#############################
# Step 0:                   #
#   Download and Unzip Data #
#############################
  
  ##   The following code is commented out because the instructions assume the data 
  ## already exists in an unzipped folder ...
  
# setwd("C:\\Users\\vbhat\\gettingandcleaningdata")

# file.loc <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# dest.file <- "samsung_data.zip"
# setInternet2(use = TRUE)
# download.file(file.loc, dest.file, mode = "wb")

# unzip(dest.file)
  ## data is now in a folder called "UCI HAR Dataset" in the current working directory


  # set working directory and execute package loads
setwd("C:\\Users\\vbhat\\gettingandcleaningdata\\UCI HAR Dataset")
library(dplyr)

################################
# Step 1:                      #
#    Merge Component Data Sets #
################################

  # first read in Training and Test Data
x_train <- read.table("train\\X_train.txt")
y_train <- read.table("train\\y_train.txt")
subject_train <- read.table("train\\subject_train.txt")

dim(x_train)
  # 7352 x 561
dim(y_train)
  # 7352 x 1
dim(subject_train)
  # 7352 x 1

x_test <- read.table("test\\X_test.txt")
y_test <- read.table("test\\y_test.txt")
subject_test <- read.table("test\\subject_test.txt")

dim(x_test)
  # 2947 x 561
dim(y_test)
  # 2947 x 1
dim(subject_test)
  # 2947 x 1

  # bring in feature names for the training variables
feature.names <- read.table("features.txt", stringsAsFactors = FALSE)
feature.names <- feature.names[,2]
  # column 2 has the actual names (the first column is just a counting column

  # bind together training-related and test-related data
training.data <- cbind(subject_train, y_train, x_train)
names(training.data) <- c("subject_id", "activity", feature.names)

test.data <- cbind(subject_test, y_test, x_test)
names(test.data) <- c("subject_id", "activity", feature.names)

  # bind all training and test data together
all.data <- rbind(training.data, test.data)                              

dim(all.data)
  # 10299 x 563

length(unique(all.data$subject_id))
length(unique(all.data$activity))
  # 30 unique subjects and 6 unique activities, so we should get 180 rows after the summary

#####################################################
# Step 2:                                           #
#    Subset Data Frame for Mean and Std Dev Columns #
#####################################################

  # gets the pair of ID columns and all columns with "mean()" or "std()" in them
all.data <- all.data[,c("subject_id", "activity", names(all.data)[grep("(mean\\(\\))|(std\\(\\))", names(all.data))])]
  ##   note that I'm avoiding pulling the ones that are like meanFreq(), so I'm searching specifically  
  ## for mean() and std() as the patterns; the meanFreq() columns are described as being a weighted avg
  ## of the frequency components to obtain a mean frequency, but as we're asked to pull the mean and std 
  ## columns for each measurement, it doesn't seem to me that these already-consolidated columns fit the bill

dim(all.data)
  # 10299 x 68 -- most of the columns have gone by the wayside

##########################################################################
# Step 3:                                                                #
#   Create Descriptive Activity Names for the activities in the Data Set #
##########################################################################

activity.labels <- read.table("activity_labels.txt", stringsAsFactors = FALSE, col.names = c("activity", "activity_label"))
  # the labels file already has descriptive values for the activities, rather than just the ID values

all.data <- merge(x = all.data, y = activity.labels, by = "activity", all = TRUE)
all.data <- select(all.data, -activity)
  # removing the now unnecessary activity ID number

all.data <- arrange(all.data, subject_id, activity_label)
  # just reordering the data so that subjects are grouped in consecutive row blocks

###############################################################
# Step 4:                                                     #
#   Appropriately Labels the Variables with Descriptive Names #
###############################################################

# backup <- all.data

  # from the "features_info.txt" files, we know:
  #  (1) "t" at the start of a variable name denotes time domain
  #  (2) "f" at the start of a variable name denotes frequency domain
  #  (3) "acc" (accelerometer) measurements are split into body + gravity components 
  # 
  # it'd be easier to read this as [domain]_[measure type]_[component1]_[component2]_[directional component]_[moment]

  # remove all parentheses and convert dashes to underscores
names(all.data) <- gsub("[()]", "", names(all.data))
names(all.data) <- gsub("-", "_", names(all.data))

  # spell out domain as time or frequency
names(all.data) <- gsub("^t", "time_", names(all.data))
names(all.data) <- gsub("^f", "freq_", names(all.data))

  # reorder into [measure type]_[component] instead of current [componentMeasureType] style
names(all.data) <- gsub("_GravityAcc", "_acc_gravity", names(all.data))
names(all.data) <- gsub("_BodyAcc", "_acc_body", names(all.data))
names(all.data) <- gsub("_BodyGyro", "_gyro_body", names(all.data))

  # split out Jerk/Mag composite components (BodyBodyGyroMag)
names(all.data) <- gsub("Jerk", "_jerk", names(all.data))
names(all.data) <- gsub("Mag", "_mag", names(all.data))

names(all.data) <- gsub("BodyBodyGyro", "gyro_body_body", names(all.data))
names(all.data) <- gsub("BodyBodyAcc", "acc_body_body", names(all.data))

  # reorder directional components and statistical moment
names(all.data) <- gsub("mean_X", "x_mean", names(all.data))
names(all.data) <- gsub("mean_Y", "y_mean", names(all.data))
names(all.data) <- gsub("mean_Z", "z_mean", names(all.data))

names(all.data) <- gsub("std_X", "x_std", names(all.data))
names(all.data) <- gsub("std_Y", "y_std", names(all.data))
names(all.data) <- gsub("std_Z", "z_std", names(all.data))

###################################################################################
# Step 5:                                                                         #
#    Create a Tidy, Summarized Data Set with the Avg of each Measurement Variable #
###################################################################################

subject.summary <- all.data %>% group_by(subject_id, activity_label) %>% summarise_each(funs(mean))

dim(subject.summary)
  # 180 x 68
  # this makes sense because there were 30 subjects and 6 activities (so 180 rows)

summary(subject.summary)
  # some values are negative because the data was normalized and so the original data can lie between [-1, 1]

write.table(subject.summary, 
            ".\\subject_summary.txt", 
            row.names = FALSE)

