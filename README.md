---

title: "Getting and Cleaning data with R - project"
author: "svaba"
date: "November 22, 2015"
output: html_document
---

```{r}

# load data.table, dplyr
library(dplyr, data.table)

# must set your working directory here - assumption is that is already done
# per the instructions

wdir <- getwd()

# set activity, features labels
actl <- paste(wdir, "activity_labels.txt",sep="\\")
feat <- paste(wdir, "features.txt",sep="\\")

# read in activity, features vectors applies to train and test sets
df_act <- read.table(actl
                         ,header = FALSE
                         ,sep = ""
                         ,strip.white= TRUE
                         ,as.is = TRUE)
df_feat <- read.table(feat
                     ,header = FALSE
                     ,sep = ""
                     ,strip.white= TRUE
                     ,as.is = TRUE)


# read in X_train as df
X_train <- paste(wdir, "train", "X_train.txt",sep="\\")
df_train_X <- read.table(X_train
                        ,header = FALSE
                        ,sep = ""
                        ,strip.white= TRUE
                        ,as.is = TRUE)

# read in y_train.txt
y_train <- paste(wdir, "train", "y_train.txt",sep="\\")
df_y_train <- read.table(y_train
                         ,header = FALSE
                         ,sep = ""
                         , col.names = c("Activity.Number")
                         ,strip.white= TRUE
                         ,as.is = TRUE)


# read in subject_train.txt
subj_train <- paste(wdir, "train", "subject_train.txt",sep="\\")
df_subj_train <- read.table(subj_train
                         ,header = FALSE
                         ,sep = ""
                         , col.names = c("Subject.ID")
                         ,strip.white= TRUE
                         ,as.is = TRUE)



# read in X_test as df
X_test <- paste(wdir, "test", "X_test.txt",sep="\\")
df_test_X <- read.table(X_test
                         ,header = FALSE
                         ,sep = ""
                         ,strip.white= TRUE
                         ,as.is = TRUE)

# read in y_test.txt
y_test <- paste(wdir, "test", "y_test.txt",sep="\\")
df_y_test <- read.table(y_test
                         ,header = FALSE
                         ,sep = ""
                        , col.names = c("Activity.Number")
                         ,strip.white= TRUE
                         ,as.is = TRUE)


# read in subject_test.txt
subj_test <- paste(wdir, "test", "subject_test.txt",sep="\\")
df_subj_test <- read.table(subj_test
                            ,header = FALSE
                            ,sep = ""
                           , col.names = c("Subject.ID")
                            ,strip.white= TRUE
                            ,as.is = TRUE)

# rename the feature columns
names(df_feat) <- c("Column.Number", "Test.Name")

# Name columns on test and train data sets
names(df_test_X) <- df_feat$Test.Name
names(df_train_X) <- df_feat$Test.Name

# add training data X_train, columns y_train, subject_train
df_train_X$Activity.Number <- df_y_train$Activity.Number
df_train_X$Subject.ID <- df_subj_train$Subject.ID

# add test data X_test, columns y_test, subject_test
df_test_X$Activity.Number <- df_y_test$Activity.Number
df_test_X$Subject.ID <- df_subj_test$Subject.ID


# concatenate train/test sets
df_full <- rbindlist(list(df_test_X, df_train_X))


# select only columns of mean/stdev
colsel <- df_feat$Column.Number[regexec("mean\\(\\)", df_feat$Test.Name) >0 
                      | regexec("std", df_feat$Test.Name) >0 ]
# add 2 last columns of Activity.Number and Subject.ID
colsel <- append(colsel, c(562,563))

# Subset the full to only columns of mean/stdev plus subject/activity
df_full_sub <- subset(df_full, select=colsel)


# rename activity columns, then join with activity and subset df
names(df_act) <- c("Activity.Number", "Activity.Name")


# join activity data frame converted to data table with full data table
# on activity ID, rename the columns
df_full_subx <- inner_join(df_full_sub
                           , as.data.table(df_act), by="Activity.Number")

# Part5 Tidy data set:
# summarized each parameter by activity and subject_id

dt_sum <- df_full_subx[, lapply(.SD, mean)
                       , by = c("Subject.ID", "Activity.Name")]

# write out the summary file in the working directory
write.table(dt_sum, file="tidy_data.csv", row.names = FALSE, sep =",")
# Columns saved:
# 
# [1] "Subject.ID"                  "Activity.Name"              
# [3] "Activity.Number"             "tBodyAcc-mean()-X"          
# [5] "tBodyAcc-mean()-Y"           "tBodyAcc-mean()-Z"          
# [7] "tBodyAcc-std()-X"            "tBodyAcc-std()-Y"           
# [9] "tBodyAcc-std()-Z"            "tGravityAcc-mean()-X"       
# [11] "tGravityAcc-mean()-Y"        "tGravityAcc-mean()-Z"       
# [13] "tGravityAcc-std()-X"         "tGravityAcc-std()-Y"        
# [15] "tGravityAcc-std()-Z"         "tBodyAccJerk-mean()-X"      
# [17] "tBodyAccJerk-mean()-Y"       "tBodyAccJerk-mean()-Z"      
# [19] "tBodyAccJerk-std()-X"        "tBodyAccJerk-std()-Y"       
# [21] "tBodyAccJerk-std()-Z"        "tBodyGyro-mean()-X"         
# [23] "tBodyGyro-mean()-Y"          "tBodyGyro-mean()-Z"         
# [25] "tBodyGyro-std()-X"           "tBodyGyro-std()-Y"          
# [27] "tBodyGyro-std()-Z"           "tBodyGyroJerk-mean()-X"     
# [29] "tBodyGyroJerk-mean()-Y"      "tBodyGyroJerk-mean()-Z"     
# [31] "tBodyGyroJerk-std()-X"       "tBodyGyroJerk-std()-Y"      
# [33] "tBodyGyroJerk-std()-Z"       "tBodyAccMag-mean()"         
# [35] "tBodyAccMag-std()"           "tGravityAccMag-mean()"      
# [37] "tGravityAccMag-std()"        "tBodyAccJerkMag-mean()"     
# [39] "tBodyAccJerkMag-std()"       "tBodyGyroMag-mean()"        
# [41] "tBodyGyroMag-std()"          "tBodyGyroJerkMag-mean()"    
# [43] "tBodyGyroJerkMag-std()"      "fBodyAcc-mean()-X"          
# [45] "fBodyAcc-mean()-Y"           "fBodyAcc-mean()-Z"          
# [47] "fBodyAcc-std()-X"            "fBodyAcc-std()-Y"           
# [49] "fBodyAcc-std()-Z"            "fBodyAccJerk-mean()-X"      
# [51] "fBodyAccJerk-mean()-Y"       "fBodyAccJerk-mean()-Z"      
# [53] "fBodyAccJerk-std()-X"        "fBodyAccJerk-std()-Y"       
# [55] "fBodyAccJerk-std()-Z"        "fBodyGyro-mean()-X"         
# [57] "fBodyGyro-mean()-Y"          "fBodyGyro-mean()-Z"         
# [59] "fBodyGyro-std()-X"           "fBodyGyro-std()-Y"          
# [61] "fBodyGyro-std()-Z"           "fBodyAccMag-mean()"         
# [63] "fBodyAccMag-std()"           "fBodyBodyAccJerkMag-mean()" 
# [65] "fBodyBodyAccJerkMag-std()"   "fBodyBodyGyroMag-mean()"    
# [67] "fBodyBodyGyroMag-std()"      "fBodyBodyGyroJerkMag-mean()"
# [69] "fBodyBodyGyroJerkMag-std()" 


```