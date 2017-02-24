#------------------------------------------------------------------------------#
#                        DAMIEN DOS SANTOS LUIS                                #
#             GETTING AND CLEANING DATA COURSE ON COURSERA                     #
#                               PEER REVIEW                                    #
#                               20-02-17                                       #
#------------------------------------------------------------------------------#

# !!! Script modified for the purpose of the assignment !!!

## Part 1 : accessing and downloading the data
        # This part is mandatory and has to be performed one single time

#directory <- "your_path"

#if(!file.exists(directory)) {
#        dir.create(directory)
#}

#setwd(directory)

#url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(url, "dataset.zip")
#unzip("dataset.zip")

#setwd("UCI HAR Dataset")


#files <- list.files(recursive = TRUE)

## Part2 : reading and merging the two datasets train and test

library(plyr)
library(dplyr)
library(reshape2)
library(matrixStats)

training <- read.table("train/X_train.txt")
head(training)
str(training)

testing <- read.table("test/X_test.txt")
head(testing)
str(testing)

# Merging the two datasets and some check controls
ncol(training) == ncol(testing)
merging <- rbind(training, testing)
nrow(merging) == nrow(testing) + nrow(training)



# Part 3 : Extracts only the measurements on the mean and standard deviation for each measurement
merging.mean.sd <- cbind(mean = rowMeans(merging), sd = rowSds(as.matrix(merging)))
#write.csv(merging.mean.sd, "merging_mean_sd.csv")



# Part 4 : Uses descriptive activity names to name the activities in the data set
# Extract the classes for each group and add the corresponding activity label

# Takes two tables, renames the column and binds them
## !!! Function adapted to the project's datasets !!! 
merge.tables <- function(table1, table2, column.name) {
        read.table1 <- read.table(table1)
        colnames(read.table1) <- column.name
        read.table2 <- read.table(table2)
        colnames(read.table2) <- column.name  
        
        rbind(read.table1, read.table2)
}

## Merging tables containing information about subjets, their activity and their quantitative measurements
activity.labels <- read.table(files[[1]])
colnames(activity.labels) <- c("class", "activity")

all.labels.activity <- merge(merge.tables("train/y_train.txt", "test/y_test.txt", "class"), activity.labels, by = "class")
merging.labelled <- cbind(all.labels.activity, merge.tables("train/subject_train.txt" , "test/subject_test.txt", "subject"), merging.mean.sd)

#write.csv(merging.labelled, "merged_datasets_labelled.csv")



# Part 5 : From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject.

summarized.table <- merging.labelled %>% group_by(activity, subject) %>%  summarise_each(funs(mean(.)))
str(summarized.table)

write.table(summarized.table, "summarized_dataset_table.txt", row.names = FALSE)









