
#library(dplyr)
library(plyr)
library(tidyr)
library(stringr)
library(reshape2)
#set working directory
setwd("C:\\Coursera\\Getting and Cleaning Data\\project")
run_analysis <- function() {
        
   #download the zip file and then unzip it
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "     
        filename <- file.path(getwd(), "UCIData.zip")
        if (!file.exists(filename)){
                download.file(fileURL, filename, mode="wb")
                unzip(filename)
        } 
        v<- list.files(getwd(),all.files=TRUE, recursive=TRUE) #get all fully expanded file names in c:\aa\bb\cc\filename.txt form
        vdf <- as.data.frame(v,stringsAsFactors = FALSE)
        
        filepath <- function(X, ptrn){for(pth in X){ if(str_detect(pth,ptrn)) {fp<-pth}}
                                                 return(fp)
                }
        
        
        #1 Merges the training and the test sets to create one data set.(this is step 1 of the assignment)
        
        #read features and activity labels txt files
        
        features <- read.table(sapply(X = vdf,FUN=filepath, ptrn="features.txt"), header=FALSE)
        activityTypes <- read.table(sapply(X = vdf,FUN=filepath, ptrn="activity_labels.txt"), header=FALSE)
        
        #read training data
        
        subjectTrain <- read.table(sapply(X = vdf,FUN=filepath, ptrn="subject_train.txt"), header=FALSE)
        XTrain <- read.table(sapply(X = vdf,FUN=filepath, ptrn="X_train.txt"), header=FALSE)
        yTrain <- read.table(sapply(X = vdf,FUN=filepath, ptrn="y_train.txt"), header=FALSE)
        
              
        # Assigin column names to the above data files
        colnames(activityTypes) = c('activityId','activityType');
        colnames(subjectTrain)  = "subjectId";
        colnames(XTrain) = features[,2]; #associate 561 feature labels to the training set columns
        colnames(yTrain) = "activityId"; # these are the training labels for activities performed by subject
    
        # merge the three data sets; yTrain (i.e. activities), subjectTrain (the subjects who performs the activity), and XTrain (the reading)
        mergedTrainingData = cbind(yTrain,subjectTrain,XTrain);
        
        
        # Now read the test data sets
        subjectTest <- read.table(sapply(X = vdf,FUN=filepath, ptrn="subject_test.txt"), header=FALSE)
        XTest <- read.table(sapply(X = vdf,FUN=filepath, ptrn="X_test.txt"), header=FALSE)
        yTest <- read.table(sapply(X = vdf,FUN=filepath, ptrn="y_test.txt"), header=FALSE)
        
        # Like before assign column names
        colnames(subjectTest) = "subjectId";
        colnames(XTest)  = features[,2]; 
        colnames(yTest) = "activityId";
        
        # merge the three test data sets like before we did for training data
        mergedTestData = cbind(yTest,subjectTest,XTest);
        
        # Combine all rows from training and test data sets to create a ALLdata set
        allData = rbind(mergedTrainingData,mergedTestData);
        
   # 2 Extracts only the measurements on the mean and standard deviation for each measurement.(step 2 from assignment)
        cnames <- colnames(allData)
        #find col numbers for all means and all stdeviations also add col number for activityId and subjectId
        colNumbers <- c(grep("activityId", cnames), grep("subjectId",cnames), grep("-mean"  , cnames),grep("-std",cnames))
        #now subset the data based on the extracted column numbers
        extractedData <- subset(allData,select= colNumbers)
        
   #3.Uses descriptive activity names to name the activities in the data set (step 3 from assignment)
    #4 Appropriately labels the data set with descriptive variable names.
        
        labeledExtrData = merge(activityTypes, extractedData,by='activityId',all.x=TRUE); #step3
        
        #before we clean variable names (column names) let us get column names into cnames for labeled extracted data
        cnames <- colnames(labeledExtrData)
        #now we remove all () as in "tBodyAcc-mean()-X"
        cnames <- gsub("\\()","", cnames)
        #change -mean to Mean
        cnames <- gsub("-mean","Mean", cnames)
        #change -std to StdDev
        cnames <- gsub("-std","StdDev", cnames)
        #change t to Time
        cnames <- gsub("^t", "Time", cnames)
        #change f to Frequency
        cnames <- gsub("^f", "Frequency", cnames)
        # there is a BodyBody error in last three columns
        cnames <- gsub("BodyBody", "Body", cnames)
        #I AM NOT CHANGING  ACC to Acceleraion and Mag to Magnitude becasue it becomes TOO LONG
        
        #Now assign the cleaned up variable names to the columns of labeled extracted data
        colnames(labeledExtrData) = cnames;
        
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. (this is step 5 in Assignment)
        # Column means for all except the activityId, activityType and subjectId columns (1,2,3)
        subColMeans <- function(data) { colMeans(data[,-c(1,2,3)]) }
        #now we split the data frame by activityId and SubjectId and find averages . 
        #please note we have to change column names to precede with Mean to stress that these are averages
        avgActSubj <- ddply(labeledExtrData, .(subjectId, activityId), subColMeans)
        names(avgActSubj)[-c(1,2)] <- paste0("Mean", names(avgActSubj)[-c(1,2)])
        
        # Merging the tidyData with activityType to include descriptive acitvity names
        avgActSubj = merge(activityTypes, avgActSubj,by='activityId',all.x=TRUE);
        
        # now arrange the data first by activity Id within which subject Id and finally call it tidyData :)
        tidyData <- arrange(avgActSubj, activityId,subjectId)
       
        #write out to a file with NO ROW names
        f <- file.path(getwd(), "tidyData.txt")
        write.table(tidyData, f, row.names = FALSE)
        #finally return the tidy data
        tidyData
        
        
}