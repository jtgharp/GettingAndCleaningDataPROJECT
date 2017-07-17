## Code Book

### Activities and Subject
  These variables identify the unique subject/activity pair. Each pair serves as a key for one row of data. 
  
  #### Subject: 
       Each Subject identified using the integer subject ID.
        
  #### Activity: 
        ActivityId: Unique Identifier for Activity
  #### Activity Type:    These are associated to Activity Id 1 through 6
    Walking
    Walking Upstairs
    Walking Downstairs
    Sitting
    Standing
    Laying
    
 #### Note to Reviewer: 
 ##### In my final write out of the tidy data set I have suppressed the activity type (labels as for each row we still have the activitiyId)  
 ##### See last line in the code: write.table(avgActSubj, f, row.names = FALSE) 
 ### Measurement Means:
 #### All variables are the mean of a measurement for each activity and subject. This is stressed by use of the initial Mean in the variable name.
##### Following are Time Domain body acceleration means along X, Y and Z axes
* MeanTimeBodyAccMean-X
* MeanTimeBodyAccMean-Y
* MeanTimeBodyAccMean-Z
##### Following are Time Domain gravity acceleration means along X, Y and Z axes
* MeanTimeGravityAccMean-X
* MeanTimeGravityAccMean-Y
* MeanTimeGravityAccMean-Z
* MeanTimeBodyAccJerkMean-X
* MeanTimeBodyAccJerkMean-Y
* MeanTimeBodyAccJerkMean-Z
##### Following are Time Domain gyro  means along X, Y and Z axes
* MeanTimeBodyGyroMean-X
* MeanTimeBodyGyroMean-Y
* MeanTimeBodyGyroMean-Z
* MeanTimeBodyGyroJerkMean-X
* MeanTimeBodyGyroJerkMean-Y
* MeanTimeBodyGyroJerkMean-Z
* MeanTimeBodyAccMagMean
* MeanTimeGravityAccMagMean
* MeanTimeBodyAccJerkMagMean
* MeanTimeBodyGyroMagMean
* MeanTimeBodyGyroJerkMagMean


* MeanFrequencyBodyAccMean-X
* MeanFrequencyBodyAccMean-Y
* MeanFrequencyBodyAccMean-Z
##### Following codes can be interpreted in similar fashion  as above excpet  frequency would mean frequency domain and Time would mean time domain
* MeanFrequencyBodyAccMeanFreq-X
* MeanFrequencyBodyAccMeanFreq-Y
* MeanFrequencyBodyAccMeanFreq-Z
* MeanFrequencyBodyAccJerkMean-X
* MeanFrequencyBodyAccJerkMean-Y
* MeanFrequencyBodyAccJerkMean-Z
* MeanFrequencyBodyAccJerkMeanFreq-X
* MeanFrequencyBodyAccJerkMeanFreq-Y
* MeanFrequencyBodyAccJerkMeanFreq-Z

* MeanFrequencyBodyGyroMean-X
* MeanFrequencyBodyGyroMean-Y
* MeanFrequencyBodyGyroMean-Z
* MeanFrequencyBodyGyroMeanFreq-X
* MeanFrequencyBodyGyroMeanFreq-Y
* MeanFrequencyBodyGyroMeanFreq-Z
* MeanFrequencyBodyAccMagMean
* MeanFrequencyBodyAccMagMeanFreq
* MeanFrequencyBodyAccJerkMagMean
* MeanFrequencyBodyAccJerkMagMeanFreq
* MeanFrequencyBodyGyroMagMean
* MeanFrequencyBodyGyroMagMeanFreq
* MeanFrequencyBodyGyroJerkMagMean
* MeanFrequencyBodyGyroJerkMagMeanFreq

* MeanTimeBodyAccStdDev-X
* MeanTimeBodyAccStdDev-Y
* MeanTimeBodyAccStdDev-Z
* MeanTimeGravityAccStdDev-X
* MeanTimeGravityAccStdDev-Y
* MeanTimeGravityAccStdDev-Z
* MeanTimeBodyAccJerkStdDev-X
* MeanTimeBodyAccJerkStdDev-Y
* MeanTimeBodyAccJerkStdDev-Z

* MeanTimeBodyGyroStdDev-X
* MeanTimeBodyGyroStdDev-Y
* MeanTimeBodyGyroStdDev-Z
* MeanTimeBodyGyroJerkStdDev-X
* MeanTimeBodyGyroJerkStdDev-Y
* MeanTimeBodyGyroJerkStdDev-Z
* MeanTimeBodyAccMagStdDev
* MeanTimeGravityAccMagStdDev
* MeanTimeBodyAccJerkMagStdDev
* MeanTimeBodyGyroMagStdDev
* MeanTimeBodyGyroJerkMagStdDev

* MeanFrequencyBodyAccStdDev-X
* MeanFrequencyBodyAccStdDev-Y
* MeanFrequencyBodyAccStdDev-Z
* MeanFrequencyBodyAccJerkStdDev-X
* MeanFrequencyBodyAccJerkStdDev-Y
* MeanFrequencyBodyAccJerkStdDev-Z
* MeanFrequencyBodyGyroStdDev-X
* MeanFrequencyBodyGyroStdDev-Y
* MeanFrequencyBodyGyroStdDev-Z
* MeanFrequencyBodyAccMagStdDev
* MeanFrequencyBodyAccJerkMagStdDev
* MeanFrequencyBodyGyroMagStdDev
* MeanFrequencyBodyGyroJerkMagStdDev
 