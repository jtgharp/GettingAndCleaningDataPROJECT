 
 
This submission addresses the stated goal in the project assignment; viz." The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set."
## Change my script in one place if you want to test it
Please change line 8 which is :  setwd("C:\\Coursera\\Getting and Cleaning Data\\project") :  to set to working directory on your machine
## Summary of script steps
First of all, the script is abundantly commented.  Also,throughtout the code you will see the steps required to be performed and stated in the Project Assignment description. For example step 1 in the assignment states; #1 Merges the training and the test sets to create one data set.(this is step 1 of the assignment).  You will find this exact text in the script.  Then the code that follows it achieves that step.
Here is the summary of what the script performs.

(1) First; line 1 to line 25, loads relevant libraries, downloads zip file and then unzips it.  It also sets the working directory.  

(2) Then: line 26 to 64,  performs step 1 of the assigment.  First read features.txt and activity_label.txt.  Then read the three training txt files. Then we Assigin column names to the above data files.  Then we merge the three training data sets using column bind. We do the same two steps for the test data sets.  For all reading I have written a small function that fiures out the fully expanded path for each file till its parent directory so you do not have to specify that.  After this we merge the train and the test data sets using the row binds.   

(3) Then: lines 67 to 71 perform step 2 of the project assignment.  Here we first find column numbers for all means and all stdeviations  feature columns and then add column numbers for activityId and subjectId to the array.  Then using this array we extract (subset) only the columns that we need.  

(4) Then: lines 74 to 94 addresses step 3 of the assignment requirements, and step 4 where we cleanup the column names (Variable names) for better description.  Every step in this is commented.  Please see the script for each step.   

(5) Then  in lines 98 to 115 , we create a tidy dat set.  Please note here I have followed the isntruction in step 5 exactly.  It says; "creates a second, independent tidy data set with the average of each variable for each activity and each subject.  Please note the order; first activity and then the subject.  So the merged data set column has activity ID, Activity Type (walking, seating etc) and the SubjectId column.  After ensuring this oirder of the columns, in the final step, I have arranged the tidy data rows by activity Id and then Subjec ID.  

## All the cleaned up variable names are provided in the code book in this repository.  Groups of variables names are preceded by an explanation.

I hope this description is good enough to udnersrstand the script
