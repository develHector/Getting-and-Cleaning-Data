message("run_analysis.R by Hector Casavantes March 2015")

# install.packages("dplyr")
library(dplyr)
library(plyr)

run <- function( home_dir = '.' )  {
  
  # Get column data names
  filename <- paste( home_dir, 'UCI HAR Dataset', 'features.txt', sep='/' );
  message( paste( "Opening ", filename, sep='"' ) ) ;
  features <- read.table( filename ) ;
  
  # Get a vector Only with columns containing Mean and StdDev 
  colnames(features) <- c("Id", "Name") ;      
  features.needed <- grepl( "mean()", features$Name, fixed = T, ) + grepl( "std()", features$Name, fixed = T, ) > 0 ;    
  
  
  
  # 'train/X_train.txt': Training set.
  filename <- paste( home_dir, 'UCI HAR Dataset', 'train', 'X_train.txt', sep='/' );
  message( paste( "Opening ", filename, sep='"' ) ) ;
  X_train <- read.table( filename, col.names = features$Name ) ;  
  X_train <- X_train[,features.needed] ;  
          
  # 'train/y_train.txt': Training labels.
  filename <- paste( home_dir, 'UCI HAR Dataset', 'train', 'y_train.txt', sep='/' );
  message( paste( "Opening ", filename, sep='"' ) ) ;
  y_train <- read.table( filename ) ;
  colnames(y_train) <- "activity_label"
  
  # 'train/subject_train.txt': Training subjects  
  filename <- paste( home_dir, 'UCI HAR Dataset', 'train', 'subject_train.txt', sep='/' );
  message( paste( "Opening ", filename, sep='"' ) ) ;
  subject_train <- read.table( filename ) ;
  colnames(subject_train) <- "subject"
      
  
  
  # test/X_test.txt': Test set.
  filename <- paste( home_dir, 'UCI HAR Dataset', 'test', 'X_test.txt', sep='/' );
  message( paste( "Opening ", filename, sep='"' ) ) ;
  X_test <- read.table( filename, col.names = features$Name ) ;  
  X_test <- X_test[,features.needed] ;
      
  # 'test/y_test.txt': Test labels.
  filename <- paste( home_dir, 'UCI HAR Dataset', 'test', 'y_test.txt', sep='/' );
  message( paste( "Opening ", filename, sep='"' ) ) ;
  y_test <- read.table( filename ) ;
  colnames(y_test) <- "activity_label"
  
  # 'test/subject_test.txt': Test subjects  
  filename <- paste( home_dir, 'UCI HAR Dataset', 'test', 'subject_test.txt', sep='/' );
  message( paste( "Opening ", filename, sep='"' ) ) ;
  subject_test <- read.table( filename ) ;
  colnames(subject_test) <- "subject"
    
  
  
  # Will use dplyr mutate to add a column indicating the partition type
  train_dataset <- cbind( subject_train, y_train, X_train ) ;
  test_dataset <- cbind( subject_test, y_test, X_test );

  # bind everything
  full_dataset <- rbind( train_dataset, test_dataset ) ;
    
  # Just a small check to rest assured all is working good
  if( nrow( y_train ) + nrow( y_test ) != nrow( full_dataset ) ) 
    stop("Error - Individual rowcounts do not correspond to final rowcount set") ;  
  
  # Get activity names
  filename <- paste( home_dir, 'UCI HAR Dataset', 'activity_labels.txt', sep='/' );
  message( paste( "Opening ", filename, sep='"' ) ) ;
  activity_labels <- read.table( filename ) ;
  colnames(activity_labels) <- c( "activity_label", "activity" ) ;
  
  
  # using the plyr join verb
  full_dataset <- join( activity_labels, full_dataset ) ;
  full_dataset$activity_label <- NULL ; # Lets remove that column once it's already labeled
      
  # Create a new dataset with the Avg of each variable, per activity, per subject
  result_dataset <- dplyr::summarise( dplyr::group_by( full_dataset, subject, activity , add = T ),
                      tBodyAcc.mean...X  = mean(  tBodyAcc.mean...X	),
                      tBodyAcc.mean...Y	= mean(	tBodyAcc.mean...Y	),
                      tBodyAcc.mean...Z	= mean(	tBodyAcc.mean...Z	),
                      tBodyAcc.std...X	= mean(	tBodyAcc.std...X	),
                      tBodyAcc.std...Y	= mean(	tBodyAcc.std...Y	),
                      tBodyAcc.std...Z	= mean(	tBodyAcc.std...Z	),
                      tGravityAcc.mean...X	= mean(	tGravityAcc.mean...X	),
                      tGravityAcc.mean...Y	= mean(	tGravityAcc.mean...Y	),
                      tGravityAcc.mean...Z	= mean(	tGravityAcc.mean...Z	),
                      tGravityAcc.std...X	= mean(	tGravityAcc.std...X	),
                      tGravityAcc.std...Y	= mean(	tGravityAcc.std...Y	),
                      tGravityAcc.std...Z	= mean(	tGravityAcc.std...Z	),
                      tBodyAccJerk.mean...X	= mean(	tBodyAccJerk.mean...X	),
                      tBodyAccJerk.mean...Y	= mean(	tBodyAccJerk.mean...Y	),
                      tBodyAccJerk.mean...Z	= mean(	tBodyAccJerk.mean...Z	),
                      tBodyAccJerk.std...X	= mean(	tBodyAccJerk.std...X	),
                      tBodyAccJerk.std...Y	= mean(	tBodyAccJerk.std...Y	),
                      tBodyAccJerk.std...Z	= mean(	tBodyAccJerk.std...Z	),
                      tBodyGyro.mean...X	= mean(	tBodyGyro.mean...X	),
                      tBodyGyro.mean...Y	= mean(	tBodyGyro.mean...Y	),
                      tBodyGyro.mean...Z	= mean(	tBodyGyro.mean...Z	),
                      tBodyGyro.std...X	= mean(	tBodyGyro.std...X	),
                      tBodyGyro.std...Y	= mean(	tBodyGyro.std...Y	),
                      tBodyGyro.std...Z	= mean(	tBodyGyro.std...Z	),
                      tBodyGyroJerk.mean...X	= mean(	tBodyGyroJerk.mean...X	),
                      tBodyGyroJerk.mean...Y	= mean(	tBodyGyroJerk.mean...Y	),
                      tBodyGyroJerk.mean...Z	= mean(	tBodyGyroJerk.mean...Z	),
                      tBodyGyroJerk.std...X	= mean(	tBodyGyroJerk.std...X	),
                      tBodyGyroJerk.std...Y	= mean(	tBodyGyroJerk.std...Y	),
                      tBodyGyroJerk.std...Z	= mean(	tBodyGyroJerk.std...Z	),
                      tBodyAccMag.mean..	= mean(	tBodyAccMag.mean..	),
                      tBodyAccMag.std..	= mean(	tBodyAccMag.std..	),
                      tGravityAccMag.mean..	= mean(	tGravityAccMag.mean..	),
                      tGravityAccMag.std..	= mean(	tGravityAccMag.std..	),
                      tBodyAccJerkMag.mean..	= mean(	tBodyAccJerkMag.mean..	),
                      tBodyAccJerkMag.std..	= mean(	tBodyAccJerkMag.std..	),
                      tBodyGyroMag.mean..	= mean(	tBodyGyroMag.mean..	),
                      tBodyGyroMag.std..	= mean(	tBodyGyroMag.std..	),
                      tBodyGyroJerkMag.mean..	= mean(	tBodyGyroJerkMag.mean..	),
                      tBodyGyroJerkMag.std..	= mean(	tBodyGyroJerkMag.std..	),
                      fBodyAcc.mean...X	= mean(	fBodyAcc.mean...X	),
                      fBodyAcc.mean...Y	= mean(	fBodyAcc.mean...Y	),
                      fBodyAcc.mean...Z	= mean(	fBodyAcc.mean...Z	),
                      fBodyAcc.std...X	= mean(	fBodyAcc.std...X	),
                      fBodyAcc.std...Y	= mean(	fBodyAcc.std...Y	),
                      fBodyAcc.std...Z	= mean(	fBodyAcc.std...Z	),
                      fBodyAccJerk.mean...X	= mean(	fBodyAccJerk.mean...X	),
                      fBodyAccJerk.mean...Y	= mean(	fBodyAccJerk.mean...Y	),
                      fBodyAccJerk.mean...Z	= mean(	fBodyAccJerk.mean...Z	),
                      fBodyAccJerk.std...X	= mean(	fBodyAccJerk.std...X	),
                      fBodyAccJerk.std...Y	= mean(	fBodyAccJerk.std...Y	),
                      fBodyAccJerk.std...Z	= mean(	fBodyAccJerk.std...Z	),
                      fBodyGyro.mean...X	= mean(	fBodyGyro.mean...X	),
                      fBodyGyro.mean...Y	= mean(	fBodyGyro.mean...Y	),
                      fBodyGyro.mean...Z	= mean(	fBodyGyro.mean...Z	),
                      fBodyGyro.std...X	= mean(	fBodyGyro.std...X	),
                      fBodyGyro.std...Y	= mean(	fBodyGyro.std...Y	),
                      fBodyGyro.std...Z	= mean(	fBodyGyro.std...Z	),
                      fBodyAccMag.mean..	= mean(	fBodyAccMag.mean..	),
                      fBodyAccMag.std..	= mean(	fBodyAccMag.std..	),
                      fBodyBodyAccJerkMag.mean..	= mean(	fBodyBodyAccJerkMag.mean..	),
                      fBodyBodyAccJerkMag.std..	= mean(	fBodyBodyAccJerkMag.std..	),
                      fBodyBodyGyroMag.mean..	= mean(	fBodyBodyGyroMag.mean..	),
                      fBodyBodyGyroMag.std..	= mean(	fBodyBodyGyroMag.std..	),
                      fBodyBodyGyroJerkMag.mean..	= mean(	fBodyBodyGyroJerkMag.mean..	),
                      fBodyBodyGyroJerkMag.std..	= mean(	fBodyBodyGyroJerkMag.std..	) ) ;
  
  result_dataset ;
}
