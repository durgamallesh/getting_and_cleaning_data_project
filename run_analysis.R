#function to cleanup the activity data
run_analysis <- function(){
  require(plyr)
  #read the training data from the local folder.
  x_train <- read.table("./data/train/X_train.txt",header=FALSE)
  y_train <- read.table("./data/train/y_train.txt",header=FALSE)
  subject_train <- read.table("./data/train/Subject_train.txt",header=FALSE)
  
  #download the test data from the local folder
  x_test <- read.table("./data/test/X_test.txt",header=FALSE)
  y_test <- read.table("./data/test/y_test.txt",header=FALSE)
  subject_test <- read.table("./data/test/Subject_test.txt",header=FALSE)
  
  #download the activity labels
  activity_labels <- read.table("./data/activity_labels.txt",header=FALSE)
  
  #download the features list
  features <- read.table("./data/features.txt",header=FALSE)
  
  #assigning rowNames to the features file
  names(features) <- c("metric_id","metric_name")
  
  #extract only the metrics ending with mean()/std() from the features file.
  # this list is later used to extract only the features we need from the data file.
  metric_ids <- features[grep("*mean\\(\\)|std\\(\\)*",features$metric_name),1]
  metric_names <- as.character(features[grep("*mean\\(\\)|std\\(\\)*",features$metric_name),2])
  
  #create a training temporary dataset to extract only the metrics we need.
  train_temp <- x_train[,metric_ids]
  
  #assign meaningful names to the newly created dataset.
  names(train_temp) <- metric_names
  names(y_train) <- "activity_id"
  names(subject_train) <- "subject_id"
  
  #create a Train dataset by joing the activity, subject and metrics datasets.
  train <- cbind(subject_train,y_train,train_temp)

  #Repeat the process with the Test Data sets.
  test_temp <- x_test[,metric_ids]
  names(test_temp) <- metric_names
  names(y_test) <- "activity_id"
  names(subject_test) <- "subject_id"
  test <- cbind(subject_test,y_test,test_temp)
  
  #Create a final temp dataset by "rbind" ing test and Train datasets.
  final_temp <- rbind(train,test)
  names(activity_labels) <- c("activity_id","activity_name")
  
  #merge the fnal temp dataset with the activity master data set 
  #to assign meaningful names to the activities.
  final_df <- merge(activity_labels, final_temp, by.x="activity_id",by.y="activity_id")

  #claculate the colwise means and create a tidy dataset.
  tidy_df <- ddply(final_df,.(subject_id,activity_name),colwise(mean))
  tidy_df$activity_id <- NULL
  
  #write the tidy dataset to a flat file.
  write.table(tidy_df,"./data/tidy.txt",sep="\t",col.names=FALSE,eol="\n",append=FALSE)

}

