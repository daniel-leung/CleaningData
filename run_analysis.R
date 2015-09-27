run_analysis <- function() {
  ## Assumption: 
  ##- A test and train subfolder with Subject_test.txt,X_test.txt, y_test.txt in it
  ##-Current folder has activity_labels.txt for activity label and features.txt 
  ## for feature vector variable name
  ##-Subject_test.txt or Subject_train.txt is the volunteer number per experiment
  ##-y_test.txt or y_train.txt is the activity number per experiment
  ##-activity_labels.txt is the activity descriptions for activity performed per
  ## experiement
  ## 
  
  ## read in features.txt for variable labels and rename variable in the all dataframe
  features<-read.table("features.txt")
  variables<-features[,2]

  ##read in X_train.txt y_train.txt Subject_train.txt under train subfolder
  xtrain<-read.table("./train/X_train.txt")
  ytrain<-read.table("./train/y_train.txt")
  strain<-read.table("./train/subject_train.txt")
  
  ##rename xtrain column names to match descriptive variable names
  names(xtrain)<-variables

  ##extract only mean and std variables
  sub_xtrain<-subset(xtrain,
                  select=c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,
                           240:241,253:254,266:271,345:350,424:429,
                           503:504,516:517,529:530,542:543))


  ## rename column name for ytrain and strain
  colnames(ytrain)[1]<-"activity"
  colnames(strain)[1]<-"volunteer"
 
  ## combine all the train data set
  alltrain<-cbind(strain,ytrain,sub_xtrain)
  
  ##read in X_test.txt y_test.txt Subject_test.txt under test subfolder
  xtest<-read.table("./test/X_test.txt")
  ytest<-read.table("./test/y_test.txt")
  stest<-read.table("./test/subject_test.txt")
  
  ##rename xtest column names to match descriptive variable names
  names(xtest)<-variables
  
  ##extract only mean and std variables
  sub_xtest<-subset(xtest,
                    select=c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,
                             240:241,253:254,266:271,345:350,424:429,
                             503:504,516:517,529:530,542:543))

  ## rename column name for ytrain and strain
  colnames(ytest)[1]<-"activity"
  colnames(stest)[1]<-"volunteer"
  
  ## combine all the train data set
  alltest<-cbind(stest,ytest,sub_xtest)
  
  ## combine train and test data set
  all<-rbind(alltrain,alltest)

  ## read in activities label and join activity label with combine data frame
  act_labels<-read.table("./activity_labels.txt")
  all_act_labels<-merge(act_labels,all,by.x="V1",by.y="activity",all=TRUE)
  all_act_labels<-all_act_labels[,2:length(all_act_labels)]
  names(all_act_labels)[1]<-"activity"

  ##use aggregate to obtain average by Subject and Activities
  attach(all_act_labels)
  avgData<-aggregate(all_act_labels,by=list(activity,volunteer),FUN=mean, na.rm=TRUE)
  result<-subset(avgData, select = -c(activity, volunteer ) )
  names(result)[1]<-"activity"
  names(result)[2]<-"volunteer"
  
  write.table(result,"tidydata1.txt",row.name=FALSE)
}