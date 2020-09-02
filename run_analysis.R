library(dplyr)
library(tidyr)


## extract useful files
act_lab<- read.table("./Raw data/activity_labels.txt", header = FALSE)
feat<- read.table("./Raw data/features.txt", header = FALSE)

## read all test data, labels and subjects
test_data<- read.table("./Raw data/test/X_test.txt", header=FALSE)
test_sub<- read.table("./Raw data/test/subject_test.txt", header=FALSE)
test_lab<- read.table("./Raw data/test/y_test.txt", header = FALSE)

## create a test table that has the data and labels
test_set<- cbind(test_sub, test_lab, test_data)

## read all train data, labels and subjects
train_data<- read.table("./Raw data/train/X_train.txt", header=FALSE)
train_sub<- read.table("./Raw data/train/subject_train.txt", header=FALSE)
train_lab<- read.table("./Raw data/train/y_train.txt", header = FALSE)


## create a train table that has the data and labels
train_set<- cbind(train_sub, train_lab, train_data)

##combine both dataset
data_joined<- rbind(test_set, train_set)
colnames(data_joined)<- c("subjects", "activity", as.vector(feat[1:561, 2]))

##extract columns with "mean" or "std
keep<-grep("(mean)()|(std)()", colnames(data_joined))
data_keep<- data_joined %>% select(subjects, activity, colnames(data_joined)[keep])

##replace activity labels with descriptive names
for (i in 1:nrow(act_lab)){
  activity_name<-replace(data_keep$activity,which(data_keep$activity == act_lab[i,1]), 
                         as.character(act_lab[i,2]))
  data_keep$activity <- activity_name
}

data_keep <- data_keep %>%  arrange(subjects, activity)

## create independent tidy data with average values
select_var<- colnames(data_keep)[-c(1:2)] #create a vector of variables to summarise
by_subject_activity<- data_keep %>% group_by(subjects, activity)
data_avg<- summarise_at(by_subject_activity, select_var, mean) %>% 
  arrange(subjects, activity)

## save tidy data sets in Tidy data folder
write.table(data_keep, "./Tidy data/data_keep.txt", row.names = FALSE)
write.table(data_avg, "./Tidy data/data_avg.txt", row.names = FALSE)

