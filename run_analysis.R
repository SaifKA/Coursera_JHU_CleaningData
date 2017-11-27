
if (!dir.exists("data")){
  dir.create("data")
}

dir()

####################   Project   ###################################

### Load relevant library

library(dplyr)


### Download Data
fileUrl<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(fileUrl,destfile = "./data/wearables_data.zip")

unzip("./data/wearables_data.zip")


### Read Features and Activity Labels (Common for Test and Train)

activity_labels<-read.csv("UCI HAR Dataset/activity_labels.txt",header = F,sep="",stringsAsFactors = F)

head(activity_labels)
str(activity_labels)

features<-read.csv("UCI HAR Dataset/features.txt",header = F,sep="",stringsAsFactors = F)

head(features)
str(features)


### Prepare Test Data Set

test<-read.csv("UCI HAR Dataset/test/X_test.txt",header = F,sep="",stringsAsFactors = F)


head(test)
str(test)

test_labels<-read.csv("UCI HAR Dataset/test/y_test.txt",header = F,sep="",stringsAsFactors = F)

head(test_labels)
str(test_labels)

test_subjects<-read.csv("UCI HAR Dataset/test/subject_test.txt",header = F,sep="",stringsAsFactors = F)

head(test_subjects)
str(test_subjects)

test_1<-cbind(test,test_labels,test_subjects)
names(test_1)

names(test_1)<-c(features$V2,'label','subject')


### Prepare Train Data Set

train<-read.csv("UCI HAR Dataset/train/X_train.txt",header = F,sep="",stringsAsFactors = F)


head(train)
str(train)

train_labels<-read.csv("UCI HAR Dataset/train/y_train.txt",header = F,sep="",stringsAsFactors = F)

head(train_labels)
str(train_labels)


train_subjects<-read.csv("UCI HAR Dataset/train/subject_train.txt",header = F,sep="",stringsAsFactors = F)

head(train_subjects)
str(train_subjects)

train_1<-cbind(train,train_labels,train_subjects)
names(train_1)


names(train_1)<-c(features$V2,'label','subject')

### Merge Test and Train - Instruction#1

mergeData<-rbind(test_1,train_1)

str(mergeData)
names(mergeData)


### Get Mean and Standard Deviation Data - Instruction#2
mean_std_Data<-mergeData[,grepl('mean()|std()|label|subject',names(mergeData)) ]
str(mean_std_Data)
names(mean_std_Data)

# # Exclude meanFreq
# mean_std_Data<-mean_std_Data[,!grepl('meanFreq',names(mean_std_Data) )]
# str(mean_std_Data)
# names(mean_std_Data)

### Descriptive Activity Names - Instruction#3

Final_raw_data<-mean_std_Data%>%
  left_join(activity_labels,by=c("label"="V1"))%>%
  rename(activity=V2)%>%
  select(-label)


### Added Descriptive Variable Names - Instruction#4

names(Final_raw_data)<-c(
  "time_domain_body_acceleration_mean_X_Axis"	,
  "time_domain_body_acceleration_mean_Y_Axis"	,
  "time_domain_body_acceleration_mean_Z_Axis"	,
  "time_domain_body_acceleration_standard_deviation_X_Axis"	,
  "time_domain_body_acceleration_standard_deviation_Y_Axis"	,
  "time_domain_body_acceleration_standard_deviation_Z_Axis"	,
  "time_domain_gravity_acceleration_mean_X_Axis"	,
  "time_domain_gravity_acceleration_mean_Y_Axis"	,
  "time_domain_gravity_acceleration_mean_Z_Axis"	,
  "time_domain_gravity_acceleration_standard_deviation_X_Axis"	,
  "time_domain_gravity_acceleration_standard_deviation_Y_Axis"	,
  "time_domain_gravity_acceleration_standard_deviation_Z_Axis"	,
  "time_domain_body_acceleration_jerk_mean_X_Axis"	,
  "time_domain_body_acceleration_jerk_mean_Y_Axis"	,
  "time_domain_body_acceleration_jerk_mean_Z_Axis"	,
  "time_domain_body_acceleration_jerk_standard_deviation_X_Axis"	,
  "time_domain_body_acceleration_jerk_standard_deviation_Y_Axis"	,
  "time_domain_body_acceleration_jerk_standard_deviation_Z_Axis"	,
  "time_domain_body_gyroscope_reading_mean_X_Axis"	,
  "time_domain_body_gyroscope_reading_mean_Y_Axis"	,
  "time_domain_body_gyroscope_reading_mean_Z_Axis"	,
  "time_domain_body_gyroscope_reading_standard_deviation_X_Axis"	,
  "time_domain_body_gyroscope_reading_standard_deviation_Y_Axis"	,
  "time_domain_body_gyroscope_reading_standard_deviation_Z_Axis"	,
  "time_domain_body_gyroscope_jerk_reading_mean_X_Axis"	,
  "time_domain_body_gyroscope_jerk_reading_mean_Y_Axis"	,
  "time_domain_body_gyroscope_jerk_reading_mean_Z_Axis"	,
  "time_domain_body_gyroscope_jerk_reading_standard_deviation_X_Axis"	,
  "time_domain_body_gyroscope_jerk_reading_standard_deviation_Y_Axis"	,
  "time_domain_body_gyroscope_jerk_reading_standard_deviation_Z_Axis"	,
  "time_domain_body_acceleration_magnitude_mean"	,
  "time_domain_body_acceleration_magnitude_standard_deviation"	,
  "time_domain_gravity_acceleration_magnitude_mean"	,
  "time_domain_gravity_acceleration_magnitude_standard_deviation"	,
  "time_domain_body_gyroscope_jerk_magnitudereading_mean"	,
  "time_domain_body_gyroscope_jerk_magnitudereading_standard_deviation"	,
  "time_domain_body_gyroscope_magnitude_reading_mean"	,
  "time_domain_body_gyroscope_magnitude_reading_standard_deviation"	,
  "time_domain_body_gyroscope_jerk_magnitude_reading_mean"	,
  "time_domain_body_gyroscope_jerk_magnitude_reading_standard_deviation"	,
  "frequency_domain_body_acceleration_mean_X_Axis"	,
  "frequency_domain_body_acceleration_mean_Y_Axis"	,
  "frequency_domain_body_acceleration_mean_Z_Axis"	,
  "frequency_domain_body_acceleration_standard_deviation_X_Axis"	,
  "frequency_domain_body_acceleration_standard_deviation_Y_Axis"	,
  "frequency_domain_body_acceleration_standard_deviation_Z_Axis"	,
  "frequency_domain_body_acceleration_mean_frequency_X_Axis"	,
  "frequency_domain_body_acceleration_mean_frequency_Y_Axis"	,
  "frequency_domain_body_acceleration_mean_frequency_Z_Axis"	,
  "frequency_domain_body_acceleration_jerk_mean_X_Axis"	,
  "frequency_domain_body_acceleration_jerk_mean_Y_Axis"	,
  "frequency_domain_body_acceleration_jerk_mean_Z_Axis"	,
  "frequency_domain_body_acceleration_jerk_standard_deviation_X_Axis"	,
  "frequency_domain_body_acceleration_jerk_standard_deviation_Y_Axis"	,
  "frequency_domain_body_acceleration_jerk_standard_deviation_Z_Axis"	,
  "frequency_domain_body_acceleration_jerk_mean_frequency_X_Axis"	,
  "frequency_domain_body_acceleration_jerk_mean_frequency_Y_Axis"	,
  "frequency_domain_body_acceleration_jerk_mean_frequency_Z_Axis"	,
  "frequency_domain_body_gyroscope_reading_mean_X_Axis"	,
  "frequency_domain_body_gyroscope_reading_mean_Y_Axis"	,
  "frequency_domain_body_gyroscope_reading_mean_Z_Axis"	,
  "frequency_domain_body_gyroscope_reading_standard_deviation_X_Axis"	,
  "frequency_domain_body_gyroscope_reading_standard_deviation_Y_Axis"	,
  "frequency_domain_body_gyroscope_reading_standard_deviation_Z_Axis"	,
  "frequency_domain_body_gyroscope_reading_mean_frequency_X_Axis"	,
  "frequency_domain_body_gyroscope_reading_mean_frequency_Y_Axis"	,
  "frequency_domain_body_gyroscope_reading_mean_frequency_Z_Axis"	,
  "frequency_domain_body_acceleration_magnitude_mean"	,
  "frequency_domain_body_acceleration_magnitude_standard_deviation"	,
  "frequency_domain_body_acceleration_magnitude_mean_frequency"	,
  "frequency_domain_body_gyroscope_jerk_magnitudereading_mean"	,
  "frequency_domain_body_gyroscope_jerk_magnitudereading_standard_deviation"	,
  "frequency_domain_body_gyroscope_jerk_magnitudereading_mean_frequency"	,
  "frequency_domain_body_gyroscope_magnitude_reading_mean"	,
  "frequency_domain_body_gyroscope_magnitude_reading_standard_deviation"	,
  "frequency_domain_body_gyroscope_magnitude_reading_mean_frequency"	,
  "frequency_domain_body_gyroscope_jerk_magnitude_reading_mean"	,
  "frequency_domain_body_gyroscope_jerk_magnitude_reading_standard_deviation"	,
  "frequency_domain_body_gyroscope_jerk_magnitude_reading_mean_frequency"	,
  "subject"	,
  "activity"	
)

### Tidy Dataset - Instruction#5

Final_tidy_data<-Final_raw_data%>%
  mutate(
    activity=as.factor(activity),
    subject=as.factor(subject)
  )%>%
  group_by(activity,subject)%>%
  summarise_all(mean)

str(Final_tidy_data)

write.table(Final_tidy_data,file="Final_tidy_data.txt",row.names = FALSE)


