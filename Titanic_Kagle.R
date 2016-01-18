# Titanic kaggle competition

#read train.csv  file into a dataframe
data_train<-read.csv("train.csv")

#read test.csv into a dataframe
data_test<-read.csv("test.csv")

#Create a train dataset for Age without all NA values 
train_age<-data_train[complete.cases(data_train$Age),]

#Create a test dataset for Age with only NA values. Age will be the label to be predicted here
test_age<-subset(data_train,is.na(data_train$Age))

#Build a lm to predict age for all missing values
model<-lm(Age ~ Pclass+factor(Sex)+factor(Embarked),data=train_age)

#using the model contructed above to predict age in test_age dataframe
test_age_fitted<-cbind(test_age,"predicted_age"=predict(model,test_age))
test_age_fitted$Age<-test_age_fitted$predicted_age

#Fit the age in the train dataset for all null values
new_train_data<-rbind(data_train[!(is.na(data_train$Age)),],test_age_fitted[,1:12])

#######Cleaning test data############
#Create a train dataset for Age without all NA values 
train_age<-data_test[complete.cases(data_test$Age),]

#Create a test dataset for Age with only NA values. Age will be the label to be predicted here
test_age<-subset(data_test,is.na(data_test$Age))

#Build a lm to predict age for all missing values
model<-lm(Age ~ Pclass+factor(Sex)+factor(Embarked),data=train_age)

#using the model contructed above to predict age in test_age dataframe
test_age_fitted<-cbind(test_age,"predicted_age"=predict(model,test_age))
test_age_fitted$Age<-test_age_fitted$predicted_age

#Fit the age in the train dataset for all null values
new_test_data<-rbind(data_test[!(is.na(data_test$Age)),],test_age_fitted[,1:11])


#use the newly created train data to construct a model for predicting survivability
model<-glm(Survived ~ Pclass+factor(Sex)+Age,family=binomial(),data=new_train_data)

#use the constructed model to predict survivability on the cleaned test data
data_test<-cbind(new_test_data,"Survived"=predict(model,new_test_data,type="response"))

#All survived values less than 0.5 round off to 0 and greater than to 1
data_test$Survived[data_test$Survived<=0.5]<-0
data_test$Survived[data_test$Survived>0.5]<-1

