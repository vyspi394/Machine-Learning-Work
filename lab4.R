library(mboost)
library(randomForest)
RNGversion("3.5.1")
#reading the csv
spam_data<-read.csv2(file="D:/Perriod 2/Machine Learning/Lab/Block 2 lab1/spambase.csv",sep=";")
spam_data$Spam <- as.factor(spam_data$Spam)
#dividing into train and test
n<-nrow(spam_data)
suppressWarnings(RNGversion("3.5.1"))
set.seed(12345)
id<-sample(1:n, floor(n*1/3))
spam_test<-spam_data[id,]
spam_train<-spam_data[-id,]
#building the model with adaboost and random forest for 10,20...,100 
i<-10
j<-1
ada_train_error<-c()
ada_test_error<-c()
rf_train_error<-c()
rf_test_error<-c()
while(i<=100){
  ####################################
  #adaboost model
  ada_boost_obj<-blackboost(formula = Spam~.,
                            data =spam_train,
                            family = AdaExp(),
                            control=boost_control(mstop=i))
  #predicting the values for both test and train with the ada_boost_obj
  ada_spam_train<-predict(ada_boost_obj,spam_train,type = "class")
  ada_spam_test<-predict(ada_boost_obj,spam_test,type = "class")
  #confusion matrix for train
  df1<-data.frame("True Values"=spam_train$Spam,
                  "Predicted Values"=ada_spam_train)
  ada_cm_train<-table(df1)
  #misclassification rate for train
  ada_train_error[j]=(1-sum(diag(ada_cm_train))/sum(ada_cm_train))*100
  #confusion matrix for test
  df2<-data.frame("True Values"=spam_test$Spam,
                  "Predicted Values"=ada_spam_test)
  ada_cm_test<-table(df2)
  #misclassification rate for test
  ada_test_error[j]=(1-(sum(diag(ada_cm_test))/sum(ada_cm_test)))*100
  
  ###############################################
  #random forest model
  rf_obj<-randomForest(formula = Spam~.,data =spam_train,ntree=j)
  #fitting the data for both train and test data
  rf_spam_train<-predict(rf_obj,spam_train,type="class")
  rf_spam_test<-predict(rf_obj,spam_test,type="class")
  #confusion matrix for train data
  df3<-data.frame("True Values"=spam_train$Spam,
                  "Predicted Values"=rf_spam_train)
  rf_cm_train<-table(df3)
  #misclassification for train
  rf_train_error[j]<-(1-sum(diag(rf_cm_train))/sum(rf_cm_train))*100
  #confusion matrix for test data
  df4<-data.frame("True Values"=spam_test$Spam,
                  "Predicted Values"=rf_spam_test)
  rf_cm_test<-table(df4)
  #misclassification for test
  rf_test_error[j]<-(1-sum(diag(rf_cm_test))/sum(rf_cm_test))*100
  j<-j+1
  i<-i+10
}
#ploting the error values
#ada boost
max_ada<-rep(NA,times=length(ada_test_error))
max_ada[which.min(ada_test_error)]<-which.min(ada_test_error)*10
plot(x=seq(10,100,length.out = 10),y=ada_test_error,
     xlab = "Number of trees",
     ylab = "Percentage error",
     type="o",main = "Ada Boost Test error graph")
text(x=seq(10,100,length.out = 10),y=ada_test_error,
     labels = max_ada[max_ada != "NA"],
     pos = 4,
     cex = 0.7,
     col = 2)
#random forest
max_rf<-rep(NA,times=length(rf_test_error))
max_rf[which.min(rf_test_error)]<-which.min(rf_test_error)*10
plot(x=seq(10,100,length.out = 10),rf_test_error,
     xlab = "Number of trees",
     ylab = "Percentage error",
     type="o",main = "Random Forest Test error graph")
text(x=seq(10,100,length.out = 10),y=rf_test_error,
     labels = max_rf[max_rf != "NA"],
     pos = 4,
     cex = 0.7,
     col = 2)


