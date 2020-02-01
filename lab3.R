#####################
#1. KERNEL METHODS:
####################
suppressWarnings(RNGversion("3.5.1"))
set.seed(1234567890)
library(geosphere)
library(hms)
#importing temperature data to R 
stations <- read.csv("D:/Perriod 2/Machine Learning/Lab/Block 1 lab3/stations.csv")
temps <- read.csv("D:/Perriod 2/Machine Learning/Lab/Block 1 lab3/temps50k.csv")
st <- merge(stations,temps,by="station_number")
utils::View(st)
#standard deviations of the guasian kernels
h_distance <-10
h_date <-2
h_time <-1
#lat long of Linkoping for which we need to predict the temperature
lat<-58.4166 
long<-15.6333
#Date and times to be predicted
date <-as.Date("2008-01-01") # The date to predict (up to the students)
times <- c("04:00:00","06:00:00",
           "08:00:00","10:00:00",
           "12:00:00","14:00:00",
           "16:00:00","18:00:00",
           "20:00:00","22:00:00",
           "24:00:00")
temp <- vector(length=length(times))
# Prediction code using guassian kernels
#formating the date and time 
st$date<-as.Date(st$date)
st$time<-strptime(st$time,"%H:%M:%S")
times<-strptime(times,format="%H:%M:%S")
#taking all the dates prior to the date of interest
st_prior<-st[st$date<date,]
#Distance between from the station to the point of interest(Linkoping)
diff_dist<-as.numeric(distHaversine(c(lat, long), 
                         st_prior[, c("latitude", "longitude")]))
#Distance between day of measured temperature and the day of interest
diff_date<-as.numeric(difftime(date,st_prior$date,units="days"))
for(i in 1:length(diff_date)){
  diff_date[i]<-min(diff_date[i]/365,365-(diff_date[i]/365))
}
#Distance between the hour of the measured temperature and the our times
#i.e 04:00-24:00
time_diff<- matrix(data = 0, nrow = dim(st_prior[1]), ncol = 11)
min_time_diff<-matrix(nrow = dim(st_prior[1]), ncol = 11)
for (i in 1:length(times)) {
  time_diff[,i]<-as.numeric(abs(difftime(strptime(times[i], "%H"),
          strptime(as.character(st_prior[,"time"]), "%H"), units = c("hours"))))
  
  min_time_diff[,i] = sapply(time_diff[,i],  function(x)min(x,(24-x))) 
}

#Sum of the three guassian Kernels
sum_guassian_kernel<-exp(-1*(as.numeric(diff_dist)/h_distance)^2)+
                        exp(-1*(as.numeric(diff_date)/h_date)^2)+
                        exp(-1*(as.numeric(min_time_diff)/h_time)^2)
prod_guassian_kernel<-exp(-1*(as.numeric(diff_dist)/h_distance)^2)*
                        exp(-1*(as.numeric(diff_date)/h_date)^2)*
                        exp(-1*(as.numeric(min_time_diff)/h_time)^2)
temp_sum<-sum_guassian_kernel*st_prior$air_temperature



##############################################
#2. SUPPORT VECTOR MACHINES
##############################################
library(kernlab)
data(spam)
#select a model
#diving the data into test train and validation
#tarining data
n<-nrow(spam)
grep("type",colnames(spam))
suppressWarnings(RNGversion("3.5.1"))
set.seed(12345)
id1<-sample(1:n, floor(n*0.5))
spam_train<-spam[id1,]
#testing and validation
idrem<-setdiff(1:n,id1)
suppressWarnings(RNGversion("3.5.1"))
set.seed(12345)
id2<-sample(idrem, n*0.25)
id3<-setdiff(idrem,id2)
spam_test<-spam[id2,]
spam_validation<-spam[id3,]

#performing model selection by selecting the model
#training the model with C=0.5 with train data set
svm_obj_0.5<-ksvm(type~.,
                  data=spam_train,
                  kernel="rbfdot",
                  type="C-svc",C=0.5,width=0.05)
#getting the predicted values of the test data
y_hut_pred_0.5<-predict(object=svm_obj_0.5,
                        newdata=spam_test,
                        type="response")
#confusion matrix
cm_0.5<-table(spam_test$type,y_hut_pred_0.5,
              dnn=c("True Values","Predicted values"))
#True positive rate and false positive rate
tpr_0.5<-cm_0.5[1,1]/sum(cm_0.5[,1])
fpr_0.5<-cm_0.5[1,2]/sum(cm_0.5[,2])
#Misclassification rate
mc_0.5<-1-sum(diag(cm_0.5))/sum(cm_0.5)
#training the data with C=1 with training data
svm_obj_1<-ksvm(type~.,
                data=spam_train,
                kernel="rbfdot",
                type="C-svc",C=1,width=0.05)
#gettting the test predicted values
y_hut_pred_1<-predict(object = svm_obj_1,
                      newdata=spam_test,
                      type="response")
#cofusion matrix
cm_1<-table(spam_test$type,y_hut_pred_1,
            dnn=c("True Values","Predicted Values"))
#True positive and false positive rates
tpr_1<-cm_1[1,1]/sum(cm_1[,1])
fpr_1<-cm_1[1,2]/sum(cm_1[,2])
#misclassification rate 
mc_1<-1-sum(diag(cm_1))/sum(cm_1)

#training the data with C=5 with training data
svm_obj_5<-ksvm(type~.,
                data=spam_train,
                kernel="rbfdot",
                type="C-svc",C=5,width=0.05)
#predicted values for the test data set
y_hut_pred_5<-predict(object=svm_obj_5,
                      newdata=spam_test,
                      type="response")
#confusion matrix
cm_5<-table(spam_test$type,y_hut_pred_1,
            dnn=c("True Values","Predicted Values"))
#True positive and false positive rates
tpr_5<-cm_5[1,1]/sum(cm_5[,1])
fpr_5<-cm_5[1,2]/sum(cm_5[,2])
#misclassification rate
mc_5<-1-sum(diag(cm_5))/sum(cm_5)
#analysing the results for all the three models
analysis<-data.frame("C=0.5"=c(round(tpr_0.5,4),round(fpr_0.5,4),round(mc_0.5,4)),
                     "C=1"=c(round(tpr_1,4),round(fpr_1,4),round(mc_1,4)),
                     "C=5"=c(round(tpr_5,4),round(fpr_5,4),round(mc_5,4)),
                     row.names=c("TPR","FPR","MC"))
analysis
#training the data with C=1 with test data
svm_obj_1<-ksvm(type~.,
                data=spam_test,
                kernel="rbfdot",
                type="C-svc",C=1)
#gettting the validation predicted values
y_hut_pred_1<-predict(object = svm_obj_1,
                      newdata=spam_validation,
                      type="response")
#cofusion matrix
cm_1<-table(spam_validation$type,y_hut_pred_1,
            dnn=c("True Values","Predicted Values"))
#True positive and false positive rates
tpr_1<-cm_1[1,1]/sum(cm_1[,1])
fpr_1<-cm_1[1,2]/sum(cm_1[,2])
#misclassification rate 
mc_1<-1-sum(diag(cm_1))/sum(cm_1)
#training the data with C=5 with test data
svm_obj_5<-ksvm(type~.,
                data=spam_test,
                kernel="rbfdot",
                type="C-svc",C=5)
#predicted values for the validation data set
y_hut_pred_5<-predict(object=svm_obj_5,
                      newdata=spam_validation,
                      type="response")
#confusion matrix
cm_5<-table(spam_validation$type,y_hut_pred_1,
            dnn=c("True Values","Predicted Values"))
#True positive and false positive rates
tpr_5<-cm_5[1,1]/sum(cm_5[,1])
fpr_5<-cm_5[1,2]/sum(cm_5[,2])
#misclassification rate
mc_5<-1-sum(diag(cm_5))/sum(cm_5)
#analysing the results for all the three models
analysis_2<-data.frame("C=0.5"=c(round(tpr_0.5,4),round(fpr_0.5,4),round(mc_0.5,4)),
                     "C=1"=c(round(tpr_1,4),round(fpr_1,4),round(mc_1,4)),
                     "C=5"=c(round(tpr_5,4),round(fpr_5,4),round(mc_5,4)),
                     row.names=c("TPR","FPR","MC"))
analysis_2

