################################################################
#Assignment 1. Using GAM and GLM to examine the mortality rates
################################################################
influenza_data<-xlsx::read.xlsx("D:/Perriod 2/Machine Learning/Lab/Block 2 lab2/Influenza.xlsx",
                      sheetName = "Raw data")
# 1.1plotting
plot(influenza_data$Time,influenza_data$Mortality)
plot(influenza_data$Time,influenza_data$Influenza)
hist(influenza_data[influenza_data$Year==1998, "Mortality"])


# 1.2 building the model
library(mgcv)
gam_model<-gam(Mortality~Year+s(Week,k=length(unique(influenza_data$Week))),
           data = influenza_data)
plot(gam_model)
# 1.3 plotting fitted VS original data
plot(x=influenza_data$Time,
     y=gam_model$fitted.values,
     xlab="Time",
     ylab="Mortality",
     main="Fitted and Original data VS Time",
     col="blue",lwd="2",type="o")
points(x=influenza_data$Time,
       y=influenza_data$Mortality,
       col="red",lwd="2",type="o")
legend("topleft",c("Original Data","Fitted Values"),fill=c("blue","red"))
#plot(influenza_data$Week,gam_model$fitted.values)
plot(gam_model, pages=1, residuals=T, pch=19, cex=0.25,
     scheme=1, col='#FF8000' , shade=T, shade.col='gray90' )
summary(gam_model)

# 1.4 Checking the gam model for differnt penalty factor
#Very small penalty factor
gam_modelp1<-gam(Mortality~Year+
                   s(Week,k=length(unique(influenza_data$Week)),sp=1e-5),
                 data = influenza_data)
#plotting the fitted VS actual data
plot(x=influenza_data$Time,
     y=gam_modelp1$fitted.values,
     xlab="Time",
     ylab="Mortality",
     main="Fitted and Original data for very low penality factor",
     col="blue",lwd="2",type="o")
points(x=influenza_data$Time,
       y=influenza_data$Mortality,
       col="red",lwd="2",type="o")
legend("topleft",c("Original Data","Fitted Values"),fill=c("blue","red"))
#Very high penalty  
gam_modelp2<-gam(Mortality~Year+
                   s(Week,k=length(unique(influenza_data$Week)),sp=1e+5),
                 data = influenza_data)
#plotting the fitted VS actual data
plot(x=influenza_data$Time,
     y=gam_modelp2$fitted.values,
     xlab="Time",
     ylab="Mortality",
     main="Fitted and Original data for high penlity factor",
     col="blue",lwd="2",type="o")
points(x=influenza_data$Time,
       y=influenza_data$Mortality,
       col="red",lwd="2",type="o")
legend("topleft",c("Original Data","Fitted Values"),fill=c("blue","red"))
#Degrees of freedom and deviance variation with penalty factor
is<-cumprod(c(1e-5,rep(10,10)))
devi<-numeric()
df<-numeric()
j<-1
for(i in is){
  g<-gam(Mortality~Year+
           s(Week,k=length(unique(influenza_data$Week)),sp=i),
         data = influenza_data)
  devi[j]<-g$deviance
  df[j]<-sum(g$edf)
  j<-j+1
}
plot(x=is,
     y=devi,
     xlab="Penality Factors",
     ylab="Deviance",
     main="Penality VS Deviance and degrees of freedom",
     col="blue",lwd="2",type="o")
plot(x=is,
     y=df,
     xlab="Penality Factors",
     ylab="Deviance",
     main="Penality VS Deviance and degrees of freedom",
     col="red",lwd="2",type="o")

# 1.5 plot the residuals and the influenza values against time
plot(x=influenza_data$Time,
     y=influenza_data$Influenza,
     xlab="Time",
     ylab="Fitted values and residuals",
     main="Fitted values and residuals",
     col="blue",lwd="2",type="o",ylim=c(-200,400))
points(x=influenza_data$Time,
       y=gam_model$residuals,
       col="red",lwd="2",type="o")
legend("topleft",c("Influenza","Residuals"),fill=c("blue","red"))

# 1.6 splines of year, week, and the number of confirmed cases of influenza
new_gam_model<-gam(Mortality~s(Year,k=length(unique(influenza_data$Year)))+
                     s(Week,k=length(unique(influenza_data$Week)))+
                     s(Influenza,k=length(unique(influenza_data$Influenza))),
                   data=influenza_data)
# Plotted the fitted VS original data
plot(x=influenza_data$Time,
     y=new_gam_model$fitted.values,
     xlab="Time",
     ylab="Mortality",
     main="Fitted and Original data VS Time",
     col="blue",lwd="2",type="o")
points(x=influenza_data$Time,
       y=influenza_data$Mortality,
       col="red",lwd="2",type="o")
legend("topleft",c("Original Data","Fitted Values"),fill=c("blue","red"))
summary(new_gam_model)

#######################################
#Assignment 2. High-dimensional methods
#######################################

# 2.1

#Reading the csv file
mail_data<-read.csv2(file="D:/Perriod 2/Machine Learning/Lab/Block 2 lab2/data.csv",sep=";")
utils::View(mail_data)
#dividing into train and test
n<-nrow(mail_data)
suppressWarnings(RNGversion("3.5.1"))
set.seed(12345)
id<-sample(1:n, floor(n*0.7))
mail_testt<-mail_data[-id,]
mail_trainn<-mail_data[id,]
#separating dependent and independent variable
library(pamr)
mail_train<-scale(mail_trainn)
mail_train[is.nan(mail_train)]<-0
mail_train<-as.data.frame(mail_train)
mail_train$Conference<-as.factor(mail_trainn$Conference)
x<-t(mail_train[,-4703])
y<-mail_train[[4703]]
mylist<-list(x=x,y=as.factor(y),
             geneid=as.character(1:nrow(x)),
             genenames=rownames(x))
pamr_model<-pamr.train(mylist,threshold = seq(0,5,0.1))
cv_model<-pamr.cv(pamr_model,mylist)
plot(x=cv_model$threshold,
     y=cv_model$loglik,
     xlab="Thresholds",
     ylab="LogLIKElyhood",
     main="LogLikelyhood VS Thresholds",
     col="blue",lwd="2",type="o")
plot(x=cv_model$threshold,
     y=cv_model$error,
     xlab="Thresholds",
     ylab="Error",
     main="Errors VS Threshold",
     col="red",lwd="2",type="o")
analysis<-data.frame("Threshold"=cv_model$threshold,"Non-zero features"=cv_model$size,"Likelyhood"=round(cv_model$loglik,2),"Error"=round(cv_model$error,2))
analysis
pamr.plotcv(cv_model)
cv_model
which.max(cv_model$loglik)
which.min(cv_model$error)
# Centroid plot
pamr.plotcen(pamr_model,mylist,threshold=0.9)
dev.off()
#list of parameters selected
para_list<-pamr.listgenes(pamr_model,mylist,threshold=0.9,genenames=TRUE)
cat(paste(as.numeric(nrow(para_list)),collapse='\n'))
cat(paste(colnames(mail_trainn)[as.numeric(para_list[1:10,1])],collapse='\n'))
#reporting the test error
#reporting the test error
mail_test<-scale(mail_testt)
mail_test[is.nan(mail_test)]<-0
mail_test<-as.data.frame(mail_test)
mail_test$Conference<-as.factor(mail_testt$Conference)
xt<-t(mail_test[,-4703])
yt<-mail_test[[4703]]
y_hut_test<-pamr.predict(fit=pamr_model,
                         newx=xt,
                         threshold=0.9,
                         type="class")
cm_pamr<-table(yt,y_hut_test,
          dnn = c("True Values","Predicted Values"))
mc_pamr<-1-sum(diag(cm_pamr))/sum(cm_pamr)
mc_pamr

# 2.2a Elastic net
library(glmnet)
mail_data$Conference
mail_trainn$Conference
#finding best lambda using cross validation
cv_elastic_model<-cv.glmnet(x=as.matrix(mail_trainn[,-4703]),
                            y=mail_trainn$Conference, 
                            alpha="0.5",
                            family="binomial",
                            type.measure="deviance")
plot(x=cv_elastic_model$lambda,
     y=cv_elastic_model$cvm,
     xlab="Lamda",
     ylab="Cross-Validated errors",
     main="Cross-validated error VS lambda",
     type="o",
     col="red",
     lwd="2")
#best lambda with minimum CVM 
best_lambda<-cv_elastic_model$lambda[which.min(cv_elastic_model$cvm)]
cat("The number of features selected",
    cv_elastic_model$nzero[which.min(cv_elastic_model$cvm)])
cat("The best lambda after cross validation is :",best_lambda)
#training the elastic model with best lambda
best_elastic_model<-glmnet(x=as.matrix(mail_trainn[,-4703]),
                           y=mail_trainn$Conference, 
                           alpha=0.5,
                           family="binomial",
                           lambda=best_lambda,
                           type.measure="deviance")
#reporting the test error
y_hut_elastic<-predict(best_elastic_model,newx=as.matrix(mail_testt[,-4703]))
#cofusion matrix and misclassification rate
cm_elastic<-table(mail_testt$Conference,y_hut_elastic,
                  dnn = c("True Values","Predicted Values"))
mc_elastic<-1-sum(diag(cm_elastic))/sum(cm_elastic)

# 2.2b Support Vector machine
library(kernlab) 
svm_obj<-ksvm(Conference~.,
             data=mail_trainn,
             kernel="vanilladot",
             type="C-svc")
y_hut_svm<-predict(object = svm_obj,
                   newdata =mail_testt,
                   type="response")

cm_svm<-table(mail_testt$Conference,
              y_hut_svm,
              dnn = c("True Values","Predicted Values"))
mc_svm<-1-sum(diag(cm_svm))/sum(cm_svm)
mc_svm

# 2.3 Benjamini-Hochberg method
t_test<-lapply(mail_data[,-4703],
               function(x) t.test(x~mail_data[[4703]],data=mail_data,
                      alternative = c("two.sided"),
                      var.equal=FALSE))
#extracting the p values from list
p_values<-sapply(X=t_test,FUN=getElement, name = "p.value")
#adjusting the p value using the BH method
adj_p_values<-p.adjust(p_values,method="BH")
#sort the values in accending order
analyze<-as.data.frame(cbind(p_values,adj_p_values))
analyze<-analyze[order(analyze$adj_p_values),]
analyze_BH<-analyze[analyze$adj_p_values<0.05,]
#########################################
p_values<-p_values[order(p_values)]
adj_p_values<-adj_p_values[order(adj_p_values)]
analyze

