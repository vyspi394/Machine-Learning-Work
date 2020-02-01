#reading one particular sheet from the xlsx file r stores it as a dataframe
data<-xlsx::read.xlsx("D:/Perriod 2/Machine Learning/Lab/Block 1 lab1/spambase.xlsx",sheetName = "spambase_data")

#1.1dividing the data into training and testing sets
n<-dim(data)[1]
set.seed(12345)
id<-sample(1:n, floor(n*0.5))
train<-data[id,]
test<-data[-id,]

#fitting the model
fit<-glm(formula = Spam~.,family = binomial(link = 'logit'),data = train)
#predicting the probability of spam for both testing and training data
Spam_given_x_train<-predict(fit,train,type="response")
Spam_given_x_test<-predict(fit,test,type="response")

#1.2 using 0.5
#training data
Spam_hat_train_1<-Spam_given_x_train
Spam_hat_train_1<-ifelse(Spam_hat_train_1>0.5,1,0)
cf<-data.frame("True values"=train$Spam,"Predicted values"=Spam_hat_train_1)
cm_glm_train_1<-table(cf)
mc_glm_train_1<-1-(sum(diag(cm_glm_train_1))/sum(cm_glm_train_1))
#test data
Spam_hat_test_1<-Spam_given_x_test
Spam_hat_test_1<-ifelse(Spam_given_x_test>0.5,1,0)
cm_glm_test_1<-table(test$Spam,Spam_hat_test_1)
mc_glm_test_1<-1-(sum(diag(cm_glm_test_1))/sum(cm_glm_test_1))

#1.3 using 0.8
#Training data
Spam_hat_train_2<-Spam_given_x_train
Spam_hat_train_2<-ifelse(Spam_given_x_train>0.8,1,0)
cm_glm_train_2<-table(train$Spam,Spam_hat_train_2)
mc_glm_train_2<-1-(sum(diag(cm_glm_train_2))/sum(cm_glm_train_2))
#Test data
Spam_hat_test_2<-Spam_given_x_test
Spam_hat_test_2<-ifelse(Spam_hat_train_2>0.8,1,0)
cm_glm_test_2<-table(test$Spam,Spam_hat_test_2)
mc_glm_test_2<-1-(sum(diag(cm_glm_test_2))/sum(cm_glm_test_2))


#1.4 K nearest neighbour classification
#Fitting a model using kknn model for K=30
output_test_30<-kknn::kknn(formula=Spam~.,train=train,test=test,k=30)
output_train_30<-kknn::kknn(formula=Spam~.,train=train,test=train,k=30)
fv_test<-output_test_30$fitted.values
fv_train<-output_train_30$fitted.values
#Classifying as spam or not
fv_test<-ifelse(fv_test>0.5,1,0)
fv_train<-ifelse(fv_train>0.5,1,0)
#Confusion matrices and misclassification rates
cm_kknn_train_30<-table(train$Spam,fv_train)
mc_train_kknn_30<-1-(sum(diag(cm_kknn_train_30))/sum(cm_kknn_train_30))
cm_kknn_test_30<-table(test$Spam,fv_test)
mc_test_kknn_30<-1-(sum(diag(cm_kknn_test_30))/sum(cm_kknn_test_30))

#1.5 K nearest neighbour classification
#Fitting model using kknn model with K=1
output_test_1<-kknn::kknn(formula = Spam~.,train=train,test = test,k=1)
output_train_1<-kknn::kknn(formula = Spam~.,train=train,test = train,k=1)
fv_test_1<-output_test_1$fitted.values
fv_train_1<-output_train_1$fitted.values
#Classifying as spam or not
fv_test_1<-ifelse(fv_test_1>0.5,1,0)
fv_train<-ifelse(fv_train>0.5,1,0)
#Confusion matrices and misclassification rates
cm_kknn_train_1<-table(train$Spam,fv_train_1)
mc_train_kknn_1<-1-(sum(diag(cm_kknn_train_1))/sum(cm_kknn_train_1))
cm_kknn_test_1<-table(test$Spam,fv_test_1)
mc_test_kknn_1<-1-(sum(diag(cm_kknn_test_1))/sum(cm_kknn_test_1))

#3Feature selection by cross-validation in a linear model
#linear regression
mylin=function(X,Y, Xpred){
  Xpred1=cbind(1,Xpred)
  X=cbind(1,X)
  #compute beta
  beta=solve(t(X)%*%X)%*%t(X)%*%Y
  Res=Xpred1%*%beta
  return(Res)
}

myCV=function(X,Y,Nfolds){
  n=length(Y)
  p=ncol(X)
  set.seed(12345)
  ind=sample(n,n)
  print(ind)
  X1=X[ind,]
  print(head(X1))
  Y1=Y[ind]
  print(head(Y1))
  sf=(n/Nfolds)
  MSE=numeric(2^p-1)
  Nfeat=numeric(2^p-1)
  Features=list()
  curr=0
  
  #we assume 5 features.
  
  for (f1 in 0:1)
    for (f2 in 0:1)
      for(f3 in 0:1)
        for(f4 in 0:1)
          for(f5 in 0:1){
            model= c(f1,f2,f3,f4,f5)
            if (sum(model)==0) next()
            SSE=0
            
            for (k in 1:Nfolds){
              #compute which indices should belong to current fold
              indices<-(((k-1)*sf)+1):(k*sf)
              #implement cross-validation for model with features in "model" and iteration i.
              X_test<-X1[indices,which(model==1)]
              X_train<-X1[-indices,which(model==1)]
              Yp<-Y1[indices]
              Y_train<-Y1[-indices]
              Ypred<-mylin(X_train,Y_train,X_test)
              #Get the predicted values for fold 'k', Ypred, and the original values for folf 'k', Yp.
              SSE=SSE+sum((Ypred-Yp)^2)
            }
            curr=curr+1
            MSE[curr]=SSE/n
            Nfeat[curr]=sum(model)
            Features[[curr]]=model
          }
  
  #plot MSE against number of features
  plot(Nfeat,MSE)
  i=which.min(MSE)
  return(list(CV=MSE[i], Features=Features[[i]]))
}
data("swiss")
swiss<-swiss
myCV(X=as.matrix(swiss[,2:6]),Y=swiss[[1]],Nfolds=5)


#4Linear regression and regularization
#4.1Import data to R
soil_data<-xlsx::read.xlsx("D:/Perriod 2/Machine Learning/Lab/Block 1 lab1/tecator.xlsx",sheetName = "data")
#plot of Moisture versus Protein.
plot(x=soil_data$Protein,y=soil_data$Moisture)
#yes the data has a neary linear relation
#4.2beacuse the M follows normal distribution thats why
#4.3fitting a polynomial model for polynomial models from 1 to 6
nn<-dim(soil_data)[1]
set.seed(12345)
id<-sample(1:nn, floor(nn*0.5))
soil_test<-soil_data[id,]
soil_train<-soil_data[-id,]
M<-list()
mse_train<-c()
mse_test<-c()
for(i in 1:6){
  M$i<-lm(soil_train$Moisture~poly(soil_train$Protein,i,raw = TRUE),data = soil_train)
  y_train<-predict(M$i,soil_train,type="response")
  y_test<-predict(M$i,soil_test,type="response")
  mse_train[i]<-mean((y_train-soil_train$Moisture)^2)
  mse_test[i]<-mean((y_test-soil_test$Moisture)^2)
}
i<-1:6
analyse<-data.frame("Polynomial"=i,"Train"=mse_train,"Test"=mse_test)
train_mse<-ggplot2::ggplot(analyse,
                         ggplot2::aes(i,mse_train))+
  ggplot2::geom_point(col="red")+
  ggplot2::ggtitle("MSE plot for train data") +
  ggplot2::ylab("Empirical error") +
  ggplot2::xlab("Polynimial degree")

test_mse<-ggplot2::ggplot(analyse,
                            ggplot2::aes(i,mse_test))+
  ggplot2::geom_point(col="red")+
  ggplot2::ggtitle("MSE plot for test data") +
  ggplot2::ylab("Predicted error") +
  ggplot2::xlab("Polynimial degree")


ggpubr::ggarrange(train_mse,test_mse,
                  ncol = 2, nrow = 1)

#4.4 Perform variable selection of a linear model
lm_fs<-lm(Fat~.,data = soil_data[,2:102])
step<-MASS::stepAIC(lm_fs,direction="both",trace = FALSE)
my_pars<-colnames(step$model)

#4.5 ridge regression 
# Fit a model
model_ridge=glmnet::glmnet(as.matrix(soil_data[,2:101]),soil_data$Fat, alpha=0,family="gaussian")
#plot coefficients and log of lambda
ridge_plot<-plot(model_ridge, xvar="lambda", label=TRUE)


#4.6 LASSO regression
model_lasso=glmnet::glmnet(as.matrix(soil_data[,2:101]),soil_data$Fat, alpha=1,family="gaussian")
#plot coefficients and log of lambda
lasso_plot<-plot(model_lasso, xvar="lambda", label=TRUE)

#4.7 cross validation
cv_lasso<-glmnet::cv.glmnet(as.matrix(soil_data[,2:101]),soil_data$Fat, alpha=1,family="gaussian")
names(cv_lasso)
plot(cv_lasso$lambda,cv_lasso$cvm,xlab = "Lambda",ylab = "Scores")

