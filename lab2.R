##################################################
#Assignment 2. Analysis of credit scoring
##################################################
library(tree)
library(e1071)
RNGversion("3.5.1")
# 2.1 Reading the data into R from the excel and dividing into train and test
credit_scores<-xlsx::read.xlsx("D:/Perriod 2/Machine Learning/Lab/Block 1 lab2/creditscoring.xls",
                               sheetName = "credit",
                               header = TRUE)
#tarining data
n<-nrow(credit_scores)
suppressWarnings(RNGversion("3.5.1"))
set.seed(12345)
id1<-sample(1:n, floor(n*0.5))
credit_train<-credit_scores[id1,]
#testing and validation
cred<-credit_scores[-id1,]
idrem<-setdiff(1:n,id1)
suppressWarnings(RNGversion("3.5.1"))
set.seed(12345)
id2<-sample(idrem, n*0.25)
id3<-setdiff(idrem,id2)
credit_test<-credit_scores[id2,]
credit_validation<-credit_scores[id3,]

# 2.2 Fit a decision tree
# a.Deviance
credit_dt_deviance<-tree(good_bad~.,
                      data = credit_train,
                      split = "deviance",
                      method = "recursive.partition")
#plot(x=credit_dt_deviance,y=NULL,type = "proportional")
#text(x=credit_dt_deviance,splits = TRUE, all = FALSE,
     #pretty = NULL, digits = getOption("digits") - 3, 
     #adj = par("adj"), xpd = TRUE)
y_hut_test_deviance<-predict(credit_dt_deviance,credit_test,type="class")
y_hut_train_deviance<-predict(credit_dt_deviance,credit_train,type = "class")
cm_train_deviance<-table(credit_train$good_bad,y_hut_train_deviance,
                         dnn=c("Actual","Predicted"))
cm_test_deviance<-table(credit_test$good_bad,y_hut_test_deviance,
                        dnn=c("Actual","Predicted"))
mc_train_deviance<-1-sum(diag(cm_train_deviance))/sum(cm_train_deviance)
mc_test_deviance<-1-sum(diag(cm_test_deviance)/sum(cm_test_deviance))

# b.gini method
credit_dt_gini<-tree(good_bad~.,
                     data = credit_train,
                      split = "gini",
                      method = "recursive.partition")
#plot(x=credit_dt_gini,y=NULL,type = "proportional")
#text(x=credit_dt_gini,pretty = NULL)
y_hut_train_gini<-predict(credit_dt_gini,credit_train,type="class")
y_hut_test_gini<-predict(credit_dt_gini,credit_test,type = "class")
cm_train_gini<-table(credit_train$good_bad,y_hut_train_gini,
                     dnn=c("Actual","Predicted"))
cm_test_gini<-table(credit_test$good_bad,y_hut_test_gini,
                    dnn = c("Actual","Predicted"))
mc_train_gini<-1-sum(diag(cm_train_gini))/sum(cm_train_gini)
mc_test_gini<-1-sum(diag(cm_test_gini)/sum(cm_test_gini))

#2.3 choose the optimal tree depth
train_score<-numeric(9)
valid_score<-numeric(9)
#fitting a model using tree
fit<-tree(good_bad~.,data = credit_train)
#vizualizing the tree
plot(x=fit,y=NULL)
text(x=fit,pretty=NULL)
for(i in 2:9){
  #refining the tree by returning the best model for i number of nodes
  refined_fit<-prune.tree(fit,best=i)
  train_score[i]<-deviance(refined_fit)
  pred<-predict(refined_fit,newdata=credit_validation,type="tree")
  valid_score[i]<-deviance(pred)
}
#graphs of deviances for the training and the validation data 
#on the number of leaves.
plot(2:9, train_score[2:9], type="b", col="red",ylim = c(250,590),lwd="2",
     xlab = "Number of nodes",ylab = "Deviance", main = "Deviance graph")
points(2:9, valid_score[2:9], type="b", col="blue",lwd="2")
legend("topright",c("Train score","Validation score"),fill=c("red","blue"))
#Report the optimal tree,report it's depth and the variables used by the tree.
#Interpret the information provided by the tree structure.
best_tree<-prune.tree(fit,best=4)
y_hut_test_prune<-predict(best_tree,credit_test,type="class")
#Estimate the misclassification rate for the test data.
cm_test_prune<-table(credit_test$good_bad,y_hut_test_prune,
                     dnn=c("Actual","Predicted"))
mc_test_prune<-1-sum(diag(cm_test_prune))/sum(cm_test_prune)

#2.4 Training data to perform classification using Naïve Bayes
naive_fit<-naiveBayes(good_bad~.,data = credit_train)
#predicting for the training and test data
y_hut_train_naive<-predict(naive_fit,newdata=credit_train)
y_hut_test_naive<-predict(naive_fit,newdata=credit_test)
#Confusion matrices and misclassification rates for train and test
cm_train_naive<-table(credit_train$good_bad,y_hut_train_naive,
                      dnn = c("Actual","Predicted"))
mc_train_naive<-1-sum(diag(cm_train_naive))/sum(cm_train_naive)
cm_test_naive<-table(credit_test$good_bad,y_hut_test_naive,
                     dnn = c("Actual","Predicted"))
mc_test_naive<-1-sum(diag(cm_test_naive))/sum(cm_test_naive)
#Compare the results with those from step 3? how ?2 ?
#2.5 ROC curve
j<-1
tpr_tree<-numeric()
fpr_tree<-numeric()
tpr_naive<-numeric()
fpr_naive<-numeric()
y_opt<-predict(best_tree,newdata=credit_test)
y_opt<-as.data.frame(y_opt)
y_nb<-predict(naive_fit,newdata=credit_test,type="raw")
y_nb<-as.data.frame(y_nb)
for(pi in seq(0.05, 0.95, 0.05)){
  #pruned tree
  y_piopt<-ifelse(y_opt$good>pi,1,0)
  cm_piopt<-table(credit_test$good_bad,y_piopt,
                        dnn=c("Actual","Predicted"))
  
  #naive bayes
  y_pinb<-ifelse(y_nb$good>pi,1,0)
  cm_pinb<-table(credit_test$good_bad,y_pinb,
                           dnn=c("Actual","Predicted"))
  tryCatch({
    #tpr and fpr for naive bayes
    tpr_naive[j]<-cm_pinb["good","1"]/sum(cm_pinb["good",])
    fpr_naive[j]<-cm_pinb["bad","1"]/sum(cm_pinb["bad",])
    #tpr and fpr for opt
    tpr_tree[j]<-cm_piopt["good","1"]/sum(cm_piopt["good",])
    fpr_tree[j]<-cm_piopt["bad","1"]/sum(cm_piopt["bad",])
    j<-j+1
  },
  error = function(e) {}
  )
}
#plotting the fpr vs tpr
plot(x=fpr_naive,y=tpr_naive,xlab = "False postive rate",
     ylab = "True positive rate",main = "ROC curve",type = "o",
     col="red",xlim = c(0,1),ylim = c(0,1))
points(x=fpr_tree,y=tpr_tree,col="blue",type = "o")
legend("top",c("Naive bayes","Decision Tree"),fill=c("red","blue"))
#2.6naive bayes with loss matrix
y_hut_train_naive_raw<-predict(naive_fit,credit_train,type="raw")
y_hut_test_naive_raw<-predict(naive_fit,credit_test,type="raw")
y_hut_train_naive_raw[,1]<-y_hut_train_naive_raw[,1]*10
y_hut_test_naive_raw[,1]<-y_hut_test_naive_raw[,1]*10
y_hut_train_lmnaive<-
  ifelse(y_hut_train_naive_raw[,2]>y_hut_train_naive_raw[,1],1,0)
y_hut_test_lmnaive<-
  ifelse(y_hut_test_naive_raw[,2]>y_hut_test_naive_raw[,1],1,0)
cm_train_lmnaive<-table(credit_train$good_bad,y_hut_train_lmnaive,
                        dnn=c("Actual","Predicted"))
mc_train_lmnaive<-1-sum(diag(cm_train_lmnaive))/sum(cm_train_lmnaive)
cm_test_lmnaive<-table(credit_test$good_bad,y_hut_test_lmnaive,
                       dnn=c("Actual","Predicted"))
mc_test_lmnaive<-1-sum(diag(cm_test_lmnaive))/sum(cm_test_lmnaive)

########################################
#Assignment 3. Uncertainty estimation
########################################
library(tree)
library(boot)
RNGversion("3.5.1")
# 3.1 importing the data into R and orderding with increase in MET
state_data_un<-read.csv2(file = "D:/Perriod 2/Machine Learning/Lab/Block 1 lab2/state.csv",sep=";")
state_data<-state_data_un[order(state_data_un$MET),]
plot(state_data$EX,state_data$MET,xlab="EX",ylab="MET",
     main="MET vs EX",lwd="4")
# 3.2 regression model with tree
#fitting a tree with minimum number of leaves =8
tree_model<-tree(EX~MET,data = state_data,
              control=tree.control(nobs=nrow(state_data),minsize = 8))
#Performing cross validation for the fitted model 
#to find the best number of nodes
cv_tree_model<-cv.tree(tree_model)
#plotting the deviances 
plot(cv_tree_model$size,cv_tree_model$dev,
     xlab="Size",
     ylab = "Deviance",
     main="Deviance plot",
     lwd="2",
     col="red",
     type="o")
#refining the tree to best number of nodes
refined_tree_model<-prune.tree(tree_model,best=3)
plot(refined_tree_model)
text(x=refined_tree_model,pretty=NULL)
#predicting with the best model
y_hut_rftree<-predict(refined_tree_model,data=state_data)
residual<-residuals(refined_tree_model)
#ploting the original data vs the predicted data
plot(y_hut_rftree,ylab="Fitted data",
     ylim = c(150,500),col="red",type = "o",
     main = "Orignal and Fitted data",lwd="2")
points(state_data$EX,col="blue",type = "o",lwd="2")
legend("topright",c("Fitted data","Original data"),fill = c("red","blue"))
#histogram of residuals
hist(residual)

# 3.3 95% confidence bands for non parametric boot strap
# computing bootstrap samples
f=function(data, ind){
  data1=data[ind,]# extract bootstrap sample
  res=tree(EX~MET, data=data1) #fit linear model
  #predict values for all Area values from the original data
  metP=predict(res,newdata=data) 
  return(metP)
}
res=boot(data=state_data,
         statistic=f,
         R=1000) #make bootstrap
#comupting 95% confidence bands
e<-envelope(res)
boot_tree<-tree(EX~MET,
                data=state_data,
                control=tree.control(nobs=nrow(state_data),minsize = 8))
metp<-predict(boot_tree)
plot(state_data$MET,state_data$EX,pch=21,bg="orange")#ploting the data points
points(state_data$MET,metp,type="l")#plotting the predicted values
points(state_data$MET,e$point[2,],type="l",col="blue")
points(state_data$MET,e$point[1,],type="l",col="red")
legend("top",c("Uppper CI","Boot Strap model","Lower CI"),
       fill=c("red","black","blue"))
width<-mean(e$point[1,]-e$point[2,])

# 3.4
mle <- prune.tree(tree(EX ~ MET, state_data, minsize = 8), best = 3) 

rng<- function(data, mle) {
  datanew <- data.frame(EX = data$EX, MET = data$MET)
  n       <- length(data$EX)
  # Generate new Expenditure
  datanew$EX <- rnorm(n, predict(mle, newdata = datanew), sd(residuals(mle)))
  return(datanew)
}

fp <- function(data){
  # Fit regression tree
  regtree <- prune.tree(tree(EX ~ MET, data, minsize = 8), best = 3)  
  # Predict values for all MET values from the original data
  exPredict <- predict(regtree, newdata = state_data)
  return(exPredict)
}
# Make bootstrap
para_bootstrap = boot(state_data, 
                      statistic = fp, 
                      R = 1000, 
                      mle = mle, 
                      ran.gen = rng, 
                      sim = "parametric")
ev<-envelope(para_bootstrap)
para_boot_tree<-tree(EX~MET,data=state_data,
                control=tree.control(nobs=nrow(state_data),minsize = 8))
metp<-predict(para_boot_tree)
plot(state_data$MET,state_data$EX,pch=21,bg="orange")#ploting the data points
points(state_data$MET,metp,type="l")#plotting the predicted values
points(state_data$MET,ev$point[2,],type="l",col="blue")
points(state_data$MET,ev$point[1,],type="l",col="red")
legend("top",c("Uppper CI","Boot Strap model","Lower CI"),
       fill=c("red","black","blue"))
wi<-mean(ev$point[1,]-ev$point[2,])

# 3.5


##########################################
#Assignment 4. Principal components
##########################################
# 4.1 Doing PCI Analysis 
spectra<-read.csv2(file = "D:/Perriod 2/Machine Learning/Lab/Block 1 lab2/NIRspectra.csv",sep=";")
spectrum<-spectra
spectrum$Viscosity<-c()
#doing a PCA analysis 
pca_res<-prcomp(spectrum)
#getting the eigen vectors
lambda<-pca_res$sdev^2
#proportion of variation with respect to each feature
vp<-as.numeric(sprintf("%2.3f",lambda/sum(lambda)*100))
#screeplot(pca_res)

plot(pca_res$x[,1],pca_res$x[,2],xlab = "PCI 1",ylab = "PCI 2")

# 4.2 Trace plots for the selected  
trace<-pca_res$rotation
plot(trace[,1],main = "Traceplot PCI 1",ylab ="Score")
plot(trace[,2],main = "Traceplot PCI 2",ylab ="Score")

