#' @import microbenchmark
#' @import pROC
#' @import knitr
#' @import DAAG
#' @import boot
#' @import bootstrap
#' @import coda
#' @import rmarkdown
#' @importFrom Matrix sparse.model.matrix
#' @importFrom xgboost xgb.DMatrix xgb.cv xgboost
#' @importFrom stats predict
#' @importFrom pROC roc plot.roc
#' @importFrom caret confusionMatrix
#' @importFrom graphics fourfoldplot
#' @importFrom Rcpp evalCpp
#' @importFrom stats quantile runif sd rnorm
#' @importFrom e1071 svm
#' @useDynLib SA23204159
NULL


#' @title A illustration dataset
#' @name lung
#' @description A dataset which contained information of patients who have lung cancer .This dataset is used to illustrate the how to construct a XGBoost prognostic model.The last two column is the endpoint status and survival months of patient,Status 1 represents the patient is censored at endpoint and status 2 represents the patient is dead at endpoint.The rest of colmuns are the covariate.
#' @examples
#' \dontrun{
#' data(lung)
#' attach(lung)
#' head(lung)
#' }
NULL


#' @title A Gibbs sampler using R
#' @description A Gibbs sampler using R
#' @param n_month the number of month that we want to predict the survival of patient
#' @param data the dataset that we use to construct XGBoost prognostic model.The data should in the form that last columns is the endpoint of status and
#' @param seed_number the number of seed that we want to set
#' @return a list contains prob_train,prob_test,confusionmatrix,md.  prob_train:predicted probility of dead at endpoint on train data,prob_test:predicted probility of dead at endpoint on test data,confusion_matrix:confusion matrix on test data,md:predictive model
#' @examples
#' \dontrun{
#' data(lung)
#' attach(lung)
#' XGB_md(6,lung)
#' }
#' @export
XGB_md<-function(n_month,data,seed_number=12345){
  set.seed(seed_number)
  n<-dim(data)[1]  ##nrow of data
  p<-dim(data)[2]  ##ncol of data
  os<-numeric(n)   ##used to save the survival information of patients at n_month
  status<-data[,p]  ##patients' endpoint status
  survival_month<-data[,(p-1)]  ##the information of survival months
  data_temp<-data[,c(-(p-1),-p)] ##delete column survival and status
  data_temp<-lapply(data_temp,factor)
  data_temp<-lapply(data_temp,as.numeric)
  data_temp<-data.frame(data_temp)
  
  ## patients status at n_month,0:patient is alive,1:patient is dead,2:patient is censored before n_month
  for(i in c(1:n)){
    if(survival_month[i]<n_month){
      if(status[i]==2)
      {
        os[i]=1
      }
      else
      {
        os[i]=2
      }
    }
    else{
      os[i]=0
    }
  }
  
  data_temp$os<-as.numeric(os)
  ##delete the part of data which patients have been censored before n_month
  data_temp<-data_temp[data_temp$os!=2,]
  ##devide the data into train set(0.7) and test set(0.3)
  train_ind<-sample(1:nrow(data_temp),round(nrow(data_temp)*0.7))
  train_data<-data_temp[train_ind,]
  test_data<-data_temp[-train_ind,]
  
  
  train_matrix <- sparse.model.matrix(os ~ .-1, data = train_data)
  test_matrix <- sparse.model.matrix(os~ .-1, data = test_data)
  train_fin <- list(data=train_matrix,label=as.numeric(train_data$os)) 
  test_fin <- list(data=test_matrix,label=as.numeric(test_data$os)) 
  dtrain <- xgb.DMatrix(data = train_fin$data, label = train_fin$label) 
  dtest <- xgb.DMatrix(data = test_fin$data, label = test_fin$label)
  
  best_param = list()
  best_seednumber = seed_number
  best_logloss = Inf
  best_logloss_index = 0
  
  ##use cross validation XGBoost to search for a good combination of parameter
  for (iter in 1:100) {
    param <- list(objective = "multi:softprob",
                  eval_metric = "mlogloss",
                  num_class = 2,
                  max_depth = sample(6:10, 1),
                  eta = runif(1, .01, .3),
                  gamma = runif(1, 0.0,2), 
                  subsample = runif(1, .6, .9),
                  colsample_bytree = runif(1, .5, .8), 
                  min_child_weight = sample(1:40, 1),
                  max_delta_step = sample(1:10, 1),
                  alpha=runif(1,0,3))
    
    cv.nround = 1000
    cv.nfold = 10
    seed.number = sample.int(10000, 1)[[1]]
    set.seed(seed.number)
    mdcv <- xgb.cv(data=dtrain, params = param, nthread=6, 
                   nfold=cv.nfold, nrounds=cv.nround,
                   verbose = 0, early_stopping_rounds=8, maximize=FALSE)
    
    min_logloss = min(mdcv$evaluation_log$test_mlogloss_mean)
    min_logloss_index = which.min(mdcv$evaluation_log$test_mlogloss_mean)
    
    if (min_logloss < best_logloss) {
      best_logloss = min_logloss
      best_logloss_index = min_logloss_index
      best_seednumber = seed.number
      best_param = param
    }
  }
  
  nround = best_logloss_index
  set.seed(best_seednumber)
  
  ##use the best parameter in cross validation to train a XGBoost model 
  md <- xgboost(data=dtrain, params=best_param, nrounds=nround, nthread=6,verbose = 0)
  pre_xgb_train = predict(md,newdata = dtrain)
  pre_train<-numeric(length(train_ind))
  for(i in 1:length(train_ind)){
    pre_train[i]=pre_xgb_train[2*i]
  }
  xgboost_prob_train <- roc(train_fin$label,as.numeric(pre_train),levels = c(0,1),
                           direction='<')
  
  pre_xgb_test = predict(md,newdata = dtest)
  pre_test<-numeric(dim(data_temp)[1]-length(train_ind))
  for(i in 1:(dim(data_temp)[1]-length(train_ind))){
    pre_test[i]=pre_xgb_test[2*i]
  }
  xgboost_prob_test <- roc(test_fin$label,as.numeric(pre_test),levels = c(0,1),
                          direction='<')
  
  confusion_matrix<-confusionMatrix(factor(round(pre_test),levels=c(0,1)),factor(test_fin$label,levels=c(0,1)))
  
  return(list(prob_train=xgboost_prob_train,prob_test=xgboost_prob_test,confusion_matrix=confusion_matrix$table,md=md))
}


#' @title A function used to plot roc curve
#' @description A function used to plot roc curve
#' @param roc_value a series of prediction probility
#' @return NULL
#' @examples
#' \dontrun{
#' data(lung)
#' attach(lung)
#' res<-XGB_md(6,lung)
#' plot_roc(res$prob_train)
#' }
#' @export
plot_roc<-function(roc_value){
  plot.roc(roc_value, print.auc=TRUE, auc.polygon=TRUE, 
       grid=c(0.1, 0.2),grid.col=c("green", "red"), 
       max.auc.polygon=TRUE,auc.polygon.col="skyblue", 
       print.thres=TRUE,main='prognostic model of XGBoost algorithm')
}

#' @title A function used to visualize confusion matrix
#' @description A function used to visualize confusion matrix
#' @param confusion_matrix A confusion matrix
#' @return NULL
#' @examples
#' \dontrun{
#' data(lung)
#' attach(lung)
#' res<-XGB_md(6,lung)
#' plot_confusion_matrix(res$confusion_matrix)
#' }
#' @export
plot_confusion_matrix<-function(confusion_matrix){
  fourfoldplot(confusion_matrix,color=c("#2E86C1","#D6EAF8"),main = "Confusion Matrix")
}

#' @title construct studentized bootstrap CI
#' @description construct studentized bootstrap CI
#' @param X the variable that you want to construct CI,must be a vector
#' @param B the number of times that replicate
#' @return lower and upper bound of CI
#' @examples
#' \dontrun{
#' data(lung)
#' attach(lung)
#' ##construct CI for patients' survival month,use lung data
#' data<-lung$存活时间
#' studentized_bootstrap_ci(data)
#' }
#' @export
studentized_bootstrap_ci <- function(X,B=500) {
  n<-length(X)
  theta_star<-numeric(B)
  se_star<-numeric(B)
  for(i in 1:B){   ##一次抽样
    ind<-sample(c(1:n),replace = TRUE)
    x_star<-X[ind]
    theta_star[i]<-mean(x_star)
    theta_star_second<-numeric(B)
    for(j in 1:B){  ##二次抽样
      ind_second<-sample(c(1:n),replace = TRUE)
      x_star_second<-x_star[ind_second]
      theta_star_second[j]<-mean(x_star_second)
    }
    se_star[i]<-sd(theta_star_second)
  }
  theta<-mean(X)
  se1<-sd(theta_star)
  quantile_1<-quantile((theta_star-theta)/se_star,c(0.025,0.975))
  lower<-theta-as.numeric(quantile_1[2])*se1
  upper<-theta-as.numeric(quantile_1[1])*se1
  return(c(lower,upper))
}