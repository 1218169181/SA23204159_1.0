## -----------------------------------------------------------------------------
library(SA23204159)
data(lung)
head(lung,5)

## ----eval=FALSE---------------------------------------------------------------
#  XGB_md<-function(n_month,data,seed_number=12345){
#    set.seed(seed_number)
#    n<-dim(data)[1]  ##nrow of data
#    p<-dim(data)[2]  ##ncol of data
#    os<-numeric(n)   ##used to save the survival information of patients at n_month
#    status<-data[,p]  ##patients' endpoint status
#    survival_month<-data[,(p-1)]  ##the information of survival months
#    data_temp<-data[,c(-(p-1),-p)] ##delete column survival and status
#    data_temp<-lapply(data_temp,factor)
#    data_temp<-lapply(data_temp,as.numeric)
#    data_temp<-data.frame(data_temp)
#  
#    ## patients status at n_month,0:patient is alive,1:patient is dead,2:patient is censored before n_month
#    for(i in c(1:n)){
#      if(survival_month[i]<n_month){
#        if(status[i]==2)
#        {
#          os[i]=1
#        }
#        else
#        {
#          os[i]=2
#        }
#      }
#      else{
#        os[i]=0
#      }
#    }
#  
#    data_temp$os<-as.numeric(os)
#    ##delete the part of data which patients have been censored before n_month
#    data_temp<-data_temp[data_temp$os!=2,]
#    ##devide the data into train set(0.7) and test set(0.3)
#    train_ind<-sample(1:nrow(data_temp),round(nrow(data_temp)*0.7))
#    train_data<-data_temp[train_ind,]
#    test_data<-data_temp[-train_ind,]
#  
#  
#    train_matrix <- sparse.model.matrix(os ~ .-1, data = train_data)
#    test_matrix <- sparse.model.matrix(os~ .-1, data = test_data)
#    train_fin <- list(data=train_matrix,label=as.numeric(train_data$os))
#    test_fin <- list(data=test_matrix,label=as.numeric(test_data$os))
#    dtrain <- xgb.DMatrix(data = train_fin$data, label = train_fin$label)
#    dtest <- xgb.DMatrix(data = test_fin$data, label = test_fin$label)
#  
#    best_param = list()
#    best_seednumber = seed_number
#    best_logloss = Inf
#    best_logloss_index = 0
#  
#    ##use cross validation XGBoost to search for a good combination of parameter
#    for (iter in 1:100) {
#      param <- list(objective = "multi:softprob",
#                    eval_metric = "mlogloss",
#                    num_class = 2,
#                    max_depth = sample(6:10, 1),
#                    eta = runif(1, .01, .3),
#                    gamma = runif(1, 0.0,2),
#                    subsample = runif(1, .6, .9),
#                    colsample_bytree = runif(1, .5, .8),
#                    min_child_weight = sample(1:40, 1),
#                    max_delta_step = sample(1:10, 1),
#                    alpha=runif(1,0,3))
#  
#      cv.nround = 1000
#      cv.nfold = 10
#      seed.number = sample.int(10000, 1)[[1]]
#      set.seed(seed.number)
#      mdcv <- xgb.cv(data=dtrain, params = param, nthread=6,
#                     nfold=cv.nfold, nrounds=cv.nround,
#                     verbose = 0, early_stopping_rounds=8, maximize=FALSE)
#  
#      min_logloss = min(mdcv$evaluation_log$test_mlogloss_mean)
#      min_logloss_index = which.min(mdcv$evaluation_log$test_mlogloss_mean)
#  
#      if (min_logloss < best_logloss) {
#        best_logloss = min_logloss
#        best_logloss_index = min_logloss_index
#        best_seednumber = seed.number
#        best_param = param
#      }
#    }
#  
#    nround = best_logloss_index
#    set.seed(best_seednumber)
#  
#    ##use the best parameter in cross validation to train a XGBoost model
#    md <- xgboost(data=dtrain, params=best_param, nrounds=nround, nthread=6,verbose = 0)
#    pre_xgb_train = predict(md,newdata = dtrain)
#    pre_train<-numeric(length(train_ind))
#    for(i in 1:length(train_ind)){
#      pre_train[i]=pre_xgb_train[2*i]
#    }
#    xgboost_prob_train <- roc(train_fin$label,as.numeric(pre_train),levels = c(0,1),
#                             direction='<')
#  
#    pre_xgb_test = predict(md,newdata = dtest)
#    pre_test<-numeric(dim(data_temp)[1]-length(train_ind))
#    for(i in 1:(dim(data_temp)[1]-length(train_ind))){
#      pre_test[i]=pre_xgb_test[2*i]
#    }
#    xgboost_prob_test <- roc(test_fin$label,as.numeric(pre_test),levels = c(0,1),
#                            direction='<')
#  
#    confusion_matrix<-confusionMatrix(factor(round(pre_test),levels=c(0,1)),factor(test_fin$label,levels=c(0,1)))
#  
#    return(list(prob_train=xgboost_prob_train,prob_test=xgboost_prob_test,confusion_matrix=confusion_matrix$table,md=md))
#  }

## ----eval=FALSE---------------------------------------------------------------
#  plot_roc<-function(roc_value){
#    plot.roc(roc_value, print.auc=TRUE, auc.polygon=TRUE,
#         grid=c(0.1, 0.2),grid.col=c("green", "red"),
#         max.auc.polygon=TRUE,auc.polygon.col="skyblue",
#         print.thres=TRUE,main='prognostic model of XGBoost algorithm')
#  }

## ----eval=FALSE---------------------------------------------------------------
#  plot_confusion_matrix<-function(confusion_matrix){
#    fourfoldplot(confusion_matrix,color=c("#2E86C1","#D6EAF8"),main = "Confusion Matrix")
#  }

## -----------------------------------------------------------------------------
library(SA23204159)
data(lung)
data=lung
##由于数据较多，从数据中随机抽取2000个样本
ind<-sample(c(1:dim(data)[1]),2000)
data<-data[ind,]
res<-XGB_md(18,data)
plot_roc(res$prob_train)
plot_confusion_matrix(res$confusion_matrix)

## ----eval=FALSE---------------------------------------------------------------
#  studentized_bootstrap_ci <- function(X,B=500) {
#    n<-length(X)
#    theta_star<-numeric(B)
#    se_star<-numeric(B)
#    for(i in 1:B){   ##一次抽样
#      ind<-sample(c(1:n),replace = TRUE)
#      x_star<-X[ind]
#      theta_star[i]<-mean(x_star)
#      theta_star_second<-numeric(B)
#      for(j in 1:B){  ##二次抽样
#        ind_second<-sample(c(1:n),replace = TRUE)
#        x_star_second<-x_star[ind_second]
#        theta_star_second[j]<-mean(x_star_second)
#      }
#      se_star[i]<-sd(theta_star_second)
#    }
#    theta<-mean(X)
#    se1<-sd(theta_star)
#    quantile_1<-quantile((theta_star-theta)/se_star,c(0.025,0.975))
#    lower<-theta-as.numeric(quantile_1[2])*se1
#    upper<-theta-as.numeric(quantile_1[1])*se1
#    return(c(lower,upper))
#  }

## ----eval=FALSE---------------------------------------------------------------
#  NumericVector studentized_bootstrap_ci_C(NumericVector X, int B=500) {
#    NumericVector ci(2);
#    int n=X.size();
#    NumericVector theta_star(B);
#    NumericVector se_star(B);
#    NumericVector x_star(n);
#    NumericVector theta_star_second(B);
#    NumericVector x_star_second(n);
#    int ind1,ind2;
#    double sum;
#    double mean;
#    int ind;
#    double variance;
#    for(int i = 0; i < B; i++) {
#      for(int j=0;j<n;j++){
#        ind=0 + rand()%n;
#        x_star[j]=X[ind];
#      }
#      sum = std::accumulate(std::begin(x_star), std::end(x_star), 0.0);
#      theta_star[i]=sum/n;
#      for(int k=0;k<B;k++){
#        for(int j=0;j<n;j++){
#        ind=rand()%n;
#        x_star_second[j]=x_star[ind];
#      }
#      sum = std::accumulate(std::begin(x_star_second), std::end(x_star_second), 0.0);
#      theta_star_second[k]=sum/n;
#      }
#      sum = std::accumulate(std::begin(theta_star_second), std::end(theta_star_second), 0.0);
#      mean=sum/B;
#      variance = 0.0;
#      std::for_each(std::begin(theta_star_second), std::end(theta_star_second), [&](const double d) {
#          variance += pow(d-mean, 2);
#      });
#      variance /= B;
#      se_star[i]=sqrt(variance);
#    }
#    double theta;
#    sum = std::accumulate(std::begin(X), std::end(X), 0.0);
#    theta=sum/n;
#  
#    sum = std::accumulate(std::begin(theta_star), std::end(theta_star), 0.0);
#    mean=sum/B;
#    variance = 0.0;
#    std::for_each(std::begin(theta_star_second), std::end(theta_star_second), [&](const double d) {
#        variance += pow(d-mean, 2);
#    });
#    variance /= B;
#    double se1=sqrt(variance);
#    NumericVector t(B);
#    for(int k=0;k<B;k++){
#        t[k]=(theta_star[k]-theta)/se_star[k];
#    }
#    std::sort(t.begin(),t.end());
#    ind1=round(0.025*n-1);
#    ind2=round(0.975*n-1);
#    double q1=t[ind1];
#    double q2=t[ind2];
#    ci[0]=theta-q2*se1;
#    ci[1]=theta-q1*se1;
#    return(ci);
#  }

## -----------------------------------------------------------------------------
library(SA23204159)
data(lung)
data=lung
##由于数据较多，从数据中随机抽取1000个样本
ind<-sample(c(1:dim(data)[1]),1000)
data<-data[ind,]
ci1<-studentized_bootstrap_ci(data$存活时间)
ci2<-studentized_bootstrap_ci_C(data$存活时间) ##Rcpp version
print(ci1)
print(ci2)

## -----------------------------------------------------------------------------
library(microbenchmark)
library(SA23204159)
data(lung)
data=lung
##由于数据较多，从数据中随机抽取500个样本
ind<-sample(c(1:dim(data)[1]),500)
data<-data[ind,]
tm <- microbenchmark(
  vR = studentized_bootstrap_ci(data$存活时间),
  vC = studentized_bootstrap_ci_C(data$存活时间),   ##Rcpp version
  times = 20
)
knitr::kable(summary(tm)[,c(1,3,5,6)])

