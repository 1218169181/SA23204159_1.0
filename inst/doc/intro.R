## -----------------------------------------------------------------------------
library(SA23204159)
data(lung)
head(lung,5)

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

