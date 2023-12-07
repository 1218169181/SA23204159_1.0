## -----------------------------------------------------------------------------
x0<-rep(1,100)
x1<-rnorm(100)
x2<-runif(100,min=-1,max=1)
eps<-rnorm(100)
beta0<-runif(1,min=1,max=10)
beta1<-runif(1,min=0,max=2)
beta2<-runif(1,min=0,max=2)
y=beta0*x0+beta1*x1+beta2*x2+eps
lm_1<- lm(y ~ x1+x2)
plot(lm_1)

## -----------------------------------------------------------------------------
boxplot(Sepal.Length~Species,data=iris)
boxplot(Sepal.Width~Species,data=iris)
boxplot(Petal.Length~Species,data=iris)
boxplot(Petal.Width~Species,data=iris)

## -----------------------------------------------------------------------------
knitr::kable(head(iris))

## -----------------------------------------------------------------------------
library(e1071)
length=iris[iris$Species!="setosa",1]
width=iris[iris$Species!="setosa",2]
species=iris$Species[iris$Species!="setosa"]
plot(x=length,y=width,col=ifelse(species=="versicolor",1,3),pch=20)
svm_1<-svm(species~length+width, scale=FALSE, kernel='linear', gamma=2, cost= 0.2)
w=t(svm_1$coefs)%*%svm_1$SV
b=-svm_1$rho
abline(a=-b/w[1,2],b=-w[1,1]/w[1,2],col="red",lty=5)
text(6,3, "SVM classifier boundary", col = "red", adj = c(-.1, -.1))
legend(4.8, 3.6, legend = c("versicolor", "virginiaca"),
       col = c(1, 3), lty = 2:3, cex = 0.6)

## -----------------------------------------------------------------------------
my.sample<-function(x,size,...){
  #x为待抽样样本集，size为抽样大小，prob为每个样本对应的抽样概率，缺省prob时即为等概率抽样
  var_args <- list(...)
  prob <- if(!is.null(var_args[["prob"]])) var_args[["prob"]] else rep(1,length(x))/length(x)
  cp <- cumsum(prob)
  U = runif(size)
  r <- x[findInterval(U,cp)+1]
}

## -----------------------------------------------------------------------------
mysample1<-my.sample(c(1,2,3,4),1000,prob=c(.1,.2,.3,.4))
sample1<-sample(c(1,2,3,4),1000,replace=TRUE,prob=c(.1,.2,.3,.4))

mysample2<-my.sample(c(1,2,3,4,5),1000,prob=c(.1,.2,.2,.2,.3))
sample2<-sample(c(1,2,3,4,5),size=1000,replace=TRUE,prob=c(.1,.2,.2,.2,.3))

#等权重情况
mysample3<-my.sample(c(1,2,3),1000)
sample3<-sample(c(1,2,3),size=1000,replace=TRUE)

barplot(height = matrix(c(as.vector(table(mysample1)),as.vector(table(sample1))), ncol = 4, nrow = 2,byrow=TRUE),  # 绘图数据（矩阵）
        names.arg = c('1', '2', '3','4'),  # 柱子名称
        col = c('orange', 'steelblue'),  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        ylab = '频数',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        legend.text = c('my.sample', 'sample'),  # 图例文本
        beside = TRUE  # 是否平行排列
       )

barplot(height = matrix(c(as.vector(table(mysample2)),as.vector(table(sample2))), ncol = 5, nrow = 2,byrow=TRUE),  # 绘图数据（矩阵）
        names.arg = c('1', '2', '3','4','5'),  # 柱子名称
        col = c('orange', 'steelblue'),  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        ylab = '频数',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        legend.text = c('my.sample', 'sample'),  # 图例文本
        beside = TRUE  # 是否平行排列
       )

barplot(height = matrix(c(as.vector(table(mysample3)),as.vector(table(sample3))), ncol = 3, nrow = 2,byrow=TRUE),  # 绘图数据（矩阵）
        names.arg = c('1', '2', '3'),  # 柱子名称
        col = c('orange', 'steelblue'),  # 填充颜色
        border = '#ffffff',   # 轮廓颜色
        ylab = '频数',  # Y轴名称
        horiz = FALSE,  # 是否为水平放置
        legend.text = c('my.sample', 'sample'),  # 图例文本
        beside = TRUE  # 是否平行排列
       )

## -----------------------------------------------------------------------------
my.rlaplace<-function(n){
  u<-runif(n)
  r<-ifelse(u<0.5,log(2*u),-log(2*(1-u)))
}

laplace.sample<-my.rlaplace(1000)
hist(laplace.sample,prob=TRUE,breaks=50,ylim=c(0,0.5))
y <- seq(-7, 7, .01)
lines(y, 0.5*exp(-abs(y)))

## -----------------------------------------------------------------------------
##计算Beta分布的概率密度函数
my.dbeta<-function(x,a,b){
  ##Beta(a,b)的值
  beta.value<-factorial(a-1)*factorial(b-1)/factorial(a+b-1)
  result<-1/beta.value*x^(a-1)*(1-x)^(b-1)
}

##生成Beta分布随机数
my.rbeta<-function(n,a,b){
  ##Beta(a,b)的值
  beta.value<-factorial(a-1)*factorial(b-1)/factorial(a+b-1)
  
  ##f(x,a,b)的最大值
  max.value=1/beta.value*((a-1)/(a+b-2))^(a-1)*((b-1)/(a+b-2))^(b-1)
  k=0
  y=numeric(n)
  while(k<n){
    u=runif(1)
    x <- runif(1) #random variate from g(.)
    if (my.dbeta(x,a,b)/max.value > u) {
    #we accept x
    k <- k + 1
    y[k] <- x
  }
  }
  y
}
result<-my.rbeta(1000,3,2)
hist(result,prob=TRUE,breaks=25)
y <- seq(0, 1, .01)
lines(y, my.dbeta(y,3,2))

## -----------------------------------------------------------------------------
my.rEpan<-function(n){
  u1<-runif(n,min=-1,max=1)
  u2<-runif(n,min=-1,max=1)
  u3<-runif(n,min=-1,max=1)
  result<-ifelse((abs(u3)>=abs(u2))&(abs(u3)>=abs(u1)),u2,u3)
}

result<-my.rEpan(100000)
hist(result,prob=TRUE,breaks=50)
y <- seq(-1, 1, .01)
lines(y, 3/4*(1-y^2))

## -----------------------------------------------------------------------------
set.seed(12345)
m <- 1e6
d <- 1
n_replicate<-100

##rho=1
l=1
rho_1=c()
for(i in 1:n_replicate){
  X <- runif(m,0,d/2)
  Y <- runif(m,0,pi/2)
  pihat <- 2*l/d/mean(l/2*sin(Y)>X)
  rho_1[i]=pihat
}

##rho=0.8
l <- 0.8
rho_2=c()
for(i in 1:n_replicate){
  X <- runif(m,0,d/2)
  Y <- runif(m,0,pi/2)
  pihat <- 2*l/d/mean(l/2*sin(Y)>X)
  rho_2[i]=pihat
}

##rho=0.8
l <- 0.5
rho_3=c()
for(i in 1:n_replicate){
  X <- runif(m,0,d/2)
  Y <- runif(m,0,pi/2)
  pihat <- 2*l/d/mean(l/2*sin(Y)>X)
  rho_3[i]=pihat
}

var1=var(rho_1)
var2=var(rho_2)
var3=var(rho_3)
var<-c(m*var1,m*var2,m*var3,pi^3/2-pi^2)
rho<-c("1","0.8","0.5","理论最小方差")
result<-data.frame(rho=rho,var=var)
print(result)

## -----------------------------------------------------------------------------
MC <- function(m , antithetic = TRUE) {
  u <- runif(m/2)
  if (!antithetic) {
    v <- runif(m/2) 
    }
  else{
    v <- 1 - u
  }
  u <- c(u, v)
  theta.hat<-mean(exp(u))
}
duiou_method=c()
MC_method=c()
m=10000
for(i in 1:10000){
  duiou_method[i]=MC(m)
  MC_method[i]=MC(m,antithetic=FALSE)
}

theoretical_var_duiou=(10*exp(1)-3*exp(2)-5)/(2*m)
theoretical_var_MC=(4*exp(1)-exp(2)-3)/(2*m)
theoretical_ratio=theoretical_var_duiou/theoretical_var_MC

empirical_var_duiou=var(duiou_method)
empirical_var_MC=var(MC_method)
empirical_ratio=empirical_var_duiou/empirical_var_MC

cat("对偶方法的理论方差和经验方差分别为：",theoretical_var_duiou,empirical_var_duiou,"\n")
cat("简单MC方法的理论方差和经验方差分别为：",theoretical_var_MC,empirical_var_MC,"\n")
cat("两种方法的理论方差之比和经验方差之比分别为：",theoretical_ratio,empirical_ratio,"\n")

## -----------------------------------------------------------------------------
rm(list = ls())
set.seed(123)

m <- 100000
theta.hat <- se <- numeric(2)
g <- function(x) {
  x^2/sqrt(2*pi)*exp(-x^2/2) * (x > 1)
}

x<-rnorm(m)  #using f1
fg<-g(x)/dnorm(x)
theta.hat[1]<-mean(fg)
se[1]<-sd(fg)/(sqrt(m))

x <- rexp(m, 1) #using f2
fg <- g(x) / exp(-x)
theta.hat[2] <- mean(fg)
se[2] <- sd(fg)/sqrt(m)

plot(1, type = "n", xlab = "",
     ylab = "", xlim = c(1, 10), 
     ylim = c(0, 0.5))
x=seq(from=1, to=10, by=0.01)
lines(x=x,y=g(x),col="black")
lines(x=x,y=dnorm(x),col="red")
lines(x=x,y=exp(-x),col="green")
legend("topright", c("g", "f1","f2"),
       lty = c(1,1,1),
       col = c("black","red", "green"))

plot(1, type = "n", xlab = "",
     ylab = "", xlim = c(1, 10), 
     ylim = c(0, 2))
lines(x=x,y=g(x)/dnorm(x),col="red")
lines(x=x,y=g(x)/exp(-x),col="green")
legend("topright", c("g/f1","g/f2"),
       lty = c(1,1),
       col = c("red", "green"))


## -----------------------------------------------------------------------------
set.seed(123)
M<-10000
k<-5
r<-M/k
theta<-numeric(k)
var_j<-numeric(k)
g<-function(x){
  exp(-x)/(1+x^2)*(x>0)*(x<1)
}
for(i in 1:k){
  u<-runif(r,(i-1)/k,i/k)
  theta[i]<-mean(g(u))
  var_j[i]<-var(g(u))
}

value_est<-mean(theta)
var_est<-mean(var_j)/M
print(c(value_est,sqrt(var_est)))

## -----------------------------------------------------------------------------
set.seed(123)
M<-1000  #实验重复次数
n <- 20
alpha <- .05
cov1<-numeric(M)
cov2<-numeric(M)
for(i in 1:M){
  x <- rchisq(n,2)
  UCL <- (n-1) * var(x) / qchisq(alpha, df=n-1)
  cov1[i]<-4<UCL
  temp_value=abs(sqrt(n)*mean(x-2))/sqrt(var(x))
  cov2[i]<-temp_value<qt(1-alpha/2,n-1)
}
print(c(mean(cov2),mean(cov1)))

## -----------------------------------------------------------------------------
set.seed(123)
mu<-1 #null hypothesis
M<-10000
n<-20
p.val1<-numeric(M)
p.val2<-numeric(M)
p.val3<-numeric(M)
for(i in 1:M){
  x1<-rchisq(n,1)
  x2<-runif(n,0,2)
  x3<-rexp(n,1)
  p.val1[i]<-2*(1-pt(abs(sqrt(n)*mean(x1-mu))/sqrt(var(x1)),n-1))
  p.val2[i]<-2*(1-pt(abs(sqrt(n)*mean(x2-mu))/sqrt(var(x2)),n-1))
  p.val3[i]<-2*(1-pt(abs(sqrt(n)*mean(x3-mu))/sqrt(var(x3)),n-1))
}
print(c(mean(p.val1<=0.05),mean(p.val2<=0.05),mean(p.val3<=0.05)))

## -----------------------------------------------------------------------------
rm(list = ls())
set.seed(12345)
m<-1000
m_null<-m*0.95
m_alter<-m*0.05
M<-1000
alpha=0.1
value1<-c(rep(0,times=m_null),rep(1,times=m_alter))   #950个0和50个1，0代表原假设成立，1代表备择假设成立
Bonf_FWER<-numeric(M)
Bonf_FDR<-numeric(M)
Bonf_TPR<-numeric(M)
BH_FWER<-numeric(M)
BH_FDR<-numeric(M)
BH_TPR<-numeric(M)
for(i in 1:M){
  p_value_null<-runif(m_null)  ##原假设成立时的p值
  p_value_alter<-rbeta(m_alter,0.1,1)  ##备择假设成立时的p值
  p_value<-c(p_value_null,p_value_alter) ##全体1000p值
  p_value_Bonf<-p.adjust(p_value,method="bonferroni")
  p_value_BH<-p.adjust(p_value,method = "BH")
  value_Bonf<-ifelse(p_value_Bonf<alpha,1,0)  ##在Bonferroni校正后，1代表拒绝原假设，0代表接受原假设
  value_BH<-ifelse(p_value_BH<alpha,1,0)      ##在B-H校正后，1代表拒绝原假设，0代表接受原假设
  table_Bonf<-table(value_Bonf,value1)        ##table[1,1]为U，table[1,2]为T，table[2,1]为V，table[2,2]为S
  table_BH<-table(value_BH,value1)
  Bonf_FWER[i]<-table_Bonf[2,1]>0
  BH_FWER[i]<-table_BH[2,1]>0
  Bonf_FDR[i]<-table_Bonf[2,1]/(table_Bonf[2,1]+table_Bonf[2,2])
  BH_FDR[i]<-table_BH[2,1]/(table_BH[2,1]+table_BH[2,2])
  Bonf_TPR<-table_Bonf[2,2]/(table_Bonf[1,2]+table_Bonf[2,2])
  BH_TPR<-table_BH[2,2]/(table_Bonf[1,2]+table_BH[2,2])
}
result<-data.frame(method=c("Bonf","B-H"),FWER=c(mean(Bonf_FWER),mean(BH_FWER)),FDR=c(mean(Bonf_FDR),mean(BH_FDR)),TPR=c(mean(Bonf_TPR),mean(BH_TPR)))
print(result)

## -----------------------------------------------------------------------------
lambda<-2
B<-1000
bootstrap_exp<-function(n,m,B){
  boot_bias<-numeric(m)
  boot_se<-numeric(m)
  for(i in 1:m){
    x<-rexp(n,lambda)
    theta<-1/mean(x)
    thetastar<-numeric(B)
    for(b in 1:B){
      xstar<-sample(x,replace=TRUE)
      thetastar[b]<-1/mean(xstar)
    }
    boot_bias[i]<-mean(thetastar)-theta
    boot_se[i]<-sd(thetastar)
  }
  c(boot.bias=mean(boot_bias),the.bias=lambda/(n-1),boot.se=mean(boot_se),the.se=lambda*n/((n-1)*sqrt(n-2)))
}
result1=bootstrap_exp(5,m,B)
result2=bootstrap_exp(10,m,B)
result3=bootstrap_exp(20,m,B)
result=data.frame(n=c(5,10,20),boot.bias=c(result1[1],result2[1],result3[1]),
                  the.bias=c(result1[2],result2[2],result3[2]),
                  boot.se=c(result1[3],result2[3],result3[3]),
                  the.se=c(result1[4],result2[4],result3[4]))
print(result)

## -----------------------------------------------------------------------------
set.seed(12345)
library(bootstrap)
library(boot)
x1<-cbind(law$LSAT, law$GPA)     ##包含15个样本
x2<-cbind(law82$LSAT, law82$GPA) ##包含82个样本
B<-1000
theta1_star<-numeric(B)
theta2_star<-numeric(B)
se1_star<-numeric(B)
se2_star<-numeric(B)
for(i in 1:B){
  ind1<-sample(c(1:dim(x1)[1]),replace=TRUE)
  ind2<-sample(c(1:dim(x2)[1]),replace=TRUE)
  x1star<-x1[ind1,]
  x2star<-x2[ind2,]
  theta1_star[i]<-cor(x1star[,1],x1star[,2])
  theta2_star[i]<-cor(x2star[,1],x2star[,2])
  theta1_star_second<-numeric(B)
  theta2_star_second<-numeric(B)
  for(j in 1:B){         ##二次抽样
    ind1<-sample(c(1:dim(x1)[1]),replace=TRUE)
    ind2<-sample(c(1:dim(x2)[1]),replace=TRUE)
    x1star_second<-x1star[ind1,]
    x2star_second<-x2star[ind2,]
    theta1_star_second[j]<-cor(x1star_second[,1],x1star_second[,2])
    theta2_star_second[j]<-cor(x2star_second[,1],x2star_second[,2])
  }
  se1_star[i]<-sd(theta1_star_second)
  se2_star[i]<-sd(theta2_star_second)
}
theta1<-cor(law$LSAT, law$GPA)
theta2<-cor(law82$LSAT, law82$GPA)
se1<-sd(theta1_star)
se2<-sd(theta2_star)
quantile_1<-quantile((theta1_star-theta1)/se1_star,c(0.025,0.975))
quantile_2<-quantile((theta2_star-theta2)/se2_star,c(0.025,0.975))

## -----------------------------------------------------------------------------
library(boot)
rm(list = ls())
set.seed(12345)
m<-1e4
x=c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)
boot.lambda<-function(x,i) mean(x[i])
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,1,2)
obj <- boot(data=x,statistic=boot.lambda, R = 999)
ci <- boot.ci(obj,type=c("norm","basic","perc","bca"))
ci.norm[1,]<-ci$norm[2:3];ci.basic[1,]<-ci$basic[4:5]
ci.perc[1,]<-ci$percent[4:5];ci.bca[1,]<-ci$bca[4:5]
result<-data.frame(method=c("norm","perc","basic","bca"),lower_bound=c(ci.norm[1],ci.perc[1],ci.basic[1],ci.bca[1]),
                   upper_bound=c(ci.norm[2],ci.perc[2],ci.basic[2],ci.bca[2]))
print(result)
d=density(x)
plot(d)
abline( v = mean(x), col = "blue")
text(mean(x),0.002, "mean(x)", col = "gray60", adj = c(0, -.1))

## -----------------------------------------------------------------------------
library(bootstrap)
data(scor)
data<-scor
n<-dim(scor)[1]
ev <- eigen(cov(data))
theta_hat<-ev$val[1]/sum(ev$val)
theta_jack<-numeric(n)
for(i in 1:n){
  data_star<-data[-i,]
  ev <- eigen(cov(data_star))
  theta_jack[i]<-ev$val[1]/sum(ev$val)
}
jack.bias<-(n-1)*(mean(theta_jack)-theta_hat)
jack.var<-(n-1)*var(theta_jack)*(n-1)/n
jack.se<-sqrt(jack.var)
round(c(original=theta_hat,bias.jack=jack.bias,
se.jack=jack.se),3)

## -----------------------------------------------------------------------------
library(DAAG); attach(ironslag)
n <- length(magnetic) #in DAAG ironslag
e1 <- e2 <- e3 <- e4 <- numeric(n*(n-1)/2)
# fit models on leave-two-out samples
i=1
for (k in 1:(n-1)) {
  for(j in (k+1):n){
    y <- magnetic[-c(k,j)]
    x <- chemical[-c(k,j)]
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[c(k,j)]
    e1[i] <- sum((magnetic[c(k,j)] - yhat1)*(magnetic[c(k,j)] - yhat1))/2
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[c(k,j)] +
    J2$coef[3] * chemical[c(k,j)]^2
    e2[i] <- sum((magnetic[c(k,j)] - yhat2)*(magnetic[c(k,j)] - yhat2))/2
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[c(k,j)]
    yhat3 <- exp(logyhat3)
    e3[i] <- sum((magnetic[c(k,j)] - yhat3)*(magnetic[c(k,j)] - yhat3))/2
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[c(k,j)])
    yhat4 <- exp(logyhat4)
    e4[i] <- sum((magnetic[c(k,j)] - yhat4)*(magnetic[c(k,j)] - yhat4))/2
    i=i+1
  }
}
c(mean(e1),mean(e2),mean(e3),mean(e4))

## -----------------------------------------------------------------------------
rm(list = ls())
set.seed(123)
attach(chickwts)
x <- sort(as.vector(weight[feed == "soybean"]))
y <- sort(as.vector(weight[feed == "linseed"]))
detach(chickwts)
R<-999
z<-c(x,y)
n<-length(x)
m<-length(y)
reps<-numeric(R)
F_n<-ecdf(x)
G_m<-ecdf(y)
t0<-m*n/(m+n)^2*(sum((F_n(x)-G_m(x))^2)+sum((F_n(y)-G_m(y))^2))
for(i in 1:R){
  xy<-sample(z)
  x1<-xy[1:n]
  y1<-xy[-(1:n)]
  F_n<-ecdf(x1)
  G_m<-ecdf(y1)
  reps[i]<-m*n/(m+n)^2*(sum((F_n(x1)-G_m(x1))^2)+sum((F_n(y1)-G_m(y1))^2))
}
p<-mean(c(t0,reps)>=t0)
print(p)

## -----------------------------------------------------------------------------
set.seed(123)
n1<-20
n2<-50
x<-rnorm(n1,mean=2,sd=1)
y<-rnorm(n2,mean=1,sd=2)
X<-x-mean(x)
Y<-y-mean(y)
R<-999
z<-c(X,Y)
reps<-numeric(R)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))
t0<-max(c(outx,outy))
for(i in 1:R){
  xy<-sample(z)
  x1<-xy[1:n1]
  y1<-xy[-(1:n1)]
  outx1 <- sum(x1 > max(y1)) + sum(x1 < min(y1))
  outy1 <- sum(y1 > max(x1)) + sum(y1 < min(x1))
  reps[i]<-max(c(outx1,outy1))
}
p<-mean(c(t0,reps)>=t0)
print(p)

## -----------------------------------------------------------------------------
rm(list = ls())
set.seed(123)
output.a<-function(N,b1,b2,b3,f0){
  x1<-rpois(N,lambda=1)
  x2<-rexp(N,rate=1)
  x3<-rbinom(N,1,0.5)
  g<-function(alpha){
    tmp<-exp(-alpha-b1*x1-b2*x2-b3*x3)
    p<-1/(1+tmp)
    mean(p)-f0
  }
  res<-uniroot(g,c(-20,20))[1]
}

## -----------------------------------------------------------------------------
res1<-output.a(10^6,0,1,-1,0.1)
res2<-output.a(10^6,0,1,-1,0.01)
res3<-output.a(10^6,0,1,-1,0.001)
res4<-output.a(10^6,0,1,-1,0.0001)

## -----------------------------------------------------------------------------
plot(x=c(res1,res2,res3,res4),y=-log(c(0.1,0.01,0.001,0.0001)),
     xlab="a",ylab="-logf0")

## -----------------------------------------------------------------------------
set.seed(12345)
dlaplace<-function(x) 1/2*exp(-abs(x))
rw.MH<-function(sigma,x0,N){
  x<-numeric(N)
  x1<-x0
  acc.rate<-0
  for(i in 2:N){
    y<-rnorm(1,x[i-1],sigma)
    u<-runif(1)
    if(u<=dlaplace(y)/dlaplace(x[i-1])){
      x[i]<-y
      acc.rate<-acc.rate+1
    }
    else
      x[i]<-x[i-1]
  }
  return(list(x=x,acc.rate=acc.rate/N))
}
N<-5000
sigma<-c(0.05,0.5,2,10)
x0<-10
rw1<-rw.MH(sigma[1],x0,N)
rw2<-rw.MH(sigma[2],x0,N)
rw3<-rw.MH(sigma[3],x0,N)
rw4<-rw.MH(sigma[4],x0,N)
rw<-cbind(rw1$x,rw2$x,rw3$x,rw4$x)
for(j in 1:4){
  plot(rw[,j],type="l",xlab=bquote(sigma==.(round(sigma[j],3))),
  ylab="x",ylim=range(rw[,j]))
}

## -----------------------------------------------------------------------------
hist(rw3$x,breaks =100,freq=F,xlab=bquote(sigma==2))
x=seq(-6,6,0.01)
lines(x,dlaplace(x))

## -----------------------------------------------------------------------------
set.seed(12345)
N<-5000
x<-matrix(0,N,2)
rho<-0.9
mu1<-mu2<-0
sigma1<-sigma2<-1
sigma1_new<-sqrt(1-rho^2)*sigma1
sigma2_new<-sqrt(1-rho^2)*sigma2
x[1,]<-c(10,10)
for(i in 2:N){
  x2<-x[i-1,2]
  mu1_new<-mu1+rho*sigma1/sigma2*(x2-mu2)
  x1<-x[i,1]<-rnorm(1,mu1_new,sigma1_new)
  mu2_new<-mu2+rho*sigma2/sigma1*(x1-mu1)
  x[i,2]<-rnorm(1,mu2_new,sigma2_new)
}
burn_in<-1000
res<-x[(burn_in+1):N,]
plot(res,xlab="X",ylab="Y",cex=0.4)

## -----------------------------------------------------------------------------
X<-res[,1]
Y<-res[,2]
lm.model<-lm(Y~X)
residuals <- residuals (lm.model)
a<-ppoints(100)
QR<-qnorm(a,mean=0,sd=sigma2_new) ##残差的理论分布
Q<-quantile(X,a)
qqplot(QR,Q,xlab="理论正态分布分位数",ylab="样本残差分位数")
plot(X,residuals)

## -----------------------------------------------------------------------------
set.seed(12345)
f <- function(x, sigma) {
  if (any(x < 0)) return (0)
  stopifnot(sigma > 0)
  return((x / sigma^2) * exp(-x^2 / (2*sigma^2)))
}
generate.chain<-function(m,x0,sigma){
  x <- numeric(m)
  x[1] <- x0
  u <- runif(m)
  for (i in 2:m) {
    xt <- x[i-1]
    y <- rchisq(1, df = xt)
    num <- f(y, sigma) * dchisq(xt, df = y)
    den <- f(xt, sigma) * dchisq(y, df = xt)
    if (u[i] <= num/den) x[i] <- y else {
      x[i] <- xt
    }
  }
  return(x)
}
Gelman.Rubin <- function(psi) {
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi) #row means
  B <- n * var(psi.means) #between variance est.
  psi.w <- apply(psi, 1, "var") #within variances
  W <- mean(psi.w) #within est.
  v.hat <- W*(n-1)/n + (B/n) #upper variance est.
  r.hat <- v.hat / W #G-R statistic
  return(r.hat)
}

m <- 10000
sigma <- 4
x0<-c(1,5,10,20)
X<-matrix(0,nrow=4,ncol=m)
for(i in 1:4){
  X[i,]<-generate.chain(m,x0[i],sigma)
}

psi<-t(apply(X,1,cumsum))
for (i in 1:nrow(psi)){
  psi[i,]<-psi[i,]/(1:ncol(psi))
}
print(Gelman.Rubin(psi))
rhat<-rep(0,m)
b<-1000
for(j in (b+1):m){
  rhat[j]<-Gelman.Rubin(psi[,1:j])
}
plot(rhat[(b+1):m],type="l",xlab="",ylab="R_hat")
abline(h=1.2,lty=2)

## -----------------------------------------------------------------------------
library(coda)
res=mcmc.list(mcmc(X[1,]),mcmc(X[2,]),mcmc(X[3,]),mcmc(X[4,]))
gelman.diag(res)
gelman.plot(res)

## -----------------------------------------------------------------------------
rm(list = ls())
u<-c(11,8,27,13,16,0,23,10,24,2)
v<-c(12,9,28,14,17,1,24,11,25,3)
f<-function(x){
  res=0
  for(i in 1:length(u)){
    res=res+(v[i]-u[i]*exp(-x*(u[i]-v[i])))/(exp(-x*(u[i])-v[i])-1)
  }
  return(res)
}

lambda_mle<-uniroot(f,c(0,1))$root  ###lambda的极大似然估计

g<-function(x1,x0){
  n/x1-n
}

EM<-function(max.it=10000,eps=1e-5){
  n<-length(v)
  lambda_0=1
  lambda_1=2
  while(abs(lambda_1-lambda_0)>eps){
    g<-function(x) n/x-n/lambda_0-f(x)
    lambda_0<-lambda_1
    lambda_1<-uniroot(g,c(0,1))$root
  }
  return(lambda_1)
}
lambda_em<-EM()  ###EM算法得到lambda的估计

## -----------------------------------------------------------------------------
set.seed(12345)
A <- matrix(c( 0,-2,-2,3,0,0,4,0,0,
              2,0,0,0,-3,-3,4,0,0,
              2,0,0,3,0,0,0,-4,-4,
              -3,0,-3,0,4,0,0,5,0,
              0,3,0,-4,0,-4,0,5,0,
              0,3,0,0,4,0,-5,0,-5,
              -4,-4,0,0,0,5,0,0,6,
              0,0,4,-5,-5,0,0,0,6,
              0,0,4,0,0,5,-6,-6,0), 9, 9)
B<-A+2

solve.game <- function(A) {
  #solve the two player zero-sum game by simplex method
  #optimize for player 1, then player 2
  #maximize v subject to ...
  #let x strategies 1:m, and put v as extra variable
  #A1, the <= constraints
  #
  min.A <- min(A)
  A <- A - min.A #so that v >= 0
  max.A <- max(A)
  A <- A / max(A)
  m <- nrow(A)
  n <- ncol(A)
  it <- n^3
  a <- c(rep(0, m), 1) #objective function
  A1 <- -cbind(t(A), rep(-1, n)) #constraints <=
  b1 <- rep(0, n)
  A3 <- t(as.matrix(c(rep(1, m), 0))) #constraints sum(x)=1
  b3 <- 1
  sx <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
  maxi=TRUE, n.iter=it)
  #the ’solution’ is [x1,x2,...,xm | value of game]
  #
  #minimize v subject to ...
  #let y strategies 1:n, with v as extra variable
  a <- c(rep(0, n), 1) #objective function
  A1 <- cbind(A, rep(-1, m)) #constraints <=
  b1 <- rep(0, m)
  A3 <- t(as.matrix(c(rep(1, n), 0))) #constraints sum(y)=1
  b3 <- 1
  sy <- simplex(a=a, A1=A1, b1=b1, A3=A3, b3=b3,
  maxi=FALSE, n.iter=it)
  soln <- list("A" = A * max.A + min.A,
  "x" = sx$soln[1:m],
  "y" = sy$soln[1:n],
  "v" = sx$soln[m+1] * max.A + min.A)
  soln
}

library(boot) #needed for simplex function
s <- solve.game(B)
round(cbind(s$x,s$y),7)
cat("The value of game A:",t(s$x)%*%A%*%s$y,"\n")
cat("The value of game B:",t(s$x)%*%B%*%s$y,"\n")

## -----------------------------------------------------------------------------
x <- list(list(1, 2), c(3, 4),c("a","b","c","d"),"e","f")
unlist(x)
as.vector(x)

## -----------------------------------------------------------------------------
a<-c(1,2,3,4,5,6,7,8,9)
dim(a)

b<-matrix(data=c(1:6),nrow=2,ncol=3)
cat(c(is.matrix(b),is.array(b)))

## -----------------------------------------------------------------------------
df<-data.frame(a=c(1,2,3),b=c("a","b","c"),c=c(T,T,F))
as.matrix(df)

df[-c(1,2,3),]  ##0行
df[,-c(1,2,3)]  ##0列

## -----------------------------------------------------------------------------
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
##对所有列使用scale01函数
df1<-data.frame(replicate(6,sample(c(1:10,NA),10,rep=T)))
sapply(df1,scale01)


##仅对数值列使用scale01函数
scale01_numeric <- function(x) {
  if(class(x)!="numeric") return(x)
  else return(scale01(x))
}
df2<-data.frame(a=c(1,2,3),b=c("a","b","c"),c=c(T,T,F),d=c(4,8,10))
sapply(df2,scale01_numeric)

## -----------------------------------------------------------------------------
df1<-data.frame(replicate(6,sample(c(1:10,NA),10,rep=T)))
my_sd<-function(x){
  funcs<-c(sd)
  sapply(funcs,function(f) f(x, na.rm = TRUE))
}
vapply(df1,my_sd,FUN.VALUE = c(sd=0))

df2<-data.frame(a=c(1,2,3),b=c("a","b","c"),c=c(T,T,F),d=c(6,8,10))
my_sd_numeric<-function(x){
  if(class(x)=="numeric"){
    funcs<-c(sd)
    return(sapply(funcs,function(f) f(x, na.rm = TRUE)))
  }
  else{
    return(0)
  }
}
vapply(df2,my_sd_numeric,FUN.VALUE = c(sd=0))

## -----------------------------------------------------------------------------
gen_r<-function(a=1,b=2,n=5000){
  res<-matrix(data=NA,nrow=n,ncol=2)
  res[1,]<-c(n/2,0.5)
  for(i in 2:n){
    y<-res[i-1,2]
    x<-rbinom(1,n,y)
    res[i,1]<-x
    y<-rbeta(1,x+a,n-x+b)
    res[i,2]<-y
  }
  return(res)
}

## ----eval=FALSE---------------------------------------------------------------
#  #include <Rcpp.h>
#  #include <iostream>
#  #include <random>
#  using namespace Rcpp;
#  using namespace std;
#  // [[Rcpp::export]]
#  NumericMatrix gen_Rcpp(double a=1,double b=2,int n=5000) {
#    NumericMatrix res(n,2);
#    NumericVector x(n);
#    NumericVector y(n);
#    x[0]=n/2;
#    y[0]=0.5;
#  	double ytemp=0;
#  	int xtemp=0;
#  	for(int i = 1; i < n; ++i) {
#  	ytemp=y[i-1];
#  	std::default_random_engine generator;
#  	std::binomial_distribution<int> distribution(n,ytemp);
#  	xtemp=distribution(generator);
#  	x[i]=xtemp;
#  	ytemp=std::beta(xtemp+a,n-xtemp+b);
#  	y[i]=ytemp;
#  	}
#  	res(_,0)=x;
#  	res(_,1)=y;
#  	return res;
#  }

## -----------------------------------------------------------------------------
library(Rcpp)
library(microbenchmark)
sourceCpp(paste0("C:/Users/Billy/Desktop/统计计算/作业/作业10/","gen_Rcpp.cpp"))
ts <- microbenchmark(r_time=gen_r(),rcpp_time=gen_Rcpp())
summary(ts)

