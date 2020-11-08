#3.11
X3_11 <- X3_11[-c(1,12),-c(1,6:10)]
class(X3_11)
x1 <- unlist(X3_11[,2]);x2 <- unlist(X3_11[,3]);x3 <- unlist(X3_11[,4]);y <- unlist(X3_11[,1])
cor(X3_11)
Model311 <- lm(X3_11$`货运总量y(万吨)`~X3_11$`工业总产值x1(亿元)`+X3_11$`农业总产值x2(亿元)`+X3_11$`居民非商品支出x3(亿元)`);summary(Model311)
Model3111 <- lm(X3_11$`货运总量y(万吨)`~X3_11$`工业总产值x1(亿元)`+X3_11$`农业总产值x2(亿元)`);summary(Model3111)
confint(Model3111)#系数置信区间
stdzy1<-scale(X3_11,center=T,scale=T)#数据标准化
stdmodel1<-lm(X3_11$`货运总量y(万吨)`~X3_11$`工业总产值x1(亿元)`+X3_11$`农业总产值x2(亿元)`,data.frame(stdzy1))#标准化方程
stdmodel1
model <- lm(y~x1+x2, head = T)
x0 <- data.frame(x1=75,x2=42,x3=3.1)#新增数据
predict(Model3111,x0,interval="prediction",levels=0.95)#新数据置信区间预测
library(car)
durbinWatsonTest(model1)#DW检验
#3.12
X3_12 <- X3_12[-c(1,25),]
Model312 <- lm(X3_12$GDP~X3_12$第一产业增加值+X3_12$第二产业增加值);summary(Model312)

#4.9
#(1)
X4_9 <- X4_9[-c(54,55),]
Model49 <- lm(X4_9$Y~X4_9$X);summary(Model49)
plot(Model49)
#（2）异方差性检验
library(car)
ncvTest(Model49)
#(3)加权最小二乘
d<-seq(-2,2,0.5)
R<-c()
for (i in 1:length(d)) {
  lm <- lm(X4_9$Y~X4_9$X, weights=1/(X4_9$X^d[i]))
  k <- summary(lm)
  R[i]<-k$adj.r.squared
}
Model491 <- lm(X4_9$Y~X4_9$X, weights=1/(X4_9$X^d[which.max(R)]))
summary(Model491)

#(4)方差稳定变换
y1 <- X4_9$Y^0.5
Model492 <- lm(y1~X4_9$X);summary(Model492)

#4.13
Model413 <- lm(X4_13$y~X4_13$x);summary(Model413)
plot(Model413)
durbinWatsonTest(Model413)
#迭代法
r <- 1-0.5*0.644368
y  <- c();x <- c()
for(i in 1:length(X4_13$x)){
  x[i] <- X4_13$x[i]-r*X4_13$x[i-1]
  y[i] <- X4_13$y[i]-r*X4_13$y[i-1]
}
x <- x[-1];y <- y[-1]
Model413D <- lm(y~x);summary(Model413D)
#二步迭代
r1 <- 1-0.5*1.367556
y1  <- c();x1 <- c()
for(i in 1:length(x)){
  x1[i] <- x[i]-r1*x[i-1]
  y1[i] <- y[i]-r1*y[i-1]
}
x1 <- x1[-1];y1 <- y1[-1]
Model413D1 <- lm(y1~x1);summary(Model413D1)
durbinWatsonTest(Model413D1)
#差分
dy <- c();dx <- c()
for (i in 1:length(X4_13$x)){
  dy[i] <- X4_13$y[i]-X4_13$y[i-1]
  dx[i] <- X4_13$x[i]-X4_13$x[i-1]
}
dx <- dx[-1];dy <- dy[-1]
Model413C <- lm(dy~dx);summary(Model413C)
durbinWatsonTest(Model413C)
#4.14
X4_14 <- X4_14[-53,]
#自己把y转成数值型，强制类型转换会产生NA
y <- c(893.93,1091.27,1229.97,1045.85,997.24,1495.14,1200.56,747.24,866.43,603,343.52,472.1,171.79,135.79,925.9,1574.01,1405.33,971.27,1165.2,597.85,490.34,709.59,987.3,954.6,1216.89,1491.52,668.3,915.03,565.92,1267.98,930.24,379.38,500.74,83.65,982.94,722.28,1337.44,1150.51,1514.84,1442.08,767.64,1020.03,1067.49,1484.12,957.68,1344.91,1361.78,1424.69,1158.21,827.56,803.16,1447.46)
x1 <- X4_14$`周演出场次x1,`;x2 <- X4_14$`周点击率x2,`
Model414 <- lm(y~x1+x2);summary(Model414)
plot(Model414)
durbinWatsonTest(Model414)
#迭代
r <- 1-0.5*0.745264
ry  <- c();rx1 <- c();rx2 <-c()
for(i in 1:length(y)){
  rx1[i] <- x1[i]-r*x1[i-1]
  rx2[i] <- x2[i]-r*x2[i-1]
  ry[i] <- y[i]-r*y[i-1]
}
rx1 <- rx1[-1];rx2 <- rx2[-1];ry <- ry[-1]
Model414D <- lm(ry~rx1+rx2);summary(Model414D)
durbinWatsonTest(Model414D)
#差分
dy <- c();dx1 <- c();dx2 <- c()
for(i in 1:length(y)){
  dx1[i] <- x1[i]-x1[i-1]
  dx2[i] <- x2[i]-x2[i-1]
  dy[i] <- y[i]-y[i-1]
}
dx1 <- dx1[-1];dx2 <- dx2[-1];dy <- dy[-1]
Model414C <- lm(dy~dx1+dx2);summary(Model414C)
durbinWatsonTest(Model414C)
durbinWatsonTest(Model413C)