#E5.5
library(readr);E5_5 <- read_csv("E5.5.csv")
Model0 <- lm(y~x1+x2+x3+x4,data = E5_5);summary(Model0)
Model1 <- step(Model0)
Model2 <- lm(y~x1+x2+x4,data = E5_5);summary(Model2)
Model3 <- lm(y~x1+x2,data = E5_5);summary(Model3)
library(pls)
#偏最小二乘方法1
set.seed(1) #求最优的成分个数
pls1 <- plsr(y~x1+x2+x3+x4,data = E5_5, scale = T, validation = "CV")
summary(pls1)
plot(RMSEP(pls1),legendpos = "topright")
loading.weights(pls1)
#偏最小二乘方法2
y1 <- E5_5[5];x1 <- E5_5[1:4]
y <- scale(y);x <- scale(x)
pls2 <- plsr(y~x, validation = "LOO", jackknife = "TRUE")
summary(pls2)
plot(RMSEP(pls2),legendpos = "topright")
pls2 <- plsr(y~x, validation = "LOO", ncomp = 3, jackknife = "TRUE");summary(pls2)
loading.weights(pls2)
coef(pls2)

#7.6
library(readr);X7_6 <- read_csv("7.6.csv")
cor(X7_6)
Model0 <- lm(X5~X1+X2+X3+X4,data = X7_6);summary(Model0)
library(carData)
library(car)
vif(Model0)
Model1 <- step(Model0,direction = "backward")
Model2 <- step(Model0,direction = "both")
summary(Model2)
library(Matrix)
library(glmnet)
library(MASS)
ridge.sol <- lm.ridge(X5~X1+X2+X3+X4,lambda = seq(0, 150, length = 151), data = X7_6, model = TRUE)
ridge.sol$lambda[which.min(ridge.sol$GCV)]  #找到GCV最小时的lambdaGCV
ridge.sol$coef[which.min(ridge.sol$GCV)]  ##找到GCV最小时对应的系数
par(mfrow = c(1, 2))
# 画出图形，并作出lambdaGCV取最小值时的那条竖直线
matplot(ridge.sol$lambda, t(ridge.sol$coef), xlab = expression(lamdba), ylab = "Cofficients", type = "l", lty = 1:20)
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])
# 下面的语句绘出lambda同GCV之间关系的图形
plot(ridge.sol$lambda, ridge.sol$GCV, type = "l", xlab = expression(lambda), ylab = expression(beta))
abline(v = ridge.sol$lambda[which.min(ridge.sol$GCV)])
library(ridge)
mod <- linearRidge(X5~X1+X2+X3+X4, data = X7_6)
summary(mod)