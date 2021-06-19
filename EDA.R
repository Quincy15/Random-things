rm(list = ls())
graphics.off()

library(zoo)
library(vars)
library(tseries)
library(readr)
library(plm)
library(earth)
library(urca)
library(caret)
library(corrplot)
library(psych)
library(ggplot2)
library(lmtest)
data_raw <- read.csv("eda_data_raw.csv");summary(data_raw)
X_standard <- apply(data_raw[,-c(1,14:17)], 2, scale)
data_standard <- cbind(X_standard, data_raw[,c(14,16)])

#COR,Series
corrplot(corr =cor(data_standard[,-2]),order="AOE",type="upper",tl.pos="tp")
corrplot(corr = cor(data_standard[,-2]),add=TRUE, type="lower", method="number",
         order="AOE", col="black",diag=FALSE,tl.pos="n", cl.pos="n")
----------------------------------------------------------------------------
#EFA
#1. KMO&Bartlett test, KMO should be close to 1, bartlett should be significant
cor <- cor(X_standard[,-c(2,3)])
cortest.bartlett(cor)#excluding BPI
KMO(cor)
#2. choose the number of factors
fa.parallel((cor(X_standard[,-c(2,3)])), fa="both", n.iter=100,
            main="因子提取数")
#3. using promax to get the factors
fa.promax <- fa(cor(X_standard[,-c(2,3)]), nfactors=2, rotate="promax", fm="pa");fa.promax
##get the loadings
fsm <- function(oblique) {
  if (class(oblique)[2]=="fa" & is.null(oblique$Phi)) {
    warning("Object doesn't look like oblique EFA")
  } else {
    P <- unclass(oblique$loading)
    F <- P %*% oblique$Phi
    colnames(F) <- c("PA1", "PA2")
    return(F)
  }
}
fsm(fa.promax)# the real corr between factors and original data
fa.promax$weight# the parameters
##visualize the loading
factor.plot(fa.promax, labels=rownames(fa.promax$loadings), title = "因子载荷图")
##calculate the factor
w <- fa.promax$weights
fa <- X_standard[,-c(2,3)]%*%w
--------------------------------------------------------------------------
#VAR
#adftest
adf.test(diff(data_raw$BPI))
adf.test(diff(data_raw$BLR_Dslogit))
adf.test(data_raw$BLR_Gslogit)
#
d_BPI <- diff(data_raw$BPI)
d_BLR <- diff(data_raw$BLR_Dslogit)
fa1 <- fa[-1,]
data_ds <- cbind(fa1, d_BPI, d_BLR)
#VAR model
VARselect(data_ds, lag.max = 5, type = "both")[["selection"]]
var_ds <- VAR(data_ds, ic="AIC", lag.max=5)
summary(var_ds)
#test
serial.test(var_ds, type = "PT.asymptotic")
x2y <- causality(var_ds, cause = "d_BPI");x2y$Granger
y2x <- causality(var_ds, cause = "d_BLR");y2x$Granger
#impulse response
var.irf1<-irf(var_ds, impulse = "d_BPI", response = "d_BLR", n.head=50)
plot(var.irf1)
#
fevd(var_ds, n.ahead = 15)
#predict
pre <- predict(var_ds, n.ahead = 4, ci = 0.95)
plot(pre)
