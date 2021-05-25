rm(list = ls())
graphics.off()

library(tsm)
library(vars)
library(mFilter)

dat_sa <- read.csv(file = "data_sa.csv")

gdp <- ts(log(dat_sa$gdp), start = c(1981, 2), freq = 4)
inf <- ts(dat_sa$inf, start = c(1981, 2), freq = 4)
int <- ts(dat_sa$int, start = c(1981, 2), freq = 4)

plot(cbind(gdp, inf, int))
#
gdp.hp <- hpfilter(gdp, freq = 1600)
gdp.gap <- gdp.hp$cycle
#
gdp.acf <- ac(gdp.gap, main = "GDP")
inf.acf <- ac(inf, main = "INF")
int.acf <- ac(int, main = "INT")
#
adf.gdp <- ur.df(gdp.gap, type = "trend", selectlags = "AIC")
summary(adf.gdp)
adf.inf <- ur.df(inf, type = "trend", selectlags = "AIC")
summary(adf.inf)
adf.int <- ur.df(int, type = "trend", selectlags = "AIC")
summary(adf.int)
#2.1
#
dat.bv <- cbind(int, inf)
colnames(dat.bv) <- c("int", "inf")

info.bv <- VARselect(dat.bv, lag.max = 12, type = "const")
info.bv$selection
#
bv.est <- VAR(dat.bv, p = 2, type = "const", season = 4, 
              exog = NULL)
summary(bv.est)
#
bv.est <- VAR(dat.bv, p = 2, type = "const", season = NULL, 
              exog = NULL)
summary(bv.est)
#
bv.serial <- serial.test(bv.est, lags.pt = 12, type = "PT.asymptotic")
bv.serial
plot(bv.serial, names = "int")
#
bv.arch <- arch.test(bv.est, lags.multi = 12, multivariate.only = TRUE)
bv.arch
#
bv.norm <- normality.test(bv.est, multivariate.only = TRUE)
bv.norm
#
resid <- residuals(bv.est)
par(mfrow = c(1, 1))
plot.ts(resid[, 1])
plot.ts(resid[60:80, 1])
resid[60:80, 1]
dum <- rep(0, dim(dat.bv)[1])
dum[68] <- 1
dum[69] <- 1
plot(dum)
bv.estdum <- VAR(dat.bv, p = 2, type = "const", season = NULL, 
                 exog = dum)
bv.serialdum <- serial.test(bv.estdum, lags.pt = 12, type = "PT.asymptotic")
plot(bv.serialdum)
bv.normdum <- normality.test(bv.estdum, multivariate.only = TRUE)
bv.normdum
#
bv.cusum <- stability(bv.est, type = "OLS-CUSUM")
plot(bv.cusum)
#
bv.cause.int <- causality(bv.est, cause = "int")
bv.cause.int
bv.cause.inf <- causality(bv.est, cause = "inf")
bv.cause.inf
#
irf.int <- irf(bv.est, response = "int", n.ahead = 40, boot = TRUE)
plot(irf.int)
#
irf.inf <- irf(bv.est, response = "inf", n.ahead = 40, boot = TRUE)
plot(irf.inf)
#
bv.vardec <- fevd(bv.est, n.ahead = 10)
plot(bv.vardec)


