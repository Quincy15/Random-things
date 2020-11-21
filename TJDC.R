library(readr)
library(psych)
library(GPArotation)
tongjidiaocha <- read_csv("Codes/R/tongjidiaocha.csv")
View(tongjidiaocha)
cor(tongjidiaocha)
tongjidiaocha[,c(15:17)]
x <- tongjidiaocha[,-c(15:17)]
y1 <- tongjidiaocha$y1
y2 <- tongjidiaocha$y2
y3 <- tongjidiaocha$y3
Model0 <- lm(y1~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14, data = tongjidiaocha);summary(Model0)
Model1 <- lm(y2~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14, data = tongjidiaocha);summary(Model1)
Model2 <- lm(y3~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14, data = tongjidiaocha);summary(Model2)

#逻辑回归
tongjidiaocha$y4 <- tongjidiaocha$y1
tongjidiaocha$y4[tongjidiaocha$y1 > 3] <- 1
tongjidiaocha$y4[tongjidiaocha$y1 < 4] <- 0
tongjidiaocha$y4 <- factor(tongjidiaocha$y4, levels=c(0,1), labels=c("No","Yes"))
table(tongjidiaocha$y4)
ModelLog1 <- glm(y4 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14, data=tongjidiaocha, family=binomial())
summary(ModelLog1)

tongjidiaocha$y5 <- tongjidiaocha$y2
tongjidiaocha$y5[tongjidiaocha$y2 > 3] <- 1
tongjidiaocha$y5[tongjidiaocha$y2 < 4] <- 0
tongjidiaocha$y5 <- factor(tongjidiaocha$y5, levels=c(0,1), labels=c("No","Yes"))
table(tongjidiaocha$y5)
ModelLog2 <- glm(y5 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14, data=tongjidiaocha, family=binomial())
summary(ModelLog2)
ModelLog1Step <- step(ModelLog1, direction = "both")
summary(ModelLog1Step)
ModelLog2Step <- step(ModelLog2, direction = "both")
summary(ModelLog2Step)

#EFA
covariances <- cov(x)
correlations <- cov2cor(covariances)
fa.parallel(correlations, fa="both", n.iter=100)
fa1 <- fa(correlations, nfactors=2, rotate="varimax", fm="pa");fa1
factor.plot(fa1, labels=rownames(fa1$loadings))
fa.diagram(fa1, simple=FALSE)

fa2 <- fa(correlations, nfactors=3, rotate="varimax", fm="pa");fa2
factor.plot(fa2, labels=rownames(fa2$loadings))
fa.diagram(fa2, simple=FALSE)

fa3 <- fa(correlations, nfactors=4, rotate="varimax", fm="pa");fa3
factor.plot(fa3, labels=rownames(fa3$loadings))
fa.diagram(fa3, simple=FALSE)

#CFA
library(lavaan)
model <- ' F1 =~ x1 + x2 + x3 + x8 + x11 + x14
           F2 =~ x4 + x5 + x10 + x12 + x13
           F3 =~ x6 + x7 + x9 '
fit <- cfa(model, data=tongjidiaocha)
summary(fit, fit.measures=TRUE)
reliability(fit)
fitmeasures(fit,fit.measures="all",baseline.modle=null)
semPaths(fit,what="std", style="lisrel",nCharNodes=0,nCharEdges=0, title=TRUE, layout="spring",edg.lable.cex=1,fade=FALSE) 
