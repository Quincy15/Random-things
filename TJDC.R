library(readr)
library(psych)
library(GPArotation)
tongjidiaocha <- read_csv("~/Codes/R/tongjidiaocha.csv")
View(tongjidiaocha)
cor <- cor(tongjidiaocha)
corrplot(cor, method = "circle")

#满意度对忠诚度建模
Model1 <- lm(y2 ~ y1, data = tongjidiaocha); summary(Model1)

#逻辑回归
tongjidiaocha$y3 <- tongjidiaocha$y1
tongjidiaocha$y3[tongjidiaocha$y1 > 3] <- 1
tongjidiaocha$y3[tongjidiaocha$y1 < 4] <- 0
tongjidiaocha$y3 <- factor(tongjidiaocha$y3, levels=c(0,1), labels=c("No","Yes"))
table(tongjidiaocha$y3)
ModelLog1 <- glm(y3 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14, data=tongjidiaocha, family=binomial())
summary(ModelLog1)

tongjidiaocha$y4 <- tongjidiaocha$y2
tongjidiaocha$y4[tongjidiaocha$y2 > 3] <- 1
tongjidiaocha$y4[tongjidiaocha$y2 < 4] <- 0
tongjidiaocha$y4 <- factor(tongjidiaocha$y4, levels=c(0,1), labels=c("No","Yes"))
table(tongjidiaocha$y4)
ModelLog2 <- glm(y4 ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14, data=tongjidiaocha, family=binomial())
summary(ModelLog2)

ModelLog1Step <- step(ModelLog1, direction = "both")
summary(ModelLog1Step)
ModelLog2Step <- step(ModelLog2, direction = "both")
summary(ModelLog2Step)

#EFA
x <- tongjidiaocha[,-c(15:18)]
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
library(semTools)
library(semPlot)
model <- ' F1 =~ x1 + x2 + x3 + x8 + x11 + x14
           F2 =~ x4 + x5 + x10 + x12 + x13
           F3 =~ x6 + x7 + x9 ' #算mi值得话要从F1里删掉x8
fit <- cfa(model, data=tongjidiaocha)
mi <- modindices(fit)
mi[mi$op == "=~",]
summary(fit, fit.measures=TRUE) #给出模型结果
reliability(fit)
fitmeasures(fit,fit.measures="all",baseline.modle=null)
semPaths(fit,what="std", style="lisrel",nCharNodes=0,nCharEdges=0, title=TRUE, layout="spring",edg.lable.cex=1,fade=FALSE) 
parameterEstimates(fit) #参数估计
standardizedSolution(fit) #标准化参数估计

#用未标准化的估计值
attach(tongjidiaocha)
tongjidiaocha$f1 <- (x1 + 1.327*x2 + 1.331*x3 + 1.303*x8 + 0.753*x11 + 1.312*x14)/6
tongjidiaocha$f2 <- (x4 + 1.185*x5 + 1.106*x10 + 1.439*x12 + 1.312*x13)/5
tongjidiaocha$f3 <- (x6 + 1.459*x7 + 1.473*x9)/3
detach(tongjidiaocha)
#标准化的估计值
attach(tongjidiaocha)
tongjidiaocha$f1 <- 0.541*x1 + 0.740*x2 + 0.693*x3 + 0.709*x8 + 0.478*x11 + 0.750*x14
tongjidiaocha$f2 <- 0.505*x4 + 0.581*x5 + 0.556*x10 + 0.75*x12 + 0.69*x13
tongjidiaocha$f3 <- 0.559*x6 + 0.754*x7 + 0.750*x9
detach(tongjidiaocha)

#再做逻辑回归
ModelLog3 <- glm(y3 ~ f1+f2+f3, data=tongjidiaocha, family=binomial())
summary(ModelLog3)
ModelLog4 <- glm(y4 ~ f1+f2+f3, data=tongjidiaocha, family=binomial())
summary(ModelLog4)
#逻辑回归和因子分析得结果都有用，因子分析给出三个大类的重要性，逻辑回归给出各细致变量的具体作用。