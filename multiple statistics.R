library(readr)
ratings <- read_csv("~/Codes/R/2K20_ratings.csv")
View(ratings)

data <- apply(ratings, 2, as.numeric)
jingong <- data[,c(2:14)]
shenti <- data[,c(15:21)]
zuzhi <- data[,c(22:27)]
fangshou <- data[,c(28:37)]

library(corrplot)
par(mfrow=c(2,2),mar=c(4,4,2,1))
cor1 <- cor(jingong)
corrplot(cor1, method = "circle", main = "jingong")
cor2 <- cor(shenti)
corrplot(cor2, method = "circle")
cor3 <- cor(fangshou)
corrplot(cor3, method = "circle")
cor4 <- cor(zuzhi)
corrplot(cor4, method = "circle")

#factor analysis
library(psych)
cov1 <- cov(jingong)
corr1 <- cov2cor(cov1)
# fa="both"：同时展示主成分分析和因子分析的结果
fa.parallel(corr1, fa="both", n.iter=100,
            main="进攻指标因子提取数")
cov2 <- cov(shenti)
corr2 <- cov2cor(cov2)
fa.parallel(corr2, fa="both", n.iter=100,
            main="身体指标因子提取数")
cov3 <- cov(zuzhi)
corr3 <- cov2cor(cov3)
fa.parallel(corr3, fa="both", n.iter=100,
            main="组织指标因子提取数")
cov4 <- cov(fangshou)
corr4 <- cov2cor(cov4)
fa.parallel(corr4, fa="both", n.iter=100,
            main="防守指标因子提取数")
#jingong and fangshou need 2 factors;zuzhi needs only 1 factor;shenti needs 3 prinfs
fa1 <- fa(corr1, nfactors=2, rotate="varimax", fm="pa");fa1
set.seed(1)
pc2 <- princomp(scale(shenti));summary(pc2,loadings = TRUE)
fa3 <- fa(corr3, nfactors=1, rotate="varimax", fm="pa");fa3
fa4 <- fa(corr4, nfactors=2, rotate="varimax", fm="pa");fa4

#计算各个因子和主成分
#进攻
zh1 <- fa1$weights
jgyinzi <- jingong %*% zh1
#身体直接出结果
stzhuchengfen <- pc2$scores[,1:3]
#组织
zh3 <- fa3$weights
zzyinzi <- zuzhi %*% zh3
#防守
zh4 <- fa4$weights
fsyinzi <- fangshou %*% zh4
data = cbind(jgyinzi,stzhuchengfen,zzyinzi,fsyinzi)
write.csv(data, file = "~/Codes/R/duoyuan.csv")

#层次聚类
library(readr)
duoyuan <- read.csv("~/Codes/R/duoyuan.csv", encoding = "utf-8")
View(duoyuan)
mingchen <- duoyuan[,1]
dy <- scale(duoyuan[,-c(1,4:6)])
dy <- cbind(dy, duoyuan[,c(4:6)])
row.names(dy) <- mingchen
#欧氏距离法
hc<-hclust(dist(dy,method = "euclidean"),method = "ward.D2")
plot(hc,hang = -0.01,cex=0.7, main = "Cluster using Ward.D2")
#最短距离法
hc2=hclust(dist(dy),method = "single")
plot(hc2,hang = -0.01,cex=0.7, main = "Cluster using single")

#求解最佳聚类
library(NbClust)
devAskNewPage(ask=TRUE)
nc<-NbClust(dy,distance = "euclidean",min.nc = 2,max.nc = 15,method = "average")
table(nc$Best.nc[1,])
barplot(table(nc$Best.nc[1,]))#最优类数为3
clusters<-cutree(hc,k=3)
table(clusters)
plot(hc,cex=0.7,hang=-0.01)
rect.hclust(hc,k=5,boeder="red")

c1 <- dy[clusters==1,]
c2 <- dy[clusters==2,]
c3 <- dy[clusters==3,]
jieguo <- cbind(dy,clusters)
write.csv(jieguo, file = "~/Codes/R/duoyuan with cluster.csv")
