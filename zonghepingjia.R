library(readr)
data <- read_csv("~/Codes/R/data for zp.csv")

#dividing the dataset
data <- as.matrix(data)
data <- apply(data, 2, as.numeric)
w <- as.vector(data[100,])
data <- data[-100,]
wdf1 <- as.numeric(w[2:5])
wdf2 <- as.numeric(w[6:11])
wst <- as.numeric(w[12:18])
wzh <- as.numeric(w[19:23])
defen1 <- data[,c(2:5)]
defen2 <- data[,c(6:11)]
shenti <- data[,c(12:18)]
zhugong <- data[,c(19:23)]

df1 <- defen1 %*% wdf1
df2 <- defen2 %*% wdf2
st <- shenti %*% wst
zg <- zhugong %*% wzh

zongping <- cbind(df1,df2,st,zg)
write.csv(zongping, file = "~/Codes/R/zongping.csv")
