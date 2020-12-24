rm(list = ls())
library(readr)
data <- read_csv("statistical software/gooddata.csv")
View(data)

#查看所有球员的能力值分布
library(ggplot2)
ggplot(data=data,aes(x=Overall))+
  geom_density()+
  labs(title = 'density of overall')

#查看英超big6的能力值差异
n=c('Manchester United','Manchester City','Chelsea','Tottenham Hotspur','Liverpool','Arsenal')
big6=data[which(data$Club %in% n),]
ggplot(data=big6,aes(x=Club,y=Overall))+
  geom_violin(fill="lightblue")+
  geom_boxplot(fill="lightgreen",width=.2)

#查看英超big6的薪资差异
ggplot(data = big6,aes(x=Club,y=Wage))+
  geom_violin(fill="lightgreen")+
  geom_boxplot(fill="lightblue")+
  labs(title = "distribution of wage in big6")

#绘制视野与能力值的散点图，并拟合平滑直线
ggplot(data=data,aes(x=Vision,y=Overall))+
  geom_point()+
  geom_smooth(method = "lm")