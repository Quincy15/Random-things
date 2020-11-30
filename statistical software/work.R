rm(list = ls())
library(ggplot2)
setwd("E:/��һ��ѧ�ڿγ�/ͳ������/homework")
data=read.csv("data.csv",header = T,encoding = "UTF-8",nrows = 10000)
data=data[-1]
data=data[-4]
data=data[-5]
data=data[-8]
data=data[-1]
data=data[-24:-49]
data$Wage=sub("�",'',data$Wage)
data$Wage=sub("K",'',data$Wage)
data$Wage=as.numeric(data$Wage)

data$Value=sub("�",'',data$Value)
data$Value=sub("M",'',data$Value)
data$Value=sub("K",'',data$Value)
data$Value=as.numeric(data$Value)

data$Weight=sub("lbs",'',data$Weight)
#data$Weight=as.numeric(data$weight) ����զ�����ز�


#�鿴������Ա������ֵ�ֲ�
ggplot(data=data,aes(x=Overall))+
  geom_density()+
  labs(title = 'density of overall')

#�鿴Ӣ��big6������ֵ����
n=c('Manchester United','Manchester City','Chelsea','Tottenham Hotspur','Liverpool','Arsenal')
big6=data[which(data$Club %in% n),]
ggplot(data=big6,aes(x=Club,y=Overall))+
  geom_violin(fill="lightblue")+
  geom_boxplot(fill="lightgreen",width=.2)

#�鿴Ӣ��big6��н�ʲ���
ggplot(data = big6,aes(x=Club,y=Wage))+
  geom_violin(fill="lightgreen")+
  geom_boxplot(fill="lightblue")+
  labs(title = "distribution of wage in big6")

#������Ұ������ֵ��ɢ��ͼ�������ƽ��ֱ��
ggplot(data=data,aes(x=Vision,y=Overall))+
  geom_point()+
  geom_smooth(method = "lm")