rm(list = ls())
library(ggplot2)
setwd("E:/��һ��ѧ�ڿγ�/ͳ������/homework")
data=read.csv("data.csv",header = T,encoding = "UTF-8",nrows = 10000)
data=data[c(-1,-2,-5,-7,-11,-14,-19,-20,-21,-23,-24,-25,-26,-89)]
data=data[-16:-41]
#������н�ʵĴ���
data$Wage=sub("�",'',data$Wage)
data$Wage=sub("K",'000',data$Wage)
data$Wage=as.numeric(data$Wage)

#���������۵Ĵ���
data$Value=sub("�",'',data$Value)
#��λ��M��K�����ܼ򵥵��޳����ó��԰������ǧ
for (i in 1:dim(data)[1]){
  if (grepl(pattern = "M",x=data[i,7])==T){
    data[i,7]=sub("M","",data[i,7])
    data[i,7]=as.numeric(data[i,7])*1000000
  }
  if (grepl(pattern = "K",x=data[i,7])==T){
    data[i,7]=sub("K","",data[i,7])
    data[i,7]=as.numeric(data[i,7])*1000
  }
}
data$Value=as.numeric(data$Value)
#���������صĴ���
data$Weight=sub("lbs",'',data$Weight)
data$Weight=as.numeric(data$Weight) 
data$Weight=data$Weight*0.4536

#���������ߵĴ���:Ӣ��Ӣ�绯Ϊ����
data$chi=0
data$cun=0
for (i in 1:dim(data)[1]){
  data[i,50]=as.numeric(strsplit(data[i,14],'\'')[[1]][1])
  data[i,51]=as.numeric(strsplit(data[i,14],'\'')[[1]][2])
  data[i,14]=data[i,50]*30.48+data[i,51]*2.54
}
data=data[c(-50,-51)]
data$Height=as.numeric(data$Height)

#�������ýŵĴ���
for (i in 1:dim(data)[1]){
  if (data[i,9] =="Right"){
    data[i,9]=1
  }
  else{
    data[i,9]=0
  }
}
data$Preferred.Foot=as.numeric(data$Preferred.Foot)


#������Ա������ȡ������
data1=data[which(data$Position=="GK"),]
#data=data[which(data$Position!="GK"),]

#����������λ�õĴ���:�����ͻ�Ϊ1�������ͻ�Ϊ0
for (i in 1:dim(data)[1]){
 if (data[i,13] %in%  c('CAM','CF','CM','LAM','LCM','LF','LM','LS','LW','RAM','RCM','RF','RM','RS','RW','ST')){
   data[i,13]=1
 }
  else{
    data[i,13]=0
  }
}
data$Position=as.numeric(data$Position)
data=data[-45:-49]  #�����Ž�λ�õ���Ա��Ӧ����ȥ��
data1=data1[c(-16,-17,-18,-22,-35)]   #���Ž�λ����Ա�о�û���õ������õ�


#---------------------------------------------------------------
#������һЩ�򵥵Ŀ��ӻ���������ġ�
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


#---------------------------------------------
#�Զ�����������ȡ���ɷ�
for (i in c(12,16,17,18,19,20,21,22,23,24,25,31,35,37,38,39,40,42,43,44)){
  data[,i]=scale(data[,i])
}

score_pca=princomp(data[,c(12,16,17,18,19,20,21,22,23,24,25,31,35,37,38,39,40,42,43,44)],cor=T)
summary(score_pca,loadings = T)

#������������ȡ���ɷ�
for (i in c(26,27,28,29,30,32,33,34)){
  data[,i]=scale(data[,i])
}

score_pca1=princomp(data[,c(26,27,28,29,30,32,33,34)],cor = T)
summary(score_pca1,loadings = T)

#���������׼���ı�����׼��һ��
for (i in c(11,36,41)){
  data[,i]=scale(data[,i])
}

#��ȥ���������õı���
#data=data[c(-4,-5,-10)]




