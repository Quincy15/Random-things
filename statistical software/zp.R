rm(list = ls())
library(ggplot2)
setwd("E:/研一上学期课程/统计软件/homework")
data=read.csv("data.csv",header = T,encoding = "UTF-8",nrows = 10000)
data=data[c(-1,-2,-5,-7,-11,-14,-19,-20,-21,-23,-24,-25,-26,-89)]
data=data[-16:-41]
#变量，薪资的处理
data$Wage=sub("�",'',data$Wage)
data$Wage=sub("K",'000',data$Wage)
data$Wage=as.numeric(data$Wage)

#变量，身价的处理
data$Value=sub("�",'',data$Value)
#单位有M、K，不能简单的剔除，得乘以百万或者千
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
#变量，体重的处理
data$Weight=sub("lbs",'',data$Weight)
data$Weight=as.numeric(data$Weight) 
data$Weight=data$Weight*0.4536

#变量，身高的处理:英尺英寸化为厘米
data$chi=0
data$cun=0
for (i in 1:dim(data)[1]){
  data[i,50]=as.numeric(strsplit(data[i,14],'\'')[[1]][1])
  data[i,51]=as.numeric(strsplit(data[i,14],'\'')[[1]][2])
  data[i,14]=data[i,50]*30.48+data[i,51]*2.54
}
data=data[c(-50,-51)]
data$Height=as.numeric(data$Height)

#变量惯用脚的处理
for (i in 1:dim(data)[1]){
  if (data[i,9] =="Right"){
    data[i,9]=1
  }
  else{
    data[i,9]=0
  }
}
data$Preferred.Foot=as.numeric(data$Preferred.Foot)


#将守门员单独提取出来？
data1=data[which(data$Position=="GK"),]
#data=data[which(data$Position!="GK"),]

#变量，场上位置的处理:攻击型化为1，防守型化为0
for (i in 1:dim(data)[1]){
 if (data[i,13] %in%  c('CAM','CF','CM','LAM','LCM','LF','LM','LS','LW','RAM','RCM','RF','RM','RS','RW','ST')){
   data[i,13]=1
 }
  else{
    data[i,13]=0
  }
}
data$Position=as.numeric(data$Position)
data=data[-45:-49]  #将非门将位置的球员相应属性去掉
data1=data1[c(-16,-17,-18,-22,-35)]   #将门将位置球员感觉没有用的属性拿掉


#---------------------------------------------------------------
#以下是一些简单的可视化，画着玩的。
#查看所有球员的能力值分布
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


#---------------------------------------------
#对动作技巧类提取主成分
for (i in c(12,16,17,18,19,20,21,22,23,24,25,31,35,37,38,39,40,42,43,44)){
  data[,i]=scale(data[,i])
}

score_pca=princomp(data[,c(12,16,17,18,19,20,21,22,23,24,25,31,35,37,38,39,40,42,43,44)],cor=T)
summary(score_pca,loadings = T)

#对身体素质提取主成分
for (i in c(26,27,28,29,30,32,33,34)){
  data[,i]=scale(data[,i])
}

score_pca1=princomp(data[,c(26,27,28,29,30,32,33,34)],cor = T)
summary(score_pca1,loadings = T)

#将其余需标准化的变量标准化一下
for (i in c(11,36,41)){
  data[,i]=scale(data[,i])
}

#再去掉几个无用的变量
#data=data[c(-4,-5,-10)]





