#the data for panel data analysiss
rm(list=ls())
library(readr)
data1 <- read.csv("Desktop/gradual thesis 毕业论文/gradualthesisdata/data1.csv",header = T,as.is = T)
View(data1)
head(data1)
dim(data1)
#缺失值处理
getwd()
sum(is.na(data1))
library(VIM)
library(mice)
is.na(data1)
complete.cases(data1)
#mice包中的md.pattern()函数可生成一个以矩阵或数据框形式展示缺失值模式的表格
md.pattern(data1)
library("VIM")
aggr(data1,prop=TRUE,numbers=TRUE)
aggr(data1,prop=FALSE,numbers=TRUE)
matrixplot(data1)
colnames(data1)
#在图形边界展示两个变量的缺失值信息
marginplot(data1[c("s1","DevAdminiLand")],pch = c(20),col = c("darkgray","red","blue"))
#行删除方法
newdata<-data1[complete.cases(data1),]

#制作比较完整的数据框
data2<-data1[-c(358:361,375:391),-c(6:9)]
is.na(data2)
sum(is.na(data2))
#mice包中的md.pattern()函数可生成一个以矩阵或数据框形式展示缺失值模式的表格
md.pattern(data2)
library("VIM")
aggr(data2,prop=TRUE,numbers=TRUE)
aggr(data2,prop=FALSE,numbers=TRUE)
matrixplot(data2)
colnames(data2)
#在图形边界展示两个变量的缺失值信息
marginplot(data2[c("s1","DevAdminiLand")],pch = c(20),col = c("darkgray","red","blue"))

data2$year=as.numeric(data2$year)
sub=which(is.na(data2$s1))
data3=data2[-sub,]
sum(is.na(data3))
library(VIM)
library(mice)
#mice包中的md.pattern()函数可生成一个以矩阵或数据框形式展示缺失值模式的表格
md.pattern(data3)
aggr(data3,prop=TRUE,numbers=TRUE)
aggr(data3,prop=FALSE,numbers=TRUE)
matrixplot(data3)
#data2.2=data2[sub,]
#model=lm(s1~TotalAdminiLand+s3)
#model
#data2.2$s1=predict(model,data2.2)
#data3=rbind(data2.1,data2.2)
#平均值算法
attach(data3)
NewOutput[is.na(NewOutput)]=mean(NewOutput,na.rm = T)
R.Dpersonnel[is.na(R.Dpersonnel)]=mean(R.Dpersonnel,na.rm = T)
R.Dexpenditure[is.na(R.Dexpenditure)]=mean(R.Dexpenditure,na.rm = T)
ForeignCapital[is.na(ForeignCapital)]=mean(ForeignCapital,na.rm = T)
DevAdminiLand[is.na(DevAdminiLand)]=mean(DevAdminiLand,na.rm = T)
data4=data.frame(year,prefecture,NewOutput,R.Dpersonnel,R.Dexpenditure,
                 ForeignCapital,border,coastal,gdp,revenue,
                 s1,DevAdminiLand,TotalAdminiLand,totalterritory,s3,guangzhou_b,shenzhen_b,gba)
#data4也可以在面板数据分析里能用到的的，但是可靠性不会那么高
detach(data3)
sum(is.na(data4))
#调整数字规模
attach(data4)
NewOutput=data4$NewOutput*0.00001
R.Dexpenditure=data4$R.Dexpenditure*0.00001
data5=data.frame(year,prefecture,NewOutput,R.Dpersonnel,R.Dexpenditure,
                 ForeignCapital,border,coastal,gdp,revenue,
                 s1,DevAdminiLand,TotalAdminiLand,totalterritory,s3,guangzhou_b,shenzhen_b,gba)

detach(data4)
#data6才是在面板数据分析里能用到的，但是与此同时，data6只不过是一个原版数据，还要一些加工
attach(data5)
#data6的一些加工 
FdiReveRate=data5$ForeignCapital/data5$revenue*100
FdiGdpRate=ForeignCapital/gdp*100
NewproRDrate=data5$NewOutput/data5$R.Dexpenditure*100
s1=s1*0.01
s3=s3*0.01
s2=DevAdminiLand/TotalAdminiLand
s2_per=s2*100
d2=sqrt(s1^2+s2^2+s3^2)*100
d1=sqrt(((s1-s2)^2+(s1-s3)^2+(s2-s3)^2)/3)*100
data6=data.frame(data5,s2_per,FdiReveRate,NewproRDrate,FdiGdpRate,d1,d2,guangzhou_b,shenzhen_b,gba)

range(data6$d1)
range(data6$d2)


summary(data6)
dim(data6)
head(data6)
library(car)
library(ggplot2)

#描述性统计
#条形图
ggplot(data6,aes(x = factor(prefecture),y = R.Dpersonnel,colour = prefecture,,fill=prefecture,group=NewOutput))+
  geom_bar(stat = "identity",width = 1.0,position = position_dodge(0.5)) + theme(axis.text.x = element_text(size = 8,vjust = 0.5, hjust = 0.5, angle = 90))

#折线图
ggplot(data = data6,
       mapping = aes(x = year, y = NewOutput,group=prefecture,colour=prefecture,fill=prefecture)) + 
  geom_area(position="fill",colour="black",size=0.5,alpha=0.8)
#散点图
ggplot(data6,aes(x=NewOutput,y=R.Dexpenditure)) + 
  geom_point()+ stat_smooth(method = "lm") + facet_wrap(~year,nrow=3)

ggplot(data6,aes(x=R.Dexpenditure,y=NewOutput,color=factor(prefecture),group=prefecture))+geom_point(size=0.5) +
  stat_smooth(method = "lm",se=FALSE) +facet_wrap(~prefecture ,nrow=3)+
  theme(axis.text.x = element_text(size = 8,vjust = 0.5, hjust = 0.5, angle = 90))
#相关性分析
colnames(data7)
install.packages("tidyverse")
library(tidyverse)
data7<-data.frame(mutate(data6,RDperMan = as.numeric(R.Dexpenditure/R.Dpersonnel)*10000))
data8<-data.frame(data7[,-c(1:2,9:10,12:13,17,20:21)])
colnames(data8)
summary(data6)
dim(data6)
colnames(data6)
#多重共线性分析
data9<-data.frame(data8[,-c(2,4,8)])
XX<-cor(data9)
kappa(XX,exact = TRUE)
eigen(XX)
View(data8)
is.numeric(data7$RDperMan)
cov(data8)
cor(data8)
cor(data8,method = "spearman")
LogNewOutput<-as.numeric(log(NewOutput))
y<-data8[c("NewOutput","NewproRDrate")]
x<-data8[c("coastal","border","s1","s2_per","s3", "ForeignCapital","FdiGdpRate","R.Dexpenditure")]
z<-data8[c("NewOutput","NewproRDrate","coastal","border","s1","s2_per","s3", "ForeignCapital","FdiGdpRate","R.Dexpenditure")]
cor(x,y)
cor.test(x,y)
library(psych)
corr.test(z,use = "complete")
?corr.test

install.packages("MSBVAR")
install.packages("corrplot")
library(corrplot) 
library(xts)
library(mice)
library(plm)
library(tseries)
#单位根检验 gradual thesis no.2
dimnames(data7)
tlist1<-xts(data6$NewOutput,as.Date(data6$year))
adf.test(tlist1)
library(lmtest)
attach(data2005)
ols<-lm(log(NewOutput) ~ForeignCapital + d1 + d2 + border + coastal,data=data2005)
summary(ols)


