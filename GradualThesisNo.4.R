#在这里主要进行的都是利用地图数据可视化



#数据导入
rm(list = ls())
library(REmap)
library(readr)
library(readr)
data_tech <- read.csv("Desktop/gradual thesis 毕业论文/gradualthesisdata/data_tech.csv",header = T,as.is = T)
View(data_tech)
data_tech$province
rm(province)
#抽取过来的中国十四个城市石油产量均值可视化
T=tapply(data_tech[,3],data_tech$province,mean)#求平均值
value<-T
T
T1=tapply(A[,3],A$city,mean) #一块求三十个城市AQI平均值
T2=tapply(A[,3],A$city,max) #一块求三十个城市AQI最大值
T3=tapply(A[,3],A$city,min) #一块求三十个城市AQI最小值
T4=T2-T3

province<-c("安徽","北京","重庆","福建","甘肃","广东","广西",
            "贵州","海南","河北","黑龙江","河南","香港","湖北",
            "湖南","江苏","江西","吉林","辽宁","内蒙古","宁夏",
            "青海","陕西","山东","上海","山西","四川","天津",
            "新疆","西藏","云南","浙江")

Cdata<-data.frame(province,value) #制作数据框
remapC(Cdata,maptype = "China",color = 'red')#进行可视化

#抽取过来的中国十四个城市煤炭产量均值可视化
T=tapply(data_tech[,4],data_tech$province,mean)#求平均值
T
T1=tapply(A[,3],A$city,mean) #一块求三十个城市AQI平均值
T2=tapply(A[,3],A$city,max) #一块求三十个城市AQI最大值
T3=tapply(A[,3],A$city,min) #一块求三十个城市AQI最小值
T4=T2-T3
province<-c("安徽","北京","重庆","福建","甘肃","广东","广西",
            "贵州","海南","河北","黑龙江","河南","香港","湖北",
            "湖南","江苏","江西","吉林","辽宁","内蒙古","宁夏",
            "青海","陕西","山东","上海","山西","四川","天津",
            "新疆","西藏","云南","浙江")
value<-T1
Cdata<-data.frame(province,value) # 制作数据框
remapC(Cdata,maptype = "China",color = 'blue') #进行可视化

# aqi index mean value visualization
T2=tapply(data_tech[,5],data_tech$province,mean) #求平均值
T2
value<-T2
Cdata<-data.frame(province,value) #制作数据框
remapC(Cdata,maptype = "China",color = 'green')#进行可视化

