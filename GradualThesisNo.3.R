#作图包
library(ggplot2)
library(plyr)
library(dplyr)
library(maptools)
library(geojsonio)
library(ggthemes)
library(sf)
library(rgdal)
library(mapdata)
library(scales)
#yuegangao district normal map
china_map<-readShapePoly("bou2_4p.shp")
x <- china_map@data          #读取行政信息
xs <- data.frame(x,id=seq(0:924)-1)          #含岛屿共925个形状
?iconv
library(ggplot2)
china_map1 <- fortify(china_map)           #转化为数据框
library(plyr)
china_map_data <- join(china_map1, xs, type = "full")
iconv(china_map_data$NAME,from = "GBK")
table(iconv(china_map_data$NAME,from = "GBK"))

#s1s2s3地图分布图 广东
mydat6.1=readShapePoly("CHN_adm2.shp")
CHN_adm6.1<-fortify(mydat6.1)
xCHN<-mydat6.1@data
xCHN2<-(plyr::rename(xCHN,c(NAME_2="prefecture")))
xs<-data.frame(xCHN,id=seq(1:344)-1)
china_map_data<-join(CHN_adm6.1,xs,type="full")
guangdong<-subset(china_map_data,NAME_1=="Guangdong")
#2005
data2005_1<-data2005[,c(1,2,11,15,16,20,21)]
data2005_2<-(plyr::rename(data2005_1,c(prefecture="NAME_2")))
GD2005<-join(guangdong,data2005_2,by="NAME_2",type="left") 
s1<-as.vector(replace(GD2005$s1,which(is.na(GD2005$s1)),32.47)) 
s2<-as.vector(replace(GD2005$s2_per,which(is.na(GD2005$s2_per)),2.888889))
s3<-as.vector(replace(GD2005$s3,which(is.na(GD2005$s3)),79.30)) 
d1<-as.vector(replace(GD2005$d1,which(is.na(GD2005$d1)),54.48776)) 
d2<-as.vector(replace(GD2005$d2,which(is.na(GD2005$d2)),85.73877)) 
GD2005_1<-GD2005[,-c(22:27)]
GD2005_2<-data.frame(cbind(GD2005_1,s1,s2,s3,d1,d2))
#2009
data2009_1<-data2009[,c(1,2,11,15,16,20,21)]
data2009_2<-(plyr::rename(data2009_1,c(prefecture="NAME_2")))
GD2009<-join(guangdong,data2009_2,by="NAME_2",type="left") 
s1<-as.vector(replace(GD2009$s1,which(is.na(GD2009$s1)),40.50)) 
s2<-as.vector(replace(GD2009$s2_per,which(is.na(GD2009$s2_per)),7.734807))
s3<-as.vector(replace(GD2009$s3,which(is.na(GD2009$s3)),86.86)) 
d1<-as.vector(replace(GD2009$d1,which(is.na(GD2009$d1)),56.22456)) 
d2<-as.vector(replace(GD2009$d2,which(is.na(GD2009$d2)),96.1495)) 
GD2009_1<-GD2009[,-c(22:27)]
GD2009_2<-data.frame(cbind(GD2009_1,s1,s2,s3,d1,d2))
#2013
data2013_1<-data2013[,c(1,2,11,15,16,20,21)]
data2013_2<-(plyr::rename(data2013_1,c(prefecture="NAME_2")))
GD2013<-join(guangdong,data2013_2,by="NAME_2",type="left") 
s1<-as.vector(replace(GD2013$s1,which(is.na(GD2013$s1)),40.65)) 
s2<-as.vector(replace(GD2013$s2_per,which(is.na(GD2013$s2_per)),8.839779))
s3<-as.vector(replace(GD2013$s3,which(is.na(GD2013$s3)),87.79)) 
d1<-as.vector(replace(GD2013$d1,which(is.na(GD2013$d1)),56.17594)) 
d2<-as.vector(replace(GD2013$d2,which(is.na(GD2013$d2)),97.14756)) 
GD2013_1<-GD2013[,-c(22:27)]
GD2013_2<-data.frame(cbind(GD2013_1,s1,s2,s3,d1,d2))
#2017
data2017_1<-data2017[,c(1,2,11,15,16,20,21)]
data2017_2<-(plyr::rename(data2017_1,c(prefecture="NAME_2")))
GD2017<-join(guangdong,data2017_2,by="NAME_2",type="left") 
s1<-as.vector(replace(GD2017$s1,which(is.na(GD2017$s1)),43.94)) 
s2<-as.vector(replace(GD2017$s2_per,which(is.na(GD2017$s2_per)),59.197991))
s3<-as.vector(replace(GD2017$s3,which(is.na(GD2017$s3)),89.17)) 
d1<-as.vector(replace(GD2017$d1,which(is.na(GD2017$d1)),32.54167)) 
d2<-as.vector(replace(GD2017$d2,which(is.na(GD2017$d2)),115.69967)) 
GD2017_1<-GD2017[,-c(22:27)]
GD2017_2<-data.frame(cbind(GD2017_1,s1,s2,s3,d1,d2))

ggplot(GD2005_2, aes(x = long, y = lat, group = group,fill=d2)) +
  geom_polygon(colour="grey40") +
  scale_fill_gradient(low="white",high="brown") +
  coord_map("polyconic") +
  ggtitle("d2-2005") +
  theme(              
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggplot(GD2009_2, aes(x = long, y = lat, group = group,fill=d2)) +
  geom_polygon(colour="grey40") +
  scale_fill_gradient(low="white",high="brown") +
  coord_map("polyconic") +
  ggtitle("d2-2009") +
  theme(              
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggplot(GD2013_2, aes(x = long, y = lat, group = group,fill=d2)) +
  geom_polygon(colour="grey40") +
  scale_fill_gradient(low="white",high="brown") +
  coord_map("polyconic") +
  ggtitle("d2-2013") +
  theme(              
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

ggplot(GD2017_2, aes(x = long, y = lat, group = group,fill=d2)) +
  geom_polygon(colour="grey40") +
  scale_fill_gradient(low="white",high="brown") +
  coord_map("polyconic") +
  ggtitle("d2-2017") +
  theme(              
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
