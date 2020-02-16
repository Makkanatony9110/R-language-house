rm(list=ls())
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


# highspeed rail line
d1<-readShapeLines("gis_osm_railways_free_1.shp")
d1_1<-fortify(d1)
d1_2<-d1@data
#map of guangdong
d2=readShapePoly("CHN_adm2.shp")
d2_1<-fortify(d2)
d2_2<-d2@data
xs_2<-data.frame(d2_2,id=seq(1:344)-1)
china_map_data_1<-join(d1_1,xs_2,by="id",type="full")
china_map_data_2<-join(d2_1,xs_2,by="id",type="full")
guangdong<-subset(china_map_data_1,NAME_1=="Guangdong")

plot(d2)


ggplot(china_map_data_2, aes(x = long, y = lat, group = group)) +
  geom_polygon(colour="grey40") +
  geom_path(china_map_data_1,aes(x = long,y = lat,group=group), color='blue')+
  coord_map("polyconic") +
  #ggtitle("d2-2005") +
  theme(              
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
    ) 
