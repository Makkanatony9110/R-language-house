library(maptools)
china_map=readShapePoly("bou2_4p.shp")
plot(china_map)
library(ggplot2)
ggplot(china_map,aes(x=long,y=lat,group=group)) +
  geom_polygon(fill="white",colour="grey") +
  coord_map("polyconic")
x<-china_map@data
xs<-data.frame(x,id=seq(0:924)-1,fileEncoding="UTF-8")

