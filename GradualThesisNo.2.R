rm(list=ls())

#趋势图
#Foreign Investment Flow
attach(data5)
ymax1<-max(log(ForeignCapital))
ymin1<-min(log(ForeignCapital))
ggplot(data5,mapping=aes(x=year,y=log(ForeignCapital),colour=prefecture)) + 
  geom_line(linetype="dashed") + geom_point() +
  ggtitle("Foreign Investment Flow Trends of 22 prefecture") +
  ylim(ymin1,ymax1) +
  theme(axis.text.x = element_text(size = 5,vjust = 0.5, hjust = 0.5, angle = 90)) 

ggplot(data5,aes(x=year,y=log(ForeignCapital)))+
  geom_line(aes(group=prefecture,colour=prefecture))+
  theme(axis.text.x = element_text(size = 5,vjust = 0.5, hjust = 0.5, angle = 0)) +
  facet_wrap(~prefecture)+
  xlab("year")+
  ylab("Foreign Investment Flow")+
  ggtitle("Foreign Investment Flow Trends of 22 prefecture")


ggplot(data5,aes(x = interaction(year,prefecture),y = log(ForeignCapital) ,fill = prefecture))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = prefecture))

#GDP
ymax1<-max(gdp)
ymin1<-min(gdp)
ggplot(data5,mapping=aes(x=year,y=gdp,colour=prefecture,group=prefecture)) + 
  geom_line(linetype="dashed") + geom_point() +
  ggtitle("GDP Trends of 22 prefecture") +
  ylim(ymin1,ymax1) +
  theme(axis.text.x = element_text(size = 5,vjust = 0.5, hjust = 0.5, angle = 90)) 

ggplot(data5,aes(x=year,y=gdp))+
  geom_line(aes(group=prefecture,colour=prefecture))+
  theme(axis.text.x = element_text(size = 5,vjust = 0.5, hjust = 0.5, angle = 0)) +
  facet_wrap(~prefecture)+
  xlab("year")+
  ylab("GDP")+
  ggtitle("GDP Trends of 22 prefecture")

#s1,s2,s3 3D scatter plot
library(scatterplot3d)
attach(mtcars)
rm(x)

#data2005
x1=seq(0,21)
y1=1+13*x1
data2005=data6[c(y1),]
s1=data2005$s1
s2=data2005$s2_per
s3=data2005$s3
library(scatterplot3d)
s3d2005<-scatterplot3d(s1,s2,s3,
                   xlim=c(0,100),ylim=c(0,100),zlim=c(0,100),
                   pch=16,type="h",color=rainbow(22),
                   main = "2005")
legend(s3d2005$xyz.convert(120,80,140),legend = data2005$prefecture,
       col=rainbow(22),pch=16,cex = 0.5,horiz = FALSE,xpd = TRUE)
lm<-lm(data=data2005,s3~s1+s2)
s3d2005$plane3d(lm)
#data2009
x2=seq(0,21)
y2=5+13*x2
data2009=data6[c(y2),]
s1=data2009$s1
s2=data2009$s2_per
s3=data2009$s3

s3d2009<-scatterplot3d(s1,s2,s3,
                   xlim=c(0,100),ylim=c(0,100),zlim=c(0,100),
                   pch=16,type="h",color=rainbow(22),
                   main = "2009")
legend(s3d2009$xyz.convert(120,80,140),legend = data2009$prefecture,
       col=rainbow(22),pch=16,cex = 0.5,horiz = FALSE,xpd = TRUE)
lm<-lm(data=data2009,s3~s1+s2)
s3d2009$plane3d(lm)
#data2013
x3=seq(0,21)
y3=9+13*x3
data2013=data6[c(y3),]
s1=data2013$s1
s2=data2013$s2_per
s3=data2013$s3

s3d2013<-scatterplot3d(s1,s2,s3,
                   xlim=c(0,100),ylim=c(0,100),zlim=c(0,100),
                   pch=16,type="h",color=rainbow(22),
                   main = "2013")
legend(s3d2013$xyz.convert(120,80,140),legend = data2013$prefecture,
       col=rainbow(22),pch=16,cex = 0.5,horiz = FALSE,xpd = TRUE)
lm<-lm(data=data2013,s3~s1+s2)
s3d2013$plane3d(lm)

x4=seq(1,22)
y4=13*x4
data2017=data6[c(y4),]
s3=data2017$s3

s3d2017<-scatterplot3d(s1,s2,s3,
                   xlim=c(0,100),ylim=c(0,100),zlim=c(0,100),
                   pch=16,type="h",color=rainbow(22),
                   main = "2017")
legend(s3d2017$xyz.convert(120,80,140),legend = data2017$prefecture,
       col=rainbow(22),pch=16,cex = 0.5,horiz = FALSE,xpd = TRUE)
lm<-lm(data=data2017,s3~s1+s2)
s3d2017$plane3d(lm)
#average value and trends in every 4years
library(gcookbook)
ggplot(data2005,aes(x=prefecture,y=data2005$s1)) + 
  theme(axis.text.x = element_text(size = 5,vjust = 0.5, hjust = 0.5, angle = 90)) + 
  geom_point()
#s1s2s3箱线图
library(ggplot2)
attach(data5)
s2=data6$s2_per

b1<-ggplot(data=data6, mapping=aes(x=factor(year), y=s1),fill=year) +
  ylim(c(0,100)) + geom_boxplot() + geom_point(aes(colour = factor(year)),size=0.8,shape=1)  + ylab("Population(Pi)") + xlab('Year') +
  theme(axis.text.x = element_text(size = 8,vjust = 0.5, hjust = 0.5, angle = 90)) + theme(legend.position='none')
 
b2<-ggplot(data=data6, mapping=aes(x=factor(year), y=s2),fill=year) + 
  ylim(c(0,100)) + geom_boxplot() + geom_point(aes(colour = factor(year)),size=0.8,shape=1) + ylab("Land(Li)") + xlab('Year') +
  theme(axis.text.x = element_text(size = 8,vjust = 0.5, hjust = 0.5, angle = 90)) + theme(legend.position='none')
 
b3<-ggplot(data=data6, mapping=aes(x=factor(year), y=s3),fill=year) + 
  ylim(c(0,100)) + geom_boxplot() + geom_point(aes(colour = factor(year)),size=0.8,shape=1)  + ylab("Industry(Ii)") + xlab('Year') +
  theme(axis.text.x = element_text(size = 8,vjust = 0.5, hjust = 0.5, angle = 90)) + theme(legend.position='none')
 
library(customLayout)
mylay<-lay_new(mat=matrix(1:3,nrow = 3))
plot_list<-list(b1,b2,b3)
lay_grid(plot_list,mylay)

#d1d2 correlation
ggplot(data6,aes(x=d1,y=d2)) + 
  geom_point()+ stat_smooth(method = "lm") + facet_wrap(~prefecture,nrow=3)

ggplot(data6,aes(x=d1,y=d2,color=factor(prefecture),group=prefecture))+geom_point(size=0.3) +
  stat_smooth(method = "lm",se=FALSE) #+ facet_wrap(~prefecture,nrow=4)
  
  


