rm(list = ls())
#Panel data Analysis
library(plm)
library(xts)
library(mice)
library(tseries)

#OLS model
ols<-lm(log(NewOutput) ~FdiGdpRate + d1 + d2 + border + coastal,data=data6_panel)
summary(ols)
?pdata.frame
#lag
LagFDI_1=lag.xts(data6$FdiGdpRate,k=1)
LagFDI_2=lag.xts(data6$FdiGdpRate,k=2)
LagRD_1=lag.xts(data6$R.Dexpenditure,k=1)
LagRD_2=lag.xts(data6$R.Dexpenditure,k=2)
#Fixed Model lag0
fixed1_time <- plm(log(NewOutput) ~ FdiGdpRate + log(R.Dexpenditure)+ s1 + s2_per+ s3 + border + coastal+factor(year),
              index = c("prefecture","year"),
              model="within",
              data=data6)
summary(fixed1_time)

fixed1 <- plm(log(NewOutput) ~ FdiGdpRate + log(R.Dexpenditure)+ s1 + s2_per+ s3 + border + coastal,
             index = c("prefecture","year"),
             model="within",
             data=data6)
plmtest(random2did,c("time"),type = "bp")
summary(fixed1)
fixef(fixed1)
summary(fixef(fixed1))
pwartest(fixed2,data=data7)
pbgtest(random2did,data=data7)

phtest()
#Fixed Model lag1
fixed2 <- plm(log(NewOutput) ~ FdiGdpRate + log(R.Dexpenditure) + LagFDI_1 + log(LagRD_1) + s1  + s2_per + s3 + border + coastal,
              index = c("prefecture","year"),
              model="within",
              data=data6)
summary(fixed2)
summary(fixef(fixed2))
summary(fixef(fixed2,type="dmean"))
summary(fixef(fixed2,type="dfirst"))

pFtest(fixed2,pool)
#Random Model

random1 <- plm(log(NewOutput) ~ FdiGdpRate + log(R.Dexpenditure)+ s1 + s2_per + s3 +border + coastal ,
              index = c("prefecture","year"),
              model="random",
              data=data6)
summary(random1)

random2 <- plm(log(NewOutput) ~ FdiGdpRate+log(R.Dexpenditure)+ LagFDI_1 +LagRD_1 + s1 + s2_per+ s3 +border + coastal,
              index = c("prefecture","year"),
              model="random",
              data=data6)
summary(random2)

#Fixed or Random,which is the better model?
phtest(fixed1,random1)
phtest(fixed1,random1did)

?plm
#Pooled Model
pool<-plm(log(NewOutput) ~ FdiGdpRate + log(R.Dexpenditure)+ s1 + s2_per + s3 + border + coastal, data = data7,
          index = c("prefecture","year"), model = "pooling")
summary(pool)

#DID model
attach(data6)
print(data6$year)
regi<-as.numeric(year>=2013)
print(regi)
data6regi<-cbind(data6,regi)
#regime change model
random1regi <- plm(log(NewOutput) ~ FdiGdpRate+log(R.Dexpenditure) + s1 + s2_per + s3 + border + coastal+regi,
               index = c("prefecture","year"),
               model="random",
               data=data6regi)
summary(random1regi)

random2regi <- plm(log(NewOutput) ~ FdiGdpRate+ log(R.Dexpenditure)+LagFDI_1+ LagRD_1 + s1 + s2_per+ s3  + border + coastal+regi,
                  index = c("prefecture","year"),
                  model="random",
                  data=data6regi)
summary(random2regi)

#2SLS
library(sem)

sls2fix<-plm(formula = log(NewOutput)~ s1 + s2_per+ s3 + FdiGdpRate+ border + coastal|s1 + s2_per+ s3 + FdiGdpRate+ border + coastal+log(R.Dexpenditure)+LagRD_1,
             data = data6,model='within',effect='time')
summary(sls2fix)
sls2random<-plm(formula = log(NewOutput)~ s1 + s2_per+ s3 + FdiGdpRate+ border + coastal|s1 + s2_per+ s3 + FdiGdpRate + border + coastal+log(R.Dexpenditure)+LagRD_1,
             data = data6,model='random',effect='time')
summary(sls2random)

summary(sls2)

#Guanzghou dummy simple plm
random1_guangzhou <- plm(log(NewOutput) ~ FdiGdpRate + log(R.Dexpenditure)+ s1 + s2_per + s3 +guangzhou_b + coastal ,
               index = c("prefecture","year"),
               model="random",
               data=data6)
summary(random1_guangzhou)

random2_guangzhou <- plm(log(NewOutput) ~ FdiGdpRate+log(R.Dexpenditure)+ LagFDI_1 +LagRD_1 + s1 + s2_per+ s3 +guangzhou_b + coastal,
               index = c("prefecture","year"),
               model="random",
               data=data6)
summary(random2_guangzhou)

#Guangzhou dummy 2sls random
sls2random_guangzhou<-plm(formula = log(NewOutput)~ s1 + s2_per+ s3 + FdiGdpRate+ guangzhou_b + coastal|s1 + s2_per+ s3 + FdiGdpRate + guangzhou_b + coastal+ log(R.Dexpenditure)+LagRD_1,
                data = data6,model='random',effect='time')
summary(sls2random_guangzhou)

#Shenzhen dummy simple plm
random1_shenzhen <- plm(log(NewOutput) ~ FdiGdpRate + log(R.Dexpenditure)+ s1 + s2_per + s3 +shenzhen_b + coastal ,
                         index = c("prefecture","year"),
                         model="random",
                         data=data6)
summary(random1_shenzhen)

random2_guangzhou <- plm(log(NewOutput) ~ FdiGdpRate+log(R.Dexpenditure)+ LagFDI_1 +LagRD_1 + s1 + s2_per+ s3 +shenzhen_b + coastal,
                         index = c("prefecture","year"),
                         model="random",
                         data=data6)
summary(random2_guangzhou)

#Shenzhen dummy 2sls random
sls2random_shenzhen<-plm(formula = log(NewOutput)~ s1 + s2_per+ s3 + FdiGdpRate+ shenzhen_b + coastal|s1 + s2_per+ s3 + FdiGdpRate + shenzhen_b + coastal+ log(R.Dexpenditure)+LagRD_1,
                          data = data6,model='random',effect='time')
summary(sls2random_shenzhen)

#GBA dummy simple plm
random1_gba <- plm(log(NewOutput) ~ FdiGdpRate + log(R.Dexpenditure)+ s1 + s2_per + s3 +gba + coastal ,
                        index = c("prefecture","year"),
                        model="random",
                        data=data6)
summary(random1_gba)

random2_gba <- plm(log(NewOutput) ~ FdiGdpRate+log(R.Dexpenditure)+ LagFDI_1 +LagRD_1 + s1 + s2_per+ s3 +gba + coastal,
                         index = c("prefecture","year"),
                         model="random",
                         data=data6)
summary(random2_gba)

#GBA dummy 2sls random
sls2random_gba<-plm(formula = log(NewOutput)~ s1 + s2_per+ s3 + FdiGdpRate+ gba + coastal|s1 + s2_per+ s3 + FdiGdpRate + gba + coastal+ log(R.Dexpenditure)+LagRD_1,
                         data = data6,model='random',effect='time')
summary(sls2random_gba)


###################
test1<-plm(log(R.Dexpenditure)~  + FdiGdpRate + border + coastal+regi, index = c("prefecture","year"),
        model="random",data = data6regi)
summary(test1)

test2<-plm(s3 ~ NewproRDrate + FdiGdpRate + border + coastal+regi, 
             index = c("prefecture","year"),
           model="random",data = data6regi)
summary(test2)

NewproRDrate
sls2vol1<-plm(formula = log(NewOutput)~ log(R.Dexpenditure)+FdiGdpRate+ border + coastal|log(R.Dexpenditure) + FdiGdpRate + border + coastal+R.Dpersonnel,
              index = c("prefecture","year"),data = data6regi,model='random')
summary(sls2vol1)

sls2vol2<-plm(formula = log(NewOutput)~ log(R.Dexpenditure)+FdiGdpRate+ border + coastal+regi|log(R.Dexpenditure) + FdiGdpRate + border + coastal+regi+R.Dpersonnel,
              index = c("prefecture","year"),data = data6regi,model='random')
summary(sls2vol2)

sls2vol3<-plm(formula = log(NewOutput)~ log(R.Dexpenditure)+FdiGdpRate+ border + coastal|log(R.Dexpenditure) + FdiGdpRate + border + coastal+s1+s2+s3,
              index = c("prefecture","year"),data = data6regi,model='random')
summary(sls2vol3)

sls2vol4<-plm(formula = log(NewOutput)~ log(R.Dexpenditure)+FdiGdpRate+ border + coastal+regi|log(R.Dexpenditure) + FdiGdpRate + border + coastal+regi+s1+s2+s3,
              index = c("prefecture","year"),data = data6regi,model='random')
summary(sls2vol4)

sls2vol5<-plm(formula = log(NewOutput)~ log(R.Dexpenditure)+FdiGdpRate+ border + coastal+s1+s2_per+s3+regi|log(R.Dexpenditure) + FdiGdpRate + border + coastal+s1+s2_per+s3+regi+R.Dpersonnel,
              index = c("prefecture","year"),data = data6regi,model='random')
summary(sls2vol5)


#自己相关性检验 
library(lmtest)
x1=seq(0,21)
y1=1+13*x1
data2005=data6[c(y1),]
ols2005<-lm(log(NewOutput) ~ForeignCapital + d1 + d2 + border + coastal,data=data2005)
summary(ols2005)
dwtest(ols2005)

x2=seq(0,21)
y2=5+13*x2
data2009=data6[c(y1),]
ols2009<-lm(log(NewOutput) ~ForeignCapital + d1 + d2 + border + coastal,data=data2009)
summary(ols2009)
dwtest(ols2009)

x3=seq(0,21)
y2=9+13*x3
data2013=data6[c(y1),]
ols2013<-lm(log(NewOutput) ~ForeignCapital + d1 + d2 + border + coastal,data=data2013)
summary(ols2013)
dwtest(ols2013)

x4=seq(1,22)
y4=13*x4
data2017=data6[c(y1),]
ols2017<-lm(log(NewOutput) ~ForeignCapital + d1 + d2 + border + coastal,data=data2017)
summary(ols2017)
dwtest(ols2017)

#random effect model is appropriate????????
plmtest(pool,type = c("bp"))

pcdtest(fixed1,test=c("lm"))
pcdtest(fixed1,test=c("cd"))

install.packages('revealjs')
