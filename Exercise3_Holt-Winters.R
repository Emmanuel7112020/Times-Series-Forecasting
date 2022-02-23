####### 2  #######

getwd()
setwd("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles")
library(forecast)
library(ggplot2)
library("readxl")
Plastic<-read_excel("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles/Plastics.xlsx")
View(Plastic)
is.ts(Plastic)
mydata<- Plastic[,2]
View(mydata)
is.ts(mydata)
myts<-ts(mydata,start=c(2013,1),frequency = 12)
View(myts)
plot(myts,xlab="Year",ylab="Sales",main="monthly sales of product A")
ggseasonplot(myts,xlab="Year",ylab="Sales",main="Season Plot of Sales of Product A"
             ,year.labels=TRUE,pch=19)
train<-window(myts,start=c(2013,1),end=c(2013,12))
used_data<-window(myts,start=c(2014,1),end=c(2017,12))
View(train)
fc_hw<-hw(used_data,seasonal="additive")
fc_hw
accuracy(used_data,myts)
ets_ts<-ets(myts)
summary(ets_ts)


####### 3E-ADDITIVE #######
getwd()
library(fpp3)
library(forecast)
library(ggplot2)
library("readxl")

View( Autos_index)
is.ts(Autos_index)
mydata<-  Autos_index[,2]
View(mydata)
is.ts(mydata)
myts<-ts(mydata,start=c(1991,1),frequency = 4)
View(myts)
plot(myts,xlab="Year",ylab="Sales",main="monthly sales of Autos")
ggseasonplot(myts,xlab="Year",ylab="Sales",main="Season Plot of Sales of Autos"
             ,year.labels=TRUE,pch=19)
train<-window(myts,start=c(1991,1),end=c(2009,4))
hold_out<-window(myts,start=c(2010,1),end=c(2015,4))
View(train)
fc_hw_additive<-hw(train,seasonal="additive",initial="simple",h=24)
summary(fc_hw_additive)
accuracy(fc_hw_additive,myts)
autoplot(fc_hw_additive)

####### 3E-MULTIPLICATIVE #######

fc_hw_multiplicative<-hw(train,seasonal="multiplicative",initial="simple",h=24)
summary(fc_hw_multiplicative)
accuracy(fc_hw_multiplicative,myts)
autoplot(fc_hw_multiplicative)