##########QUESTION 5##############

getwd()
library(forecast)
library(fpp2)
library(readxl)


mydata<-read_excel("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles/GDP_change_2.xlsx")

####Q.5A   Alpha=0.2#####
View(mydata)
GDPChangets<-ts(mydata[,2],start=1963,end=2016,frequency=1)
View(GDPChangets)
train<-window(GDPChangets,start=1963,end=2000)
View(train)
fcses<-ses(train,initial="simple",h=16, alpha=0.2)
summary(fcses)
accuracy(fcses,GDPChangets)

#### Q.5A   Alpha=0.5#####
GDPChangets<-ts(mydata[,2],start=1963,end=2016,frequency=1)
View(GDPChangets)
train<-window(GDPChangets,start=1963,end=2000)
View(train)
fcses<-ses(train,initial="simple",h=16, alpha=0.5)
summary(fcses)
accuracy(fcses,GDPChangets)


####Q.5A    Alpha=0.8#####
GDPChangets<-ts(mydata[,2],start=1963,end=2016,frequency=1)
View(GDPChangets)
train<-window(GDPChangets,start=1963,end=2000)
View(train)
fcses<-ses(train,initial="simple",h=16, alpha=0.8)
summary(fcses)
accuracy(fcses,GDPChangets)


####Q.5C   Re-run#####
GDPChangets<-ts(mydata[,2],start=1963,end=2016,frequency=1)
View(GDPChangets)
train<-window(GDPChangets,start=1963,end=2000)
View(train)
fcses<-ses(train,initial="simple",h=16)
summary(fcses)
accuracy(fcses,GDPChangets)

##### Q.5C  Alternative(Using Subset)######
getwd()
library(forecast)
library(fpp2)
library(readxl)

mydata<-read_excel("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles/GDP_change_2.xlsx")

GDPChangets<-ts(mydata[,2],start=1963,end=2016,frequency=1)
View(GDPChangets)
train<-subset(GDPChangets,end=38)
View(train)
fcses<-ses(train,initial="simple",h=16)
summary(fcses)
accuracy(fcses,GDPChangets)


########Q.6  LES#########
getwd()
library(fpp2)
library(forecast)
library(readxl)


mydata<-read_excel("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles/SA_oil_prices_2_adapted.xlsx")


View(mydata)
GDPChangets<-ts(mydata[,2],start=1978,end=2015,frequency=1)
View(GDPChangets)
train<-window(GDPChangets,start=1978,end=2007)
View(train)
fcholt<-holt(train,initial="simple",h=8)
summary(fcholt)
accuracy(fcholt,GDPChangets)
