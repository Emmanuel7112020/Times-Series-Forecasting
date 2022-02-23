####1A####
#Weather forecast: Daily and weekly forecasts
#Stock forecasts: weekly
####1B####
#Predictability of an event depends on
.# how well we understand the factors that contribute to the event
.# how much data/experience are available
. #whether the forecasts can affect the thing we are trying to forecast

#For Electricity demand: seasons of the year could help predict demand as more heating, cooking as a result of reduced outdoor activities.
#For currency exchange: geopolitical issues, government policies, national issues may help predict the exchange rate between british pound and euro. For example, brexit . This type of event has no previous history. Hence, it may be difficult to 
#forecast properly or accurately.

####1C####
#Aggregation over a period of time can help deal with irregular spacing of data. Weekend data may be aggregated at monthly level and monthly sales can be aggregated quarterly.

###1D####
#i sales of gasoline:ratio-scaled, time series
#ii. Credit rating of individual consumers:  ordinal, cross-sectional(if multiple individuals are considered same period),Panel(if multiple individuals are considered over multiple periods)
##iii.  Gross Domestic Product: number/ratio-scaled,time series
#iv.  Unemployment :  ratio-scaled, time series
#v. Subsequent population census data: discrete, number, time series



##2A####
#yearly/monthly=interval; Temperature= interval,time series

####2B & 2C######
getwd()
setwd("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles")
library(forecast)
library(fpp2)
library(ggplot2)
library("readxl")
Boulder_2<-read_excel("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles/Boulder_2.xlsx")
View(Boulder_2)
is.ts(Boulder_2)
mydata<- Boulder_2[,3]
View(mydata)
is.ts(mydata)
myts<-ts(mydata,start=c(1991,1),frequency = 12)
View(myts)
plot(myts,xlab="Year",ylab="Temperature",main="Time Series Plot of Temperature")
last_fouryears<-window(myts, start = c(2012,1), end = c(2015,12))
ggseasonplot(last_fouryears,xlab="Year",ylab="Temperature",main="Time Series Plot of Temperature"
             ,year.labels=TRUE,year.labels.left=TRUE,pch=19)



#### 3A####
getwd()
setwd("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles")
library(forecast)
library(fpp2)
library(ggplot2)
library("readxl")
library(GGally)
Rail_safety<-read_excel("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles/Rail_safety.xlsx")
is.ts(Rail_safety)
View(Rail_safety)
time<- Rail_safety[,1]
injuries<- Rail_safety[,2]
train_miles<- Rail_safety[,3]
injuries_ts<-ts(injuries,start=1990,frequency=1)
train_miles_ts<-ts(train_miles,start=1990,frequency=1)
ts.plot(injuries_ts,xlab="Time",ylab="Injuries",main="Time Series Plot of Injuries",type="l")
ts.plot(train_miles_ts,xlab="Time",ylab="Train Miles",main="Time Series Plot of Train Miles",type="l")
plot(injuries_ts,xlab="Time",ylab="Injuries",main="Scatter Plot of Injuries",type="p")
plot(train_miles_ts,xlab="Time",ylab="Train Miles",main="Scatter Plot of Train Miles",type="p")

######3B####
ggpairs(Rail_safety[,2:3])

####3C#####
injuries_per_TM<-Rail_safety[,4]
injuries_per_TM_ts<-ts(injuries_per_TM,start=1990,frequency=1)
ts.plot(injuries_per_TM_ts,xlab="Time",ylab="Injuries per T-M",main="Time Series Plot of Adjusted Data",type="l")

#########6A####################

getwd()
setwd("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles")
library("readxl")
library(DescTools)
library(GGally)


Baseball<-read_excel("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles/Baseball.xlsx")

mean(Baseball[[1]])
median(Baseball[[1]])
max(Baseball[[1]])
min(Baseball[[1]])
range<-max(Baseball[[1]])-min(Baseball[[1]])
range
mad(Baseball[[1]],center = median(Baseball[[1]]))
##############Alternative Approach############
mad(Baseball[[1]])
sd(Baseball[[1]])


mean(Baseball[[2]])
median(Baseball[[2]])
max(Baseball[[2]])
min(Baseball[[2]])
range<-max(Baseball[[2]])-min(Baseball[[2]])
range
mad(Baseball[[2]],center = median(Baseball[[2]]))
##############Alternative Approach############
mad(Baseball[[2]])
sd(Baseball[[2]])


mean(Baseball[[3]])
median(Baseball[[3]])
max(Baseball[[3]])
min(Baseball[[3]])
range<-max(Baseball[[3]])-min(Baseball[[3]])
range
mad(Baseball[[3]],center = median(Baseball[[3]]))
##############Alternative Approach############
mad(Baseball[[3]])
sd(Baseball[[3]])

mean(Baseball[[4]])
median(Baseball[[4]])
max(Baseball[[4]])
min(Baseball[[4]])
range<-max(Baseball[[4]])-min(Baseball[[4]])
range
mad(Baseball[[4]],center = median(Baseball[[4]]))
##############Alternative Approach############
mad(Baseball[[4]])
sd(Baseball[[4]])


mean(Baseball[[5]])
median(Baseball[[5]])
max(Baseball[[5]])
min(Baseball[[5]])
range<-max(Baseball[[5]])-min(Baseball[[5]])
range
mad(Baseball[[5]],center = median(Baseball[[5]]))
##############Alternative Approach############
mad(Baseball[[5]])
sd(Baseball[[5]])


mean(Baseball[[6]])
median(Baseball[[6]])
max(Baseball[[6]])
min(Baseball[[6]])
range<-max(Baseball[[6]])-min(Baseball[[6]])
range
mad(Baseball[[6]],center = median(Baseball[[6]]))
##############Alternative Approach############
mad(Baseball[[6]])
sd(Baseball[[6]])


#########6B#######
ggpairs(Baseball)



#########7A############
getwd()
setwd("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles")
library("readxl")
library(DescTools)


Walmart<-read_excel("D:/ORBA_Summer Semester  2020/Business Forecasting/DataFiles/DataFiles/Walmart_2.xlsx")
View(Walmart)
colSums(is.na(Walmart))
summary(Walmart[,2:4], na.rm=TRUE)
plot(Walmart[,c(1,2)],main=" Time Series Plot of Investment in Walmart Stores")
plot(Walmart[,c(1,3)],main=" Time Series Plot of Investment in Super Stores")
plot(Walmart[,c(1,4)],main=" Time Series Plot of Investment in Sam's Club")


Walmart_sales_ts<-ts(Walmart[,8],start =c(2003,1),end=2015,frequency=4)
plot(Walmart_sales_ts,main=" Time Series Plot of Wlamart Sales")

#To obtain growth rate#
sales_growth_rate<-diff(Walmart_sales_ts)*100/lag(Walmart_sales_ts,1)
plot(sales_growth_rate,main="Plot of Sales Growth Rate")

# To obtain year-to-year growth#
y2y_sales_growth_rate<-diff(Walmart_sales_ts,lag = 4)*100/lag(Walmart_sales_ts,4)
ts.plot(y2y_sales_growth_rate,main="Plot of Year-to-Year Sales Growth Rate")
### 7A#####
# Visually, the plots show that walmart would look to reduce investment in walmart stores retail outlets, but increase investments in super stores and sam's club in the future.

### 7B#####
#b. The time series plot of sales shows upward trend with seasonality, thus suggesting that sales is increasing over time. However, the growth rate plot is sationary, neither increasing nor decreasing. Also, the
#year-to-year growth rate tends to fluctuate, but assumes a downward trend between 2004 and 2015 with irregular increase in year-to-year growth sales growth rate.


