#Set Lokasi Kerja
setwd("D:/TA_DataMining")
getwd()

#Read Dataset 
dataset <- read.csv("BrentOilPrices.csv")
attach(dataset)

#Install package and load library
install.packages("MASS")
install.packages("tseries")
install.packages("forecast")
library(MASS)
library(tseries)
library(forecast)

#plot and convert to ln format
lnstock = log(Price[1:96])
lnstock

#price correlation acf, pacf
acf(lnstock, lag.max=20)
pacf(lnstock, lag.max=20)

#Time series and auto.arima
pricearima <- ts(lnstock, start = c(1987,05),  frequency = 12)
fitlnstock <- auto.arima(pricearima)
fitlnstock
plot(pricearima,type='l')
title('Brent Oil Price')
exp(lnstock)

#forecasted values from arima
forecastedvalues_ln = forecast(fitlnstock,h=26)
forecastedvalues_ln
plot(forecastedvalues_ln)

forecastedvaluesextracted=as.numeric(forecastedvalues_ln$mean)
finalforecastedvalues=exp(forecastedvaluesextracted)
finalforecastedvalues

#percentage error
df<-data.frame(Price[96:121],finalforecastedvalues)
col_headings<-c("Actual Price","Forecasted Price")
names(df)<-col_headings
attach(df)
percentage_error=((df$`Actual Price` -df$`Forecasted Price`)/(df$`Actual Price`))
percentage_error
mean(percentage_error)