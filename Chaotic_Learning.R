### Author: EA
### Description:
# This is more of a Jackson Pollock learning experiment I did when I first started working with time series and forecasting.
# My intention was to get acquainted with the formulas available, the new classes and how to manipulate them. 

library(quantmod)
library(PerformanceAnalytics)
library(urca)
library(forecast)
library(rcompanion)


getSymbols("^GSPC", from = "2016-02-23", to = "2017-09-16")

GSPC1 <- GSPC[, "GSPC.Adjusted", drop=F]

gspc.seasonlog = diff(diff(log(GSPC1),12))
gspc.seasonlog=na.omit(gspc.seasonlog)

m<-mean(gspc.seasonlog)
std<-sqrt(var(gspc.seasonlog))
plotNormalHistogram(gspc.seasonlog, lwd = 3)
plot(gspc.seasonlog)
ur.kpss(gspc.seasonlog)
arima <- Arima(GSPC1, order = c(0,1,1), seasonal = c(0,1,1))
frcast <- forecast(arima, h=50)
plot(frcast)
acf(gspc.seasonlog)
pacf(gspc.seasonlog)
checkresiduals(arima)
regarima <- auto.arima(GSPC[,"GSPC.Adjusted"], xreg = GSPC$GSPC.High)
checkresiduals(regarima)
fcast <- forecast(regarima, xreg = GSPC$GSPC.High, h=50)
