sp500 = getSP500Stocks()
install.packages("fma")

library(quantmod)
library(forecast)
library(fma)
library(ggplot2)
library(MASS)
micron <- getSymbols("MU", from = "2017-07-07", to = Sys.Date(), auto.assign = FALSE,)
micron = xts(micron)[,6]
head(micron,3)

micron1 = getSymbols("MU", src = "yahoo", from = "2017-07-07", auto.assign = F,return.class="xts")[,6]
head(micron1,3)

Tesla = getSymbols("TSLA", src = "yahoo", from = "2017-07-07", auto.assign = F, return.class="xts")[,6]
head(Tesla,3)

price = data.frame(micron, Tesla)
head(price,3)

tsmicron = ts(micron)
head(tsmicron,3)
library(zoo)
plot(index(micron),tsmicron,type="l")

library(fma)
library(ggplot2)

microndiff = diff(tsmicron)
install.packages("forecast")
install.packages("PerformanceAnalytics")
library(forecast)
ma1 = ma(tsmicron,50)
if(!require(installr)) {
  install.packages("installr"); require(installr)}
forecast:::.onLoad
unloadNamespace("forecast")
library(forecast)
updateR()
plot(ma1)
library(xts)
library(Quandl)
?Quandl
quandldata = Quandl("NSE/OIL", collapse="monthly", start_date="2013-01-01", type="ts")
head(quandldata,3)
plot(quandldata[,1])


plot(head(index(micron)+50,-50, na.omit(ma1),type = "l"))

data("pigs")
 head(pigs,24)
 str(pigs)
 autoplot(pigs)
 seasonplot(pigs)
 auto.arima(pigs)
 
 
 set.seed(123789)
 
 boot.garch = ugarchboot(fit.garch,
                         method = c("Partial","Full")[1],
                         sampling = "raw",
                         n.ahead = 1,
                         n.bootpred = 100000,
                         solver = "solnp")
 
 boot.garch <- ugarchboot(fit.garch,
                          method=c("Partial","Full")[1],sampling = "raw", n.ahead = 1,n.bootpred = 100000,solver = "solnp")
 
rvec = boot.garch@fseries
options(scipen = 999)
vaR = quantile(rvec,0.05)
round(vaR,6)
ES = mean(rvec[rvec<vaR])
round(ES,6)