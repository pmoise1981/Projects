library(forecast)
library(quantmod)
library(moments)

dex = getSymbols("DEXUSUK", src = 'FRED', auto.assign = F)
dex = na.omit(dex)
head(dex,3)
dex = dex["1979-12-31/2017-12-31"]
`colnames<-`(dex,"TR")
# Daily log returns
logrett = diff(log(dex$DEXUSUK))[-1]
round(head(logrett,3),6)
# Weekly log returns
logrett.w = apply.weekly(logrett, sum)
# Monthly log returns
logrett.m = apply.monthly(logrett,sum)
# Quaterly log returns
logrett.q = apply.quarterly(logrett, sum)
# Yearly log returns
logret.y = apply.yearly(logrett,sum)
na.omit(round(head(logrett,3),6))
ret = exp(logrett)-1
ret.w = exp(logrett.w)-1
ret.m = exp(logrett.m)-1
ret.q = exp(logrett.q)-1
ret.y = exp(logret.y)-1

round(head(ret.m,3),6)
round(tail(logrett.q,3),6)

round(tail(ret.y,3),6)
library(quantmod)
dexj = getSymbols("DEXJPUS", src = "FRED", auto.assign = F)
dexj = na.omit(dexj)
dexj = dexj["1979-12-31/2017-12-31"]
dexj = na.omit(dexj)
head(dexj,3)
dexj = 1/dexj
head(dexj,3)
# Daily log returns
dexjlogret = diff(log(dexj$DEXJPUS))[-1]
# monthly log returns
dexjlogret.m = apply.monthly(dexjlogret,sum)
# weekly log returns returns
dexjlogret.w = apply.weekly(dexjlogret,sum)
# quaterly log returns
dexjlogret.q = apply.quarterly(dexjlogret,sum)
round(tail(dexjlogret.q,3),6)
# yearly log returns
dexjlogret.y = apply.yearly(dexjlogret,sum)
head(dexjlogret,3)
# Discrete returns
dexjret = na.omit(exp(dexjlogret))-1
# Weekly discrete returns
dexjret.w = exp(dexjlogret.w)-1
# monthly discrete returns
dexjret.m = exp(dexjlogret.m)-1
round(head(dexjret.m,2),6)
# quaterly discrete returns
dexjret.q = exp(dexjlogret.q)-1
round(tail(dexjlogret.q,3),6)
# annualy discrete returns
dexjret.y = exp(dexjlogret.y)-1
round(tail(dexjret.y,3),6)

apply.monthly(dexjret,sum)
head(dexjret,3)
options(scipen = 999)
round(head(dexjlogret,3),6)

# Mean
mu= round(mean(logrett),6)
mu
# Standard
sig =round(sd(logrett),6)
sig

vaR = round(qnorm(0.01,mu,sig),6)
vaR

es <- round(mu-sig*dnorm(qnorm(0.01,0,1),0,1)/0.01,6)
es

alpha = 0.01
set.seed(123789)
rvec = rnorm(100000,mu,sig)
vaR = quantile(rvec, alpha)
ES = mean(rvec[rvec < vaR])
round(vaR,6)

round(ES,6)



alpha = 0.01
set.seed(123789)
rvec = sample(as.vector(logrett),100000, replace = T)
vaR = quantile(rvec, alpha)
ES = mean(rvec[rvec < vaR])
round(vaR,6)

round(ES,6)

1000000000*(exp(es)-1)

rvec = as.vector(dexjlogret)
round(skewness(rvec),2)
library(fma)
library(BatchGetSymbols)

sp500 <- GetSP500Stocks()
head(sp500,5)
SnP500 <- sp500[,c(1,2,4)]
head(SnP500,4)
arm(sp500)

library(moments)
# Skewness
rvec = as.vector(logrett)
round(skewness(rvec),2)

# kurtosis
round(kurtosis(rvec),2)

# Jarcque-Bera Test
jarque.test(rvec)

library(MASS)
# Maximum likelywood
t.fit =fitdistr(rvec,"t")
round(t.fit$estimate,6)
install.packages("metRology")
library(metRology)
alpha = 0.05
set.seed(123789)
rvec = rt.scaled(100000, mean = t.fit$estimate[1],sd = t.fit$estimate[2],df= t.fit$estimate[3])
vaR = quantile(rvec, alpha)
ES = mean(rvec[rvec<vaR])

round(vaR)

round(ES)
 # simulate 10 1-day lg return
 # Method A
alpha = 0.05
set.seed(123789)
rvec = rep(0,100000)
for (i in 1:10){
rvec = rvec+rt.scaled(100000, mean = t.fit$estimate[1],sd = t.fit$estimate[2],df= t.fit$estimate[3])
}
vaR = quantile(rvec, alpha) 
ES = mean(rvec[rvec<vaR])

# Method B, simulate 10 1-day log return
alpha = 0.05
set.seed(123789)
rvec = rep(0,100000)
for (i in 1:10){
  rvec = rvec+sample(as.vector(logrett),100000, replace = T) 
}
vaR = quantile(rvec, alpha) 
ES = mean(rvec[rvec<vaR])

round(vaR)

round(ES)

# test
library(moments)
options(scipen=999)
round(skewness(dexjlogret),2)

round(kurtosis(dexjlogret),2)
rvec = as.vector(dexjlogret)

jarque.test(rvec)

library(MASS)
# Maximum likelywood
t.fit =fitdistr(rvec,"t")
round(t.fit$estimate,6)
install.packages("metRology")
library(metRology)
alpha = 0.01
set.seed(123789)
rvec = rt.scaled(100000, mean = t.fit$estimate[1],sd = t.fit$estimate[2],df= t.fit$estimate[3])
vaR = quantile(rvec, alpha)
ES = mean(rvec[rvec<vaR])

round(vaR,6)

round(ES,6)  
  
# simulate 10 1-day lg return
# Method A
alpha = 0.01
set.seed(123789)
rvec = rep(0,100000)
for (i in 1:10){
  rvec = rvec+rt.scaled(100000, mean = t.fit$estimate[1],sd = t.fit$estimate[2],df= t.fit$estimate[3])
}
vaR = quantile(rvec, alpha) 
ES = mean(rvec[rvec<vaR])  
alpha = 0.01
set.seed(123789)
rvec = rep(0,100000)
for (i in 1:10){
  rvec = rvec+rt.scaled(100000, mean = t.fit$estimate[1],sd = t.fit$estimate[2],df= t.fit$estimate[3])
}
vaR = quantile(rvec, alpha) 
ES = mean(rvec[rvec<vaR])  
round(ES,6)

# Method B, simulate 10 1-day log return
alpha = 0.01
set.seed(123789)
rvec = rep(0,100000)
for (i in 1:10){
  rvec = rvec+sample(as.vector(dexjlogret),100000, replace = T) 
}
vaR = quantile(rvec, alpha) 
ES = mean(rvec[rvec<vaR])

round(vaR)

round(ES)


alpha <- 0.01
set.seed(123789)
rdat <- as.vector(dexjlogret)
rvec <- rep(0,100000)
posn <- seq(from=1,to=length(rdat)-9,by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 
round(ES,6)

alpha <- 0.01
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+ sample(as.vector(dexjlogret),100000,replace=TRUE)
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 
round(ES,6)
install.packages("rugarch")
library(rugarch)

#To estimate the GARCH(1,1) -t model, we use the "rugarch" package in R:
  library(rugarch)
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std")
fit.garch <- ugarchfit(spec = uspec, data = logret[,1])
The estimated parameters are in
fit.garch@fit$coef
#The output of the estimation are then saved:
  save1 <- cbind( logret[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(save1) <- c( "logret", "s", "z" )
We use the R function "ugarchboot" to simulate 1-day outcomes:
  set.seed(123789) #set seed value
boot.garch <- ugarchboot(fit.garch,
                         method=c("Partial","Full")[1], # ignore parameter uncertainty
                         sampling="raw", # draw from standardized residuals
                         n.ahead=1, # 1-day ahead
                         n.bootpred=100000, # number of simulated outcomes
                         solver="solnp")
                         #The simulated outcomes are then saved in the vector "rvec":
                         rvec <- boot.garch@fseries
                         The VaR and ES at the 95% confidence level are calculated as before:
                         VaR <- quantile(rvec,0.05)
                         ES <- mean(rvec[rvec<VaR])


  