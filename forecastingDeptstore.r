library("forecast") 
library("zoo") 

DepartmentStoreSales.data <- read.csv("DepartmentStoreSales.csv") 

fixed.nValid <- 4 

DepartmentStoreSales.ts <- ts(DepartmentStoreSales.data$Sales, freq= 4, start = c(1,1)) 
fixed.nTrain <- length(DepartmentStoreSales.ts) - fixed.nValid 

DepartmentStoreSales_train.ts <- window(DepartmentStoreSales.ts, start = c(1, 1), end=c(1,fixed.nTrain)) 
DepartmentStoreSales_valid.ts <- window(DepartmentStoreSales.ts, start = c(1, fixed.nTrain + 1), end = c(1, fixed.nTrain + fixed.nValid)) 

DepartmentStoreSales_valid2.ts <- window(DepartmentStoreSales.ts, start = c(1 , fixed.nTrain + 1), end = c(1, fixed.nTrain + 2)) 

plot(DepartmentStoreSales_train.ts, xlab = "Time (Yearly)", ylab = "Sales",  
     ylim = c(40000, 120000), bty = "l" ) 
ets.hw <- ets(DepartmentStoreSales_train.ts, restrict = FALSE, model = 'AMM', alpha = 0.2, beta = 0.15 , gamma = 0.05)  
ets.hw.pred <- forecast(ets.hw, h = fixed.nValid, level = 0 ) 
ets.hw.pred
plot(ets.hw.pred, xlab = "Time (Yearly)", ylab = "Sales",ylim = c(40000, 120000), bty = "l" )
ets.hw.pred$mean 
plot(DepartmentStoreSales_train.ts - ets.hw.pred$fitted,  ylab = "Residuals", xlab = "Time", bty = "l", xaxt = "n", main = "") 
accuracy(ets.hw.pred, DepartmentStoreSales_valid2.ts)
#differencing 

par(mfrow = c(2,2)) 
plot(DepartmentStoreSales_train.ts, ylab = "Sales", xlab = "Time (Yearly)", bty = "l", main = "Department Store Sales") 
plot(diff(DepartmentStoreSales_train.ts, lag = 1), ylab = "Lag-1", xlab = "Ti 
     me (Yearly)", bty = "l", main = "Lag-1 Difference") 
plot(diff(DepartmentStoreSales_train.ts, lag = 4), ylab = "Lag-4", xlab = "Ti 
     me (Yearly)", bty = "l", main = "Lag-4 Difference") 
plot(diff(diff(DepartmentStoreSales_train.ts, lag = 1), lag = 4), ylab = "Lag -1, then Lag-4", xlab = "Time (Yearly)", bty = "l", main = "Twice-Differenced (Lag-1, Lag-4)") 

#e 
#(average method) 
double_difference<-(diff(diff(DepartmentStoreSales_train.ts, lag = 4))) 
double_difference 
mean.pred <- meanf(diff(diff(DepartmentStoreSales_train.ts, lag = 4), lag = 1 ), h=2) 
mean.pred 
#> mean.pred$mean 
#    Qtr1  Qtr2 
#   569.2 569.2 
#y17 <- DepartmentStoreSales_train.ts[17] 
#> y20 <- DepartmentStoreSales_train.ts[20] 
#> y16 <-  DepartmentStoreSales_train.ts[16] 
#> F21 <- 569.2 
#> y21 <- F21 + y17 + y20 -y16 
#y21 -- 63982.2 
#> y18 <- DepartmentStoreSales_train.ts[18] 
#> F22 <- 569.2 
#> y22 <- F22 + y18 + y21 - y17 
#> y22 #[1] 68177.4 

accuracy( c(63982.2, 68177.4), c(60800, 64900))
snaive.pred <- snaive(DepartmentStoreSales_train.ts, h=2) 
snaive.pred$mean
accuracy(snaive.pred, DepartmentStoreSales_valid2.ts)

