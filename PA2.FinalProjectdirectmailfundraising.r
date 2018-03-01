###############################################################################
###                                                                         ###
###   Predictive Analytics 2 - Neural Nets and Regression                   ###
###                            ###
###                                                                         ###
###    Final Project- DIRECT-MAIL FUNDRAISING                                               ###
###                                                                         ###
###                                                                         ###
###############################################################################

###
###  
### 
###

# Install the package(s) below once on your machine. To do so, uncomment the 
# install.packages line(s) below.
# install.packages("caret")
# install.packages("neuralnets")
# install.packages("forecast")

# Load libraries.
library(caret)
library(neuralnets)
library(forecast)

# Load the data.

fundraising.df <- read.csv("Fundraising.csv")
futurefunds.df <- read.csv("FutureFundraising.csv")

## descriptive statistics
table(fundraising.df$TARGET_B) # %donate
mean(fundraising.df$TARGET_D[fundraising.df$TARGET_B == 1]) # mean donation

# net profit per person when sending to everyone: 
mean(fundraising.df$TARGET_D[fundraising.df$TARGET_B == 1] - 0.68)*0.051 - 0.68 * (1-0.051)

## Step 1: partition
set.seed(12345)
train.index <- sample(1:dim(fundraising.df)[1], dim(fundraising.df)[1]*0.6)
train.df <- fundraising.df[train.index,-c(1,2)]
valid.df <- fundraising.df[-train.index,-c(1,2)]


## Step 2: model building

#In this data set, the cases of interest (the "1's") are fairly rare. Since 
# sample sizes for training and validating models are limited by the 
# algorithms, it is best to have fairly equal numbers of 0's and 1's to give 
# the algorithm a good shot at finding out what distinguishes the two classes. 
# Think of it this way - if you have 100 1's and 100 0's, adding another 800 0's
# is not going to help nearly as much as adding 400 of each.

# classification models: The models shown here are Logistic Regression and Neural
# Nets

# logistic regression
logistic.reg <- glm(TARGET_B ~ ., data = train.df[,-22], family = "binomial")
pred.logistic <- predict(logistic.reg, newdata = valid.df, type = "response")
confusionMatrix(1*(pred.logistic > 0.5), valid.df$TARGET_B) ## accuracy ~ 0.54


# neural net
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)
nn.model <- neuralnet(TARGET_B ~  zipconvert_2 + zipconvert_3 + zipconvert_4 + 
                        zipconvert_5  + homeowner.dummy + NUMCHLD + INCOME +          
                        gender.dummy + WEALTH + HV + Icmed + Icavg + IC15 + 
                        NUMPROM + RAMNTALL + MAXRAMNT + LASTGIFT + totalmonths +     
                        TIMELAG + AVGGIFT, data = train.norm.df, hidden = c(2))
plot(nn.model)
pred.nn <- compute(nn.model, valid.norm.df[, -c(21,22)])$net.result 
confusionMatrix(1*(pred.nn), valid.df$TARGET_B) ## accuracy ~ 0.55


# profit (re-run for each classification model)
net.profit <- ifelse(valid.df$TARGET_B == 1, (13 - 0.68)/9.8, -0.68/0.53)

prob.rank <- order(pred.logistic, decreasing = T)
plot(cumsum(net.profit[prob.rank]), xlab = "Probability Rank", ylab = "profit", type = "l", col = "red")
prob.rank <- order(pred.nn, decreasing = T)
lines(cumsum(net.profit[prob.rank]), xlab = "Probability Rank", ylab = "profit", col = "blue")

legend(400, 20, c("Logistic", "NN"), col = c("red", "blue"), lty = 1)

#The dominant model seems to be Logistic regression. 

## Step 3: predict
pred.future <- predict(logistic.reg, newdata = futurefunds.df, type = "response")
data.frame(futurefunds.df$Row.Id., pred.future)[order(pred.future, decreasing = T),]




