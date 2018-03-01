###############################################################################
###                                                                         ###
###   Predictive Analytics 1 - Machine Learning Tools - Using R             ###
###                          
###                                                                         ###
###    Final Assignment                                            ###
###                                                                         ###


 



library(FNN)
library(rpart)
library(e1071)
library(caret)
library(forecast)
library(adabag)
library(randomForest)

## Load the data
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
bank.df$Personal.Loan = as.factor(bank.df$Personal.Loan)

#Set the random seed and partition the data
set.seed(12345)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
valid.index <- setdiff(c(1:dim(bank.df)[1]), train.index)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]

# PART A
# a
#knn model
#Normalize your training, validation, and new data.
train.norm.df <- train.df
valid.norm.df <- valid.df
bank.norm.df <- bank.df
pre.process.values <- preProcess(train.df[, -8], method = c("center", "scale"))
train.norm.df[, -8] <- predict(pre.process.values, train.df[, -8])
valid.norm.df[, -8] <- predict(pre.process.values, valid.df[, -8])

#FIT A KNN MODEL
kn <- knn(train.norm.df[, -8], valid.norm.df[,-8], cl = train.norm.df[, 8], k = 3, 
          prob=TRUE)

confusionMatrix(kn, valid.df[,8])
KNNProb = 1-attr(kn, "prob")
KNNPred = kn
head(KNNPred, 50)

#FIT A NAIVE BAYES MODEL

#naive bayes model need all predictors categorical
#There are some categorical predictors (Family, Online, CreditCard, Education,
#Securities Account, CD.Account) which R recognized as integers. Some methods 
#won't give you the trouble working with them, but naive bayes will.
#let's convert them to categorical
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
bank.df$Personal.Loan = as.factor(bank.df$Personal.Loan)
bank.df$Family <- as.factor(bank.df$Family)
bank.df$Online <- as.factor(bank.df$Online)
bank.df$CreditCard <- as.factor(bank.df$CreditCard)
bank.df$Education <- as.factor(bank.df$Education)
bank.df$Securities.Account <- as.factor(bank.df$Securities.Account)
bank.df$CD.Account <- as.factor(bank.df$CD.Account)
#others (Age, Expereince, Income, Mortgae and CCAvg) we will bin
bank.df$Age <- factor(cut(bank.df$Age, 5, labels = c(1:5)))
bank.df$Experience <- factor(cut(bank.df$Experience, 10, labels = c(1:10)))
bank.df$Income <- factor(cut(bank.df$Income, 5, labels = c(1:5)))
bank.df$CCAvg <- factor(cut(bank.df$CCAvg, 6, labels = c(1:6)))
bank.df$Mortgage <- factor(cut(bank.df$Mortgage, 10, labels = c(1:10)))
str(bank.df)
#Set the random seed and partition the data
set.seed(12345)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
valid.index <- setdiff(c(1:dim(bank.df)[1]), train.index)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]

#fit a naive bayes model
nb <- naiveBayes(Personal.Loan ~ ., data = train.df)

confusionMatrix(predict(nb, valid.df), valid.df$Personal.Loan)

NBProb = predict(nb, newdata = valid.df, type = "raw")[,2]
NBPred = predict(nb, valid.df)

#FIT A TREE MODEL

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
bank.df$Personal.Loan = as.factor(bank.df$Personal.Loan)
#Set the random seed and partition the data
set.seed(12345)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
valid.index <- setdiff(c(1:dim(bank.df)[1]), train.index)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]
tr <- rpart(Personal.Loan ~., data = train.df)
confusionMatrix(ifelse(predict(tr, valid.df)[,2]>0.5, 1, 0), 
                valid.df$Personal.Loan)
TREEProb = predict(tr, valid.df)[,2]
TREEPred = ifelse(predict(tr, valid.df)[,2]>0.5, 1, 0)

# b

res<- data.frame(valid.df$Personal.Loan,KNNProb,KNNPred,NBProb,NBPred,TREEProb,
                 TREEPred)
head(res, 100)

# c

res$majority <- 
  rowMeans(data.frame(as.numeric(res$NBPred), as.numeric(res$KNNPred), res$TREEPred))>0.5
res$avg <- rowMeans(data.frame(res$NBProb, res$KNNProb, res$TREEProb))
head(valid.df)
tail((res$majority * 1), 15)
confusionMatrix(res$majority * 1, valid.df[,8])
confusionMatrix((res$avg > 0.5)* 1, valid.df[,8])

# d

#From the aprediction accuracy measures for each of above methods, we can 
#see that classification tree has outperofrmed KNN and Naive Bayes

#PART B:
#Now let's fit bagging and boostin trees

bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
bank.df$Personal.Loan = as.factor(bank.df$Personal.Loan)

#Set the random seed and partition the data
set.seed(12345)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
valid.index <- setdiff(c(1:dim(bank.df)[1]), train.index)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[valid.index, ]

# Bagging

bag <- bagging(Personal.Loan ~., train.df)
pred <- predict(bag, valid.df, type="class")
confusionMatrix(pred$class, valid.df$Personal.Loan)

# Boosting

boost <- boosting(Personal.Loan ~., train.df)
pred <- predict(boost, valid.df, type="class")
confusionMatrix(pred$class, valid.df$Personal.Loan)

#Among the models we fitted, the predictive accuracy of Boosted tree is 
#highest (98.6%) and thus it is the winner here
