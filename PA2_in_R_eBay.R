
###############################################################################
###                                                                         ###
###   Predictive Analytics 2 - Neural Nets and Regression                   ###
###                            ###
###                                                                         ###
###    - Logistic Rgression                           ###
###                                                                         ###
###                                                                         ###
###############################################################################

###
###  
### 
###
# install.packages("caret")
library(caret)

ebay.df <- read.csv("ebayAuctions.csv")
ntotal <- length(ebay.df$Competitive.)
ntrain <- round(ntotal * 0.6)
nvalid <- ntotal - ntrain
set.seed(202)  # Seed of random number generator for reproducibility.
ntrain.index <- sort(sample(ntotal, ntrain))  # Sample rows randomly.
train.df <- ebay.df[ntrain.index, ]
valid.df <- ebay.df[-ntrain.index, ]

# With closing price.
logit.reg <- glm(Competitive. ~ ., data = train.df, family = "binomial") 
options(scipen=999, digits = 3)
summary(logit.reg)

logit.reg.pred <- predict(logit.reg, valid.df[, -8], type = "response")
logit.reg.pred[1:5]
confusionMatrix(ifelse(logit.reg$fitted > 0.5, 1, 0), train.df[, 8])
confusionMatrix(ifelse(logit.reg.pred > 0.5, 1, 0), valid.df[, 8])

# Without closing price.
logit.reg1 <- glm(Competitive. ~ ., data = train.df[ , -6], family = "binomial") 
options(scipen=999, digits = 3)
summary(logit.reg1)

logit.reg1.pred <- predict(logit.reg1, valid.df[, -c(6, 8)], type = "response")
logit.reg1.pred[1:5]
confusionMatrix(ifelse(logit.reg1$fitted > 0.5, 1, 0), train.df[, 8])
confusionMatrix(ifelse(logit.reg1.pred > 0.5, 1, 0), valid.df[, 8])

#The coefficient for "close price" is 0.10402. This positive coefficient means that higher
#closing prices are associated with a higher probability of the auction being
#competitive. Looking at the odds coefficient (1.11), we see that every $1 increase in
#the closing price increases the odds of the auction being competitive by a factor of
#1.11.