###############################################################################
###                                                                         ###
###   Predictive Analytics 1 - Machine Learning Tools - Using R             ###
###                            ###
###                                                                         ###
###                                            ###
###                                                                         ###
###                                                                         ###
###############################################################################

###
### In RStudio, set your working directory. From Menu Bar, go to Session > Set 
### Working Directory > To Source File Location.
### Install the package(s) if required. To do so, uncomment the 
### install.packages line(s).
###
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("forecast")

# Load libraries
library(rpart)
library(rpart.plot)
library(forecast)

###
### a
###

# Load data and select variables.
car.df <- read.csv("ToyotaCorolla.csv")
# Note that for a regression tree there is no need to convert a categorical 
# variable like Fuel_Type to a dummy variable.  Regression trees in R work well 
# with categories as is.
t(t(names(car.df)))  
selected.var <- c(3, 4, 7, 8, 9, 12, 14, 17, 19, 21, 25, 26, 28, 30, 34, 39)
# Check that you selected the right ones.
t(t(names(car.df[selected.var])))  

# Create training and validation sets.
ntotal <- length(car.df$Price)
ntrain <- round(ntotal * 0.6)
nvalid <- ntotal - ntrain
# Set the seed for the random number generator for reproducing the partition.
set.seed(12345)  
# Sample row numbers randomly.
ntrain.index <- sort(sample(ntotal, ntrain))  
train.df <- car.df[ntrain.index, selected.var]
valid.df <- car.df[-ntrain.index, selected.var]
# Fit a deep regression tree and compare its prediction errors in the training 
# and validation sets.
rt <- rpart(Price ~ ., data = train.df, method = "anova", cp = 0.0001, minsplit = 5)
# This tree has 189 terminal nodes.
length(rt$frame$var[rt$frame$var == "<leaf>"])
prp(rt, type = 1, extra = 1, split.font = 1, varlen = -10)  
rt$variable.importance
# Age, HP, and KM are most important.
rt.pred.in.train.set <- predict(rt, train.df)
rt.pred.in.valid.set <- predict(rt, valid.df)
# RMSE in training set is 598.5
accuracy(rt.pred.in.train.set, train.df$Price)
# RMSE in validation set is 1345.7
accuracy(rt.pred.in.valid.set, valid.df$Price)  
# This deep tree is overfit to the training set and does poorly in predicting 
# rows in the validation set.

# Fit a shallower regression tree.  Compare its prediction errors in the training
# and validation sets.
rt2 <- rpart(Price ~ ., data = train.df, method = "anova", cp = 0.001, minsplit = 5)
length(rt2$frame$var[rt2$frame$var == "<leaf>"])   # This tree has 29 terminal nodes.
prp(rt2, type = 1, extra = 1, split.font = 1, varlen = -10)
rt2.pred.in.train.set <- predict(rt2, train.df)
rt2.pred.in.valid.set <- predict(rt2, valid.df)
# RMSE in training set is 991.5
accuracy(rt2.pred.in.train.set, train.df$Price)
# RMSE in validation set is 1241.8
accuracy(rt2.pred.in.valid.set, valid.df$Price)  
# This shallower tree does much worse on the training set than the deep tree but 
# does better in predicting rows in the validation set

# Fit a regression tree using cross-validation to choose the best setting for the 
# complexity parameter cp.  Compare its prediction errors in the training and 
# validation sets.

set.seed(12345)
cv.rt <- rpart(Price ~ ., data = train.df, method = "anova", cp = 0.00001, 
               minsplit = 5, xval = 5)  
# Print out the cp table of cross-validation errors.
printcp(cv.rt)  
# Choose the cp level to the left of the cp level with the minimum xerror that 
# is first above the line.
plotcp(cv.rt)  
# Which cp setting minimizes the cross-validation error?  cp = 0.0007230157.
(best.cp <- cv.rt$cptable[which.min(cv.rt$cptable[, "xerror"]), "CP"])  
pruned.rt <- prune(cv.rt, cp = best.cp)  
# This pruned tree (using the best cp) has 36 terminal nodes.
length(pruned.rt$frame$var[pruned.rt$frame$var == "<leaf>"])  
pruned.rt.pred.in.train.set <- predict(pruned.rt, train.df)
pruned.rt.pred.in.valid.set <- predict(pruned.rt, valid.df)
# RMSE in training set is 923.854
accuracy(pruned.rt.pred.in.train.set, train.df$Price)  
# RMSE in validation set is 1235.39
accuracy(pruned.rt.pred.in.valid.set, valid.df$Price)  
# This cross-validated tree does well on both the training and validation sets.

# Predict the price of a used car.  
head(train.df) # Be careful to use 1/0 for the Yes/No specifications.
new.row.df <- data.frame(Age_08_04 = 77, KM = 117000, Fuel_Type = "Petrol", 
                         HP = 110, Automatic = 0, Doors = 5, Quarterly_Tax = 100, 
                         Mfr_Guarantee = 0, Guarantee_Period = 3, Airco = 1, 
                         Automatic_airco = 0,  CD_Player = 0, Powered_Windows = 0, 
                         Sport_Model = 0, Tow_Bar = 1)
(pruned.rt.pred.new.row <- predict(pruned.rt, new.row.df))  
# The predicted price is $7,702.353.

###
### b
###

#Create a binned variable binned_price which categorizes Price into 20 bins. 
# It should be a factor variable

car.df$binned_price <- factor(cut(car.df$Price, 20))
head(car.df)
#Note that Price variable is still in the data frame, we will need to ignore it 
#and consder binned price as response variable.

t(t(names(car.df))) 
selected.var <- c(4, 7, 8, 9, 12, 14, 17, 19, 21, 25, 26, 28, 30, 34, 39, 40)
# Check that you selected the right ones.
t(t(names(car.df[selected.var])))  

# Create training and validation sets.
ntotal <- length(car.df$binned_price)
ntrain <- round(ntotal * 0.6)
nvalid <- ntotal - ntrain
# Set the seed for the random number generator for reproducing the partition.
set.seed(12345)  
# Sample row numbers randomly.
ntrain.index <- sort(sample(ntotal, ntrain))  
train.df.ct <- car.df[ntrain.index, selected.var]
valid.df.ct <- car.df[-ntrain.index, selected.var]

# Fit a deep classification tree.
ct <- rpart(binned_price ~ ., data = train.df.ct, method = "class", cp = 0.0001, 
            minsplit = 5)
# This tree has 184 terminal nodes.
length(ct$frame$var[ct$frame$var == "<leaf>"])
prp(ct, type = 1, extra = 1, split.font = 1, varlen = -10)  
ct$variable.importance
# Age, KM and HP are top three most important predictors. Although, it is almost
# impossible to view it in the tree. 

# Fit a shallower classisfication tree.  
ct2 <- rpart(binned_price ~ ., data = train.df.ct, method = "class", cp = 0.002, 
             minsplit = 5)
# This tree has 79 terminal nodes.
length(ct2$frame$var[ct2$frame$var == "<leaf>"])   
prp(ct2, type = 1, extra = 1, split.font = 1, varlen = -10)
ct2$variable.importance

# Fit a classification tree using cross-validation to choose the best setting for the 
# complexity parameter cp.
set.seed(12345)
cv.ct <- rpart(binned_price ~ ., data = train.df.ct, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)  
# Print out the cp table of cross-validation errors.
printcp(cv.ct)  
# Choose the cp level to the left of the cp level with the minimum xerror that is
# first above the line.
plotcp(cv.ct)  
# Which cp setting minimizes the cross-validation error?  cp = 0.00375.
(best.cp <- cv.ct$cptable[which.min(cv.ct$cptable[, "xerror"]), "CP"])  
pruned.ct <- prune(cv.ct, cp = best.cp)  
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])  
# This pruned tree (using the best cp) has 24 terminal nodes.
pruned.ct.pred.in.train.set <- predict(pruned.ct, train.df.ct)
pruned.ct.pred.in.valid.set <- predict(pruned.ct, valid.df.ct)

# From the above table and the two trees we can conclude that the two trees are 
# different in size and shape; they both use the same predictor for the initial 
# splitting.

# Predict the price of a used car.
new.row.df <- data.frame(Age_08_04 = 77, KM = 117000, Fuel_Type = "Petrol", 
                         HP = 110, Automatic = 0, Doors = 5, 
                         Quarterly_Tax = 100, Mfr_Guarantee = 0, 
                         Guarantee_Period = 3, Airco = 1, Automatic_airco = 0,  
                         CD_Player = 0, Powered_Windows = 0, Sport_Model = 0, 
                         Tow_Bar = 1)
(pruned.ct.pred.new.row <- predict(pruned.ct, new.row.df))  

# predicted class/bin using classification tree is
colnames(pruned.ct.pred.new.row)[which.max(pruned.ct.pred.new.row)]

# The predicted class using the Classification Tree is 2, which is equivalent to 
# a price range of $7160-$8570. Therefore the predicted price at which the car 
# sells lies between ($7160, $8570).

# From the classification tree we get a range for the predicted price whereas 
# the regression tree gives a predicted fixed price.

# The advantage of CT is that it is simpler and therefore more robust, but you 
# do lose accuracy by binning the price. So depending on the application, RT or 
# CT would be selected.

# For example, if your purpose were simply to set an initial sticker price at 
# which to sell the car (i.e., estimation), RT would be a good choice. On the 
# other hand, if your purpose were to determine whether the car falls above or 
# below a set cutoff (say, for inclusion in an offering to a volume buyer who 
# will not pay more than a certain amount per car), CT will focus on developing 
# rules for that particular price point and might work better.

