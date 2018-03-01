###############################################################################
###                                                                         ###
###   Predictive Analytics 2 - Neural Nets and Regression                   ###
###                            ###
###                                                                         ###
###    - linear Rgression                             ###
###                                                                         ###
###                                                                         ###
###############################################################################

###
###  
###
###


# install.packages line(s) 
# install.packages("corrgram")

# Load libraries.
library(corrgram)

# Load the data.
airfares.df <- read.csv("Airfares.csv")
str(airfares.df)
summary(airfares.df)  # Which variables are numeric?

# Replace the binary categorical variables with dummy variables.
# These predictors would still be considered numeric but reference cateogry would
# be the one which is set as zero.
airfares.df$VACATION <- ifelse(airfares.df$VACATION == "Yes", 1 , 0)  # Create a dummy variable.
airfares.df$SW <- ifelse(airfares.df$SW == "Yes", 1 , 0)  # Create a dummy variable.
airfares.df$SLOT <- ifelse(airfares.df$SLOT == "Controlled", 1 , 0)  # Create a dummy variable.
airfares.df$GATE <- ifelse(airfares.df$GATE == "Constrained", 1 , 0)  # Create a dummy variable.

# Partition the data into training and testing sets.
ntotal <- length(airfares.df$FARE)
ntrain <- round(ntotal * 0.6)
nvalid <- ntotal - ntrain
set.seed(202)  # Seed of random number generator for reproducibility.
ntrain.index <- sort(sample(ntotal, ntrain))  # Sample rows randomly.
train.df <- airfares.df[ntrain.index, ]
valid.df <- airfares.df[-ntrain.index, ]

# Create a correlation table and pairs plot.
t(t(names(train.df)))  # What are the column numbers of the numeric variables?
cor(train.df[, 5:18])
pairs(FARE ~ ., data = train.df, pch = ".")
corrgram(train.df)  # In terms of the correlation with FARE, DISTANCE is the brighest blue (or the most positively correlated).  It appears to be the best predictor.
plot(train.df$DISTANCE, train.df$FARE)  # Take a look at the relationship more closely.

# Fit a linear regression.
airfares.lm <- lm(FARE ~ ., data = train.df[, 5:18])
summary(airfares.lm)

# Create new testing set row.  Convert binary categories into 0/1.  Remove "," and "$".
new.row.df <- data.frame(COUPON = 1.202, NEW = 3, VACATION = 0, SW = 0, HI = 4442.141, S_INCOME = 28760, E_INCOME = 27664, S_POP = 4557004, E_POP = 3195503, SLOT = 0, GATE = 0, PAX = 12782, DISTANCE = 1976)

airfares.lm.pred.new.row <- predict(airfares.lm, new.row.df)
airfares.lm.pred.new.row  # $252.74

# Several of the variables would not be available until after flights start operating on the route.
cbind(t(t(names(train.df)[5:17])), c("Not Available", "Not Available", "Available", "Not Available", "Not Available", "Available", "Available", "Available", "Available", "Available", "Available", "Available", "Not Available"))
