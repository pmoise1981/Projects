###############################################################################
###                                                                         ###
###   Predictive Analytics 1 - Machine Learning Tools - Using R             ###
###                            ###
###                                                                         ###
###    Naive Bayes Classifier                       ###
###                                                                         ###
###                                                                         ###
###############################################################################

###
### In RStudio, set your working directory. From Menu Bar, go to Session > Set 
### Working Directory > To Source File Location.
### Install the package(s) if required. To do so, uncomment the 
### install.packages line(s).
###

#install.packages("e1071")

library(e1071)

###Load the data
bank.df <- read.csv("UniversalBank.csv")

###Consider only the required variables
bank.df <- bank.df[ , c(10, 13, 14)]
###Reorder your variables. Put the response last.
bank.df <- bank.df[ , c(2:3, 1)]  
###Look the new order.
t(t(names(bank.df)))  

ntotal <- length(bank.df$Personal.Loan)
ntrain <- round(ntotal * 0.6)
nvalid <- ntotal - ntrain
####Set the seed for the random number generator for reproducing the partition.
set.seed(12345)  
###Sample row numbers randomly.
ntrain.index <- sort(sample(ntotal, ntrain))  
train.df <- bank.df[ntrain.index, ]
valid.df <- bank.df[-ntrain.index, ]

###
### Question 1 - Create a pivot table for the training data with Online as a 
### column variable , CC as a row variable, and Loan as a secondary row variable. 
### The values inside the cells should convey the count (number of records). 
###


###We generally need to use $ to access the variables from a data frame. This is 
###ok when variable names are short. However, when variable names are longer it 
###would be helpful attach() the data frame to make objects within dataframe 
###accessible in R with fewer keystrokes and get nice output format as variable 
###names won't contain both the data frame name and $ in it. At the end, please do not
###forget to detach() the data frame.
###

attach(train.df)

###Pivot table of training set with Online as a column variable , CC as a row 
###variable, and Loan as a secondary row variable. We use ftable() function
ftable(CreditCard, Personal.Loan, Online)

###
### Question 2 - Consider the task of classifying a customer who owns a bank 
### credit card and is actively using online banking services. Looking at the 
### pivot table that you created, what is the probability that this customer 
### will accept the loan offer?
### 

#Pivot Table

###> ftable(CreditCard, Personal.Loan, Online)
###                         Online    0    1
### CreditCard Personal.Loan                 
### 0          0                     768 1152
###            1                      83  121
### 1          0                     306  490
###            1                      35   45

###There are 490 + 45 = 535 records where online = 1 and cc = 1. 45 of them accept
###the loan, so the conditional probability is 45/535 = 0.08411
p111 <- 45/535

###
### Question 3 - Create two separate pivot tables for the training data. One will 
### have Loan (rows) as a function of Online (columns) and the other will have 
### Loan (rows) as a function of CC.
### Compute the probabilities below (report three decimals).
### Note: P(A|B) means "the probability of A given B".
###
###  1. P(CC = 1|Loan = 1) = the proportion of credit card holders among the loan 
###   acceptors
###  2. P(Online = 1|Loan = 1)
###  3. P(Loan = 1) = the proportion of loan acceptors
###  4. P(CC = 1|Loan = 0)
###  5. P(Online = 1|Loan = 0)
###  6. P(Loan = 0)

###Pivot table for Loan (rows) as a function of Online (columns). Here we can use 
###basic table() function as well.
table(Personal.Loan, Online)
###or alternatively you can use addmargins() function to print row and column sums
addmargins(table(Personal.Loan, Online))
###> addmargins(table(Personal.Loan, Online))
###
###              Online
### Personal.Loan    0    1  Sum
###           0   1074 1642 2716
###           1    118  166  284
###           Sum 1192 1808 3000

###Pivot table for Loan (rows) as a function of CC (columns)
table(Personal.Loan, CreditCard)
###or alternatively you can use addmargins() function to print row and column sums
addmargins(table(Personal.Loan, CreditCard))
###> addmargins(table(Personal.Loan, CreditCard))
###
###             CreditCard
### Personal.Loan    0    1  Sum
###           0   1920  796 2716
###           1    204   80  284
###           Sum 2124  876 3000

###1.P(CC = 1|Loan = 1) = 0.2817
p1 <- 80/284
p1

###2. P(Online = 1|Loan = 1) = 0.5845
p2 <- 166/284
p2

###3. P(Loan = 1) = 0.09467
p3 <- 284/3000
p3

###4. P(CC = 1|Loan = 0) = 0.2931
p4 <- 796/2716
p4

###5. P(Online = 1|Loan = 0) = 0.6046
p5 <- 1642/2716
p5

###6. P(Loan = 0) = 0.9053
p6 <- 2716/3000
p6

###
### Question 4 - Compute the naive Bayes probability P(Loan = 1|CC = 1, Online = 1).
### Note: Use the quantities that you computed in the previous question.
###

###Refer to naive bayes formula (8.3) in the book
### P(Loan = 1|CC = 1, Online = 1) = 
### P(Loan = 1) * [P(CC = 1 / Loan = 1) * P(Online = 1 / Loan = 1)] / 
### P(Loan = 1) * [P(CC = 1 / Loan = 1) * P(Online = 1 / Loan = 1)] + P(Loan = 0) *[P(CC = 1 / Loan = 0) * P(Online = 1 / Loan = 0)]
###
p111.1 <- (p3*(p1*p2)/((p3*(p1*p2))+(p6*(p4*p5))))
p111.1
###> p111.1
###[1] 0.08856256

###
### Question 5 - Of the two values that you computed earlier ( computed in Q2 
### and Q4), which is a more accurate estimate of P(Loan=1|CC=1, Online=1)?
###

###The value obtained from the crossed pivot table is the more accurate estimate, 
###since it does not make the simplifying assumption that the probabilities (of 
###taking a loan if you are a cc holder and if you are an online customer) are 
###independent. It is feasible in this case because there are few variables and 
###few categories to consider, and thus there are ample data for all possible
###combinations.

###
### Question 6 - In R, run naive Bayes on the training data and Examine the output
### and find entries that are needed for computing P(Loan = 1|CC = 1, Online = 1).
### Compute this probability, and also the predicted probability for 
### P(Loan=1 | Online = 1, CC = 1).
###

###check if variables in the dataset are correctly identified for their types
str(bank.df)
str(train.df)
###Change the types of variables to appropriate ones
###We only need it for training data for now
train.df$Online <- as.factor(train.df$Online)
train.df$CreditCard <- as.factor(train.df$CreditCard)
train.df$Personal.Loan <- as.factor(train.df$Personal.Loan)
str(train.df)
###Fit the naive Bayes model
nb <- naiveBayes(Personal.Loan ~ Online + CreditCard, data = train.df)
nb
###Check the "Conditional Probabilities" in the output and identify the entries
###required to comput P(Loan=1 | Online = 1, CC = 1)

###Predict probabilities
pred.prob <- predict(nb, newdata = train.df, type = "raw")
predicted <- cbind(train.df, pred.prob)
###Check for the probability of "1" in the row where Online = 1 and CreditCard = 1
head(a, 100)
###That gives
###P(Loan=1 | Online = 1, CC = 1) = 0.08856256

detach(train.df)