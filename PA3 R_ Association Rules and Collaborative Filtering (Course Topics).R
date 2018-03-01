###############################################################################
###                                                                         ###
###   Predictive Analytics 3 -                                              ###
###   Dimension Reduction, Clustering and Association Rules - Using R       ###
###                             
###                                                                         ###
###    - Association Rules and Collaborative          ###
###                            Filtering (Course Ratings)                   ###
###                                                                         ###
###                                                                         ###
###############################################################################

###
### 
#install.packages("recommenderlab")
#
### Question 1 - We again consider the data in CourseTopics.csv describing course 
### purchases at Statistics.com (see Problem 14.2 and data sample in Table 14.13). 
### We want to provide a course recommendation to a student who purchased the 
### Regression and Forecast courses. Apply user-based collaborative filtering to 
### the data. You will get a Null matrix. Explain why this happens. 

# Load and clean data.
course.df <- read.csv("Coursetopics.csv")
#change dataframe to matrix
course.mat <- as.matrix(course.df)
course.mat

library(recommenderlab)

# user-based collaborative filtering
r <- as(course.mat, "realRatingMatrix")
UB.Rec <- Recommender(r, "UBCF")
pred <- predict(UB.Rec, r, type="ratings")
as(pred, "matrix")

#This gives null matrix since input is not a rating matrix but a binary one which
#we are using a real matrix.  

### Question 2 - The file Cosmetics.csv contains the data on the cosmetics 
### purchases. Using R, apply association rules to these data (use the default 
### parameters).

library(arules)

#Read and clean data.
cosmetics.df <- read.csv("Cosmetics.csv")
#change dataframe to matrix
cosmetics.mat <- as.matrix(cosmetics.df)[,-1]

# Recast incidence matrix into transcations list. 
cosmetics.trans <- as(cosmetics.mat, "transactions")
inspect(cosmetics.trans)
itemFrequencyPlot(cosmetics.trans)

# Generate rules with the highest lift.
rules <- apriori(cosmetics.trans)
inspect(rules)
inspect(head(sort(rules, by = "lift"), 20))

### a. Interpret the first three rules in the output in words. 

#First row: If brushes are purchased, nail polish is purchased. This rule has 
#100% confidence -- purchasing a brush guarantees purchase of nail polish.It has 
#lift of 3.571, and support of about 15% (149 transactions out of 1000) for the 
#two items together.
#Second row: if nail Blush, Concealer and Eye.shadow are purchased, Mascara is 
#purchased. This rule has confidence of 95.96% -- if Blush, Concealer and 
#Eye.shadow are purchased, Mascara is 95.96% likely to be purchased as well. 
#It has lift of 3.571, and support of about 12%.
#Third row: If Blush and Eye.shadow are purchased, Mascara is also purchased. 
#This rule has confidence of 92.85%, lift of 2.60, and support of about 17%.


### b. Reviewing the first couple of dozen rules, comment on their redundancy and 
### how you would assess their utility. 

#First, a note about utility. From a static retail presentation perspective 
#(buy X together with Y), the shopper's attention can probably only handle a 
#couple of rules. Coupon and web offer generating systems have no such limit, 
#because, while one or two offers are presented to a give customer at a given 
#time, other customers, and this customer at a different time may receive 
#different offers.

#Many rules come in pairs that are mirror images of one another, 
#so we can tackle them that way.

#The first rule is certain so no need to make an offer.

#All remaining rules involve mascara, mostly as a consequent. Mascara is a good 
#bet as a companion product in general -- say for a retail display. 

#Rules 2-10 could be consolidated into a general offer covering the 5 products 
#that keep reappearing in these "multi-item" rules: eyeliner, mascara, concealer,
#eyeshadow, and blush. These seem to be the favorites of big spenders, so a 
#"buy 3, 50% off on two others" or something similar might work.

