###############################################################################
###                                                                         ###
###   Predictive Analytics 1 - Machine Learning Tools - Using R             ###
###                            ###
###                                                                         ###
###   Solution: Assignment 2 - Data Visualization and Preparation           ###
###   Solution uses base R functions for solving this assignment            ###
###                                                                         ###
###############################################################################

###
###  
### 
### 

###
### Question 1 - Load the laptop sales data into R and check if it is loaded 
### successfully. (Observe how R changes the variable/column names, as compared 
### to original names in csv file, when data is read. You may want to tweak the 
### variable names for your own convenience.)
###

###Load the data
laptop.df <- read.csv("LaptopSales.csv")

###Check if data is loaded correctly
dim(laptop.df) #find the dimension of data frame
head(laptop.df, 10) #show the first ten observations

###Print the list of variables to the screen
t(t(names(laptop.df)))

###Let's prettify the variable names
colnames(laptop.df) <- c("Date", "Configuration", "Customer_Postcode", "Store_Postcode",
                         "Retail_Price", "Screen_Size_Inches", "Battery_Life_Hours",
                         "RAM_GB", "Processor_Speeds_GHz", "Integrated_Wireless", 
                         "HD_Size_GB", "Bundled_Applications", "customer_X", 
                         "customer_Y", "store_X", "store_Y")
t(t(names(laptop.df)))

###Alternatively (with more efforts), you can change the variables names as follows
colnames(laptop.df)[3] <- c("Customer_Postcode")
colnames(laptop.df)[4] <- c("Store_Postcode")
colnames(laptop.df)[5] <- c("Retail_Price")
colnames(laptop.df)[6] <- c("Screen_Size_Inches")
colnames(laptop.df)[7] <- c("Battery_Life_Hours")
colnames(laptop.df)[8] <- c("RAM_GB")
colnames(laptop.df)[9] <- c("Processor_Speeds_GHz")
colnames(laptop.df)[10] <- c("Integrated_Wireless")
colnames(laptop.df)[11] <- c("HD_Size_GB")
colnames(laptop.df)[12] <- c("Bundled_Applications")
colnames(laptop.df)[13] <- c("customer_X")
colnames(laptop.df)[14] <- c("customer_Y")
colnames(laptop.df)[15] <- c("store_X")
colnames(laptop.df)[16] <- c("store_Y")
t(t(names(laptop.df)))

###
### Question 2 - Check using appropriate R function. Hint: class or any of other 
### common function will do the job for you. Had you expected exactly the same 
### data types as shown by R? Comment.
###

###Use class() to check variable type or class of variables individually
class(laptop.df$Date)
class(laptop.df$Configuration)
class(laptop.df$Customer_Postcode)
class(laptop.df$Store_Postcode)
class(laptop.df$Retail_Price)
class(laptop.df$Screen_Size_Inches)
class(laptop.df$Battery_Life_Hours)
class(laptop.df$RAM_GB)
class(laptop.df$Processor_Speeds_GHz)
class(laptop.df$Integrated_Wireless)
class(laptop.df$HD_Size_GB)
class(laptop.df$Bundled_Applications)
class(laptop.df$customer_X)
class(laptop.df$customer_Y)
class(laptop.df$store_X)
class(laptop.df$customer_Y)

###Alternatively, use str() to compactly display the structure of data frame and is
###the easiest and fastest one. 
###R has displayed the class of most of variables as expected. The only exception
###seems to be the Date variable.
###
str(laptop.df)

###Alternatively, one can use the following
###this will provide same information as class() do
sapply(laptop.df, class) 

###this will provide slightly different information than class()
sapply(laptop.df, typeof) 

###
### Question 3 - Are there any missing values in the data? Report the findings? 
###

###The simplest way to identify missing values in the data is to use summary(). 
###The variables Retail_Price, Store_X and Store_Y contains missing values.
summary(laptop.df)

###Alternative ways
MissingValues <- 1 * is.na(laptop.df) #creates matrix, assigns 1 for missing records,
#0 otherwise
apply(MissingValues, 2, sum) #provides count of missing values in each column
#or
missingValues <- colSums(is.na(laptop.df))
missingValues

###
### Question 4 - Observe the missing values graphically and comment on it. 
###
laptop.df <- as.data.frame(laptop.df)
heatmap(1 * is.na(laptop.df[,]), Rowv = NA, Colv = NA)

###As shown by summary() function, heatmap also shows the Price, Store X and Store Y
###has some missing values (white spaces). However, due to the large data size it
###is not much useful for getting any additional insights

###
### Question 5 - At what average and median price do the laptops sell?
###

###Use boxplot or histogram to view price distribution
###Use boxplot() and hist() in R
boxplot(laptop.df$Retail_Price)
hist(laptop.df$Retail_Price)
###Boxplot shows that the median price is about $500 and that the majority of 
###laptops sell for prices between approximately $200 and $850. The histogram 
###of price shows the symmetric, single-peaked shape of the price distribution.


###
### Question 6 - Compare retail prices across stores. Do prices vary across the 
### stores? Which stores look costly in general? 
###

###Use side-by-side boxplots to compare retail prices across stores. 
### Use boxplot() in R.

boxplot(laptop.df$Retail_Price ~ laptop.df$Store_Postcode, xlab = "Store", 
        ylab = "Price")
###We can see that overall the median price is similar across stores (around $500, plus 
###minus $50). However, it is easy to see that there are two types of stores: 
###those with larger price ranges and those with smaller price ranges.


###
### Question 7 - Does price of laptop vary according to integrated wireless? 
### 

boxplot(laptop.df$Retail_Price ~ laptop.df$Integrated_Wireless, xlab = "Integrated_Wireless?", 
        ylab = "Price")
###Overall the median price is similar for both, laptops with integrated wireless 
###or no wireless

###
###Question 8 - Draw the scatter plot of retail prices with
###Screen Size
###Battery Life
###RAM
###Processor
###HD Size
###Configuration
###
###Comment on what the above scatter plots reveal and their usability.

plot(laptop.df$Screen_Size_Inches ~ laptop.df$Retail_Price, xlab = "Screen Size",
     ylab = "Retail Price")

plot(laptop.df$Battery_Life_Hours ~ laptop.df$Retail_Price, xlab = "Battery Life",
     ylab = "Retail Price")

plot(laptop.df$RAM_GB ~ laptop.df$Retail_Price, xlab = "RAM",
     ylab = "Retail Price")

plot(laptop.df$Processor_Speeds_GHz ~ laptop.df$Retail_Price, xlab = "processor Speed",
     ylab = "Retail Price")

plot(laptop.df$HD_Size_GB~ laptop.df$Retail_Price, xlab = "HD Size",
     ylab = "Retail Price")

plot(laptop.df$Configuration~ laptop.df$Retail_Price, xlab = "Configuration",
     ylab = "Retail Price")

###
### Question 9 - In your opinion, which of the configurations make bigger 
### difference in the laptop price?
###

###Use boxplots of different configuration variables one by one to see which 
###configuration option makes bigger difference than others

boxplot(laptop.df$Retail_Price ~ laptop.df$Screen_Size_Inches, xlab = "Screen Size", 
        ylab = "Price")
boxplot(laptop.df$Retail_Price ~ laptop.df$Battery_Life_Hours, xlab = "Battery Life", 
        ylab = "Price")
boxplot(laptop.df$Retail_Price ~ laptop.df$RAM_GB, xlab = "RAM", ylab = "Price")
boxplot(laptop.df$Retail_Price ~ laptop.df$Processor_Speeds_GHz, xlab = "Processor Speed", 
        ylab = "Price")
boxplot(laptop.df$Retail_Price ~ laptop.df$HD_Size_GB, xlab = "HD Size", 
        ylab = "Price")
boxplot(laptop.df$Retail_Price ~ laptop.df$Integrated_Wireless, xlab = "Integrated Wireless", 
        ylab = "Price")
boxplot(laptop.df$Retail_Price ~ laptop.df$Bundled_Applications, xlab = "Bundled Applications", 
        ylab = "Price")
###Some configuration options make a bigger price difference than others 
###(e.g., screen size has a clear impact, while bandwidth does not , RAM size 
###makes a difference mostly if you choose the largest size.)