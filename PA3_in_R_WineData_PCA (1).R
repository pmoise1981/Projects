###############################################################################
###                                                                         ###
###   Predictive Analytics 3 -                                              ###
###   Dimension Reduction, Clustering and Association Rules - Using R       ###
###                             ###
###                                                                         ###
###    Assignment 2 - PCA                                          ###
###                                                                         ###
###                                                                         ###
###############################################################################



#Wine.csv dataset contains properties of wine captured from three different 
#wineries in the same region. There are 13 variables describing various 
#properties of wine and 3 classes. Variables represent chemical characteristics 
#of wine, and each case is a different wine.

#a. Conduct a principal component analysis on the original data. Interpret the 
#results. Explain why the value for PC1 is so much greater than that of any other
#column. 

#Load the data
wine.df <- read.csv("Wine.csv")

#PCs of original wine data
pcs <- prcomp(na.omit(wine.df[,-1]))
#view the importance of principal components
summary(pcs)
#view first four principal components
pcs$rotation[, 1:4]

#The first principal component explains almost all the variation between wines.
#The variance of the first principal component is much larger than the other 
#variances because Proline has the largest variance. The variance of the first 
#principal component is much larger than the other variances because the units of 
#Proline are an order of magnitude larger than all other variables. Wines high 
#in proline will have high PC1 scores. To give equal importance to all variables 
#in terms of variability, we must normalize the variables and re-run the PCA.

#b. Now perform PCA on the normalized data (only the numerical variables), and 
#generate PC scores. Create a scatterplot of PC1 vs. PC2 scores (use plot() 
#function), color-coded by Wine Type (use labels= and col= attributes in the 
#text() function). Is the information in PC1 and PC2 identical to the information
#in Wine Type? Can PC scatter plot help reveal groupings of wines and unusual 
#wines?

#PCs of normalized wine data
pcs.nor <- prcomp(na.omit(wine.df[,-1]), scale. = T)
#view the importance of principal components
summary(pcs.nor)
#view first four principal components
pcs.nor$rotation[, 1:4]

#scatterplot of PC1 vs. PC2
plot(x = pcs.nor$x[, 1], y = pcs.nor$x[, 2], xlab = "PC1", ylab = "PC2")

#color-coded by Wine Type
text(x = pcs.nor$x[, 1], y = pcs.nor$x[, 2], labels = wine.df$Type, 
     col=ifelse(wine.df$Type=="A","red", ifelse(wine.df$Type=="B","blue", "black")),
     pos=1)

#The information in PC1 and PC2 is nearly identical to the information in Wine 
#Type. Wines of types A and C have similar PC2 scores but different PC1 scores. 
#The PC scatterplot can help reveal groupings of wines and unusual wines.

