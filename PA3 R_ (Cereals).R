###############################################################################
###                                                                         ###
###   Predictive Analytics 3 -                                              ###
###   Dimension Reduction, Clustering and Association Rules - Using R       ###
###                             
###                                                                         ###
###    - Cluster Analysis (Cereals)                   ###
###                                                                         ###
###                                                                         ###
###############################################################################

###
###  
### 
###  
### 

###The dataset Cereals.csv includes nutritional information, store display, and 
###consumer ratings for 77 breakfast cereals.
###Data Preprocessing. Remove all cereals with missing values

###a. Apply hierarchical clustering to the data using Manhattan distance to the 
###normalized measurements. Compare the dendrograms from single linkage and 
###complete linkage, and look at cluster centroids. Comment on the structure of 
###the clusters and on their stability. Hint: To obtain cluster centroids for 
###hierarchical clustering, compute the average values of each cluster members, 
###using the aggregate() function.

# load the data
cereals.df <- read.csv("Cereals.csv")
head(cereals.df)
# preprocess 
records.missing <- rowSums(is.na(cereals.df))>0
cereals.df.nonmissing <- cereals.df[!records.missing,]

# normalize (instead of setting apriori weights)
cereals.df.nonmissing.norm <- sapply(cereals.df.nonmissing[,-c(1:3)], scale)

# cluster analysis: single vs. complete
d.norm <- dist(cereals.df.nonmissing.norm, method = "manhattan")
hc1 <- hclust(d.norm, method = "single")
plot(hc1, hang = -1, ann = FALSE)
hc2 <- hclust(d.norm, method = "complete")
plot(hc2, hang = -1, ann = FALSE)


###b. Which method leads to the most insightful or meaningful clusters? Do you 
###think the analysis will change if you use Euclidean distance?

#Complete linkage leads to more insightful clusters.  


###c. Choose one of the methods. How many clusters would you use? What distance 
###is used for this cutoff? (Look at the dendrogram.)

#We would use complete linkage method. let's cut the dendrogram at distance 19 
#which would give us 5 clusters.

###d. The elementary public schools would like to choose a set of cereals to 
###include in their daily cafeterias. Every day a different cereal is offered, 
###but all cereals should support a healthy diet. For this goal, you are 
###requested to find a cluster of "healthy cereals." Should the data be 
###normalized? If not, how should they be used in the cluster analysis?

# use complete linkage with 5 clusters, clusters 1 and 5 are "healthier"
memb <- cutree(hc2, k = 5) # cut 5 clusters
centers <- aggregate( . ~ memb, data = cereals.df.nonmissing[,-c(1:3)], 
                      FUN = mean)
centers
