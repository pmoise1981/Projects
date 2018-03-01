###############################################################################
###                                                                         ###
###   Predictive Analytics 3 -                                              ###
###   Dimension Reduction, Clustering and Association Rules - Using R       ###
###                             
###                                                                         ###
###    Assignment 3 - Cluster Analysis (University Rankings)       ###
###                                                                         ###
###                                                                         ###
###############################################################################

###
### 
### 
### 
### install.packages line(s).
### install.packages("caret")
library(caret)
###The dataset on American College and University Rankings (Universities.csv) 
###contains information on 1302 American colleges and universities offering an 
###undergraduate program. For each university, there are 17 measurements, 
###including continuous measurements (such as tuition and graduation rate) and 
###categorical measurements (such as location by state and whether it is a 
###private or public school). 
###Note that many records are missing some measurements. Our first goal is to 
###estimate these missing values from "similar" records. This will be done by 
###clustering the complete records and then finding the closest cluster for each 
###of the partial records. The missing values will be imputed from the 
###information in that cluster. 

###a. Remove all records with missing measurements from the dataset.

# load the data
universities.df <- read.csv("Universities.csv")
head(universities.df)

# non-missing records
records.missing <- rowSums(is.na(universities.df))>0
universities.df.nonmissing <- universities.df[!records.missing,]
dim(universities.df.nonmissing)

###b. For all the continuous measurements, run hierarchical clustering using 
###complete linkage and Euclidean distance. Make sure to normalize the 
###measurements. From the dendrogram: How many clusters seem reasonable for 
###describing these data? Do you think the analysis will change if you use 
###Euclidean distance?

# cluster analysis
# normalize input variables
universities.df.nonmissing.norm <- universities.df.nonmissing[, -c(1:3)]
norm.values <- preProcess(universities.df.nonmissing[, -c(1:3)], 
                          method=c("center", "scale"))
universities.df.nonmissing.norm <- predict(norm.values, 
                                           universities.df.nonmissing[, -c(1:3)])

# compute normalized distance
d.norm <- dist(universities.df.nonmissing.norm, method = "manhattan")

# compute clusters
hc1 <- hclust(d.norm, method = "complete")
plot(hc1, hang = -1, ann = FALSE)

# cut the dandrorgam
memb <- cutree(hc1, k = 2) ### can try different cuts

###c. Compare the summary statistics for each cluster and describe each cluster 
###in this context (e.g., "Universities with high tuition, low acceptance rate.").
###Hint: To obtain cluster statistics for hierarchical clustering, use the 
###aggregate() function.

# summary statistics for cluster
centers <- aggregate( . ~ memb, data = universities.df.nonmissing, FUN = mean)
centers.norm <- aggregate( . ~ memb, data = universities.df.nonmissing.norm, FUN = mean)

###d. Use the categorical measurements that were not used in the analysis (State 
###and Private/Public) to characterize the different clusters. Is there any 
###relationship between the clusters and the categorical information.

# summary by categorical variables
table(universities.df.nonmissing$State, memb)
table(universities.df.nonmissing$Public.vs.Private, memb)

###What other external information can explain the contents of some or all of 
###these clusters?

#State, not being a numeric variable, is not included in the clustering criteria.
#Note that southern states are under-represented in cluster 2 compared to 
#cluster 1, and New England states are under-represented in cluster 1. Each 
#group tends to have distinctive social and cultural norms. For example, sports 
#receives relatively greater influence in the south than in New England. New 
#England is known for its private college preparatory schools (particularly its
#elite ones) that feed students into selective (usually private and, hence, 
#unsubsidized) universities. These factors could contribute to the higher cost 
#and higher college entrance exam scores of cluster 2.
#Recall that all variables must be present for a school to be included in the 
#clustering process. Note that most of the Ivy League and several other key top 
#schools are missing - this could be because they choose not to report some of 
#the data used in the US News rankings. Since these schools are missing in the 
#"cluster-building" phase, it is possible that our set of clusters is incomplete. So, our set of clusters may be incomplete.
#There may be other factors as well that could inform our clustering process.


###BONUS. Consider Tufts University, which is missing some information. Compute the 
###Euclidean distance of this record from each of the clusters that you found 
###above (using only the measurements that you have). Which cluster is it closest 
###to? Impute the missing values for Tufts by taking the average of the cluster 
###on those measurements.

# distance from Tufts University to clusters (normalized)
tufts.ind <- which(universities.df$College.Name == "Tufts University")
Tufts.norm <- predict(norm.values, universities.df[tufts.ind,-c(1:3)])

na.ind <- which(is.na(Tufts.norm)==TRUE)
df <- rbind(centers.norm[,-1], Tufts.norm)

dist(df[-na.ind], method = "euclidean")

# impute missing (from raw data - non-normalized)
Tufts.No.PT.undergrad <- centers[1,]$No.PT.undergrad
Tufts.No.PT.undergrad
