library(mclust)
library(NbClust)
library(gmodels)

seg_data<- read.csv("segmentationdata.csv")
head(std_seg_data)
std_seg_data <- scale(seg_data[,c("Trendy", "Styling", "Reliability", "Sportiness", "Performance", "Comfort")]) 
dist <- dist(std_seg_data, method = "euclidean")
as.matrix(dist)[1:5,1:5]
set.seed(1990)
clust <- hclust(dist, method = "ward.D2")
plot(clust)
set.seed(1990)
clust <- hclust(dist, method = "ward.D2")
plot(clust)
h_cluster <- cutree(clust, 4)
rect.hclust(clust, k=4, border="red")
table(h_cluster)
hclust_summary <- aggregate(std_seg_data[,c("Trendy", "Styling", "Reliability", "Sportiness", "Performance", "Comfort")],by=list(h_cluster),FUN=mean)
hclust_summary
plot(clust)
h_cluster <- cutree(clust, 3)
rect.hclust(clust, k=3, border="red")
table(h_cluster)
hclust_summary <- aggregate(std_seg_data[,c("Trendy", "Styling", "Reliability", "Sportiness", "Performance", "Comfort")],by=list(h_cluster),FUN=mean)
hclust_summary
h_cluster <- factor(h_cluster,levels = c(1,2,3),
                    labels = c("Perf.", "Comfort", "Appearance"))
plot(cut(as.dendrogram(clust), h=9)$lower[[3]])

set.seed(1990)
NbClust(data=std_seg_data[,1:5], min.nc=3, max.nc=15, index="all", method="ward.D2")

CrossTable(seg_data$MBA,h_cluster,prop.chisq = FALSE, prop.r = T, prop.c = T,
           prop.t = F,chisq = T)
CrossTable(h_cluster,seg_data$Choice,prop.chisq = FALSE, prop.r = T, prop.c = T,
           prop.t = F,chisq = T)


            # K-MEANS
set.seed(1990)
car_Cluster3 <-kmeans(std_seg_data, 3, iter.max=100,nstart=100)
car_Cluster3

Kmean_Cluster<-factor(car_Cluster3$cluster,levels = c(1,2,3),
                      labels = c("Perf. KM", "Comfort KM", "Appearance KM"))
       # FIND THE OPTIMAL NUMBER OF CLUSTERS
set.seed(1990)
NbClust(data=std_seg_data[,1:5], min.nc=3, max.nc=15, index="all", method="kmeans")

CrossTable(seg_data$MBA,Kmean_Cluster,prop.chisq = FALSE, prop.r = T, prop.c = T,
           prop.t = F,chisq = T)

CrossTable(Kmean_Cluster,seg_data$Choice,prop.chisq = FALSE, prop.r = T, prop.c = T,prop.t = F,chisq = T)

CrossTable(h_cluster,Kmean_Cluster,prop.chisq = FALSE, prop.r = T, prop.c = T,
           prop.t = F,chisq = T)

     # LATENT CLASS ANALYSIS
set.seed(1990)
mclustBIC(std_seg_data[,1:5],verbose=F)

set.seed(1990)
lca_clust <- Mclust(std_seg_data[,1:5],verbose = FALSE)
summary(lca_clust)

lca_clusters <- lca_clust$classification
lca_clust_summary <- aggregate(std_seg_data[,c("Trendy", "Styling", "Reliability", "Sportiness", "Performance", "Comfort")],by=list(lca_clusters),FUN=mean)
lca_clust_summary
lca_clusters<-factor(lca_clusters,levels = c(1,2),
                     labels = c("Reliability LCA", "Comfort LCA"))

CrossTable(seg_data$MBA,lca_clusters,prop.chisq = FALSE, prop.r = T, prop.c = T,
          prop.t = F,chisq = T)
CrossTable(lca_clusters,seg_data$Choice,prop.chisq = FALSE, prop.r = T, prop.c = T,
           prop.t = F,chisq = T)
CrossTable(h_cluster,lca_clusters,prop.chisq = FALSE, prop.r = T, prop.c = T,
           prop.t = F,chisq = T)
CrossTable(Kmean_Cluster,lca_clusters,prop.chisq = FALSE, prop.r = T, prop.c = T,
           prop.t = F,chisq = T)