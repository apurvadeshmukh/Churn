###CUSTOMER SEGMENTATION- using clustering 


datacluster <- data6[,c(2,4,5)]



####Hierarchical clustering

hclusters <- hclust(dist(datacluster[, 2:3]))
plot(hclusters)

clusterCut <- cutree(hclusters, 5)


#####K means

set.seed(20)
kCluster <- kmeans(datacluster[, 1:3], 3, nstart = 20)
kCluster

####Plot 
plot(datacluster,col=kCluster$cluster)
points(kCluster$center,col=1:2,pch=8,cex=1)
