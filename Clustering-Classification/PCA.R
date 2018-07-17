

library(fpc)
library(data.table)
library(ggplot2)
library(caret)
library(MASS)
library(rpart)
library(rpart.plot)
###################### Clustering ######################
# clustering : remove target variable 


# # Proceed with principal components
# pc <- princomp(dataset_scaled)
# plot(pc)
# plot(pc, type='l')
# summary(pc) # 4 components is both 'elbow' and explains >85% variance


# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(dataset_scaled)
plot(pc)
# First four principal components
comp <- data.frame(pc$x[,1:4])
# Plot
#plot(comp, pch=16, col=rgb(0,0,0,0.5))


# Determine number of clusters
#wss <- (nrow(dataset_scaled)-1)*sum(apply(dataset_scaled,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(dataset_scaled,
#                                      centers=i)$withinss)
# 
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")

# From screen plot elbow occurs at k = 6
# Apply k-means with k=6
fit <- kmeans(comp, 2, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
#plot(comp, col=fit$clust, pch=16)
fit

fviz_cluster(fit, data = dataset_scaled)



dim(dataset_scaled)
dim(dataset)
# append cluster assignment
dataset_withCluster <- data.frame(dataset, fit$cluster) 
head(dataset_withCluster)

dataset_withCluster$fit.cluster<- as.factor(dataset_withCluster$fit.cluster)
summary(dataset_withCluster$fit.cluster)

head(dataset_withCluster) 
#dataset_withCluster$popularity <- popularity 
#table(fit$cluster, dataset_withCluster$popularity)
fit


c1<-colMeans(dataset[fit$cluster==1,])
c2<-colMeans(dataset[fit$cluster==2,])
c3<-colMeans(dataset[fit$cluster==3,])
c4<-colMeans(dataset[fit$cluster==4,])
c5<-colMeans(dataset[fit$cluster==5,])
c6<-colMeans(dataset[fit$cluster==6,])
comb<-cbind(c1,c2,c3,c4,c5,c6)
comb


colnames(dataset_withCluster)[which(names(dataset_withCluster) == "fit.cluster")] <- "cluster"
########### build a decision tree for the 2 clusters 
DT_c12 <- rpart(cluster~ ., method="class", data=dataset_withCluster, control = rpart.control(cp = 0, maxdepth=4))

plot(DT_c12, uniform=TRUE, main="")
text(DT_c12, cex = 1, use.n = TRUE, xpd = TRUE)

levels(dataset_withCluster$data_channel_is_world)


################### extracting clusters 

cluster1df <- dataset_withCluster[ which(dataset_withCluster$cluster==1), ]
cluster2df <- dataset_withCluster[ which(dataset_withCluster$cluster==2), ]


dim(cluster1df)
dim(cluster2df)

# build a decision tree for cluster 1 
DT_cluster1 <- rpart(cluster~ ., method="class", data=cluster1df, control = rpart.control(cp = 0, maxdepth=4))

plot(DT_cluster1, uniform=TRUE, main="")
text(DT_cluster1, cex = 1, use.n = TRUE, xpd = TRUE)



# build a decision tree for cluster 2 

DT_cluster2 <- rpart(popularity~ ., method="class", data=cluster2df, control = rpart.control(cp = 0, maxdepth = 4))

plot(DT_cluster2, uniform=TRUE, main="")
text(DT_cluster2, cex = 1, use.n = TRUE, xpd = TRUE)

