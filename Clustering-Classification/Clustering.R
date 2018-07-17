#http://inseaddataanalytics.github.io/INSEADAnalytics/groupprojects/January2017/MashableNews.html


library(fpc)
library(data.table)
library(ggplot2)
library(caret)
library(MASS)
###################### Clustering ######################
# clustering : remove target variable 
table(dataset$popularity)


###### split the dataset ##########
# inTrain = createDataPartition(dataset$popularity, p = 1/2, list = FALSE)
# Train=dataset[inTrain,]
# Test=dataset[-inTrain,]
# 
# summary(dataset$popularity)
# dim(Train)
# summary(Train$popularity)
# dim(Test)
# summary(Test$popularity)
# 
# Train_popularity_col = Train$popularity
# Train_log_shares_col= Train$log_shares
# Train$popularity <- NULL 
# Train$log_shares<- NULL
# 
# Test_popularity_col = Test$popularity
# Test_log_shares_col= Test$log_shares
# Test$popularity <- NULL
# Test$log_shares <- NULL


popularity = dataset$popularity
log_shares= dataset$log_shares
shares=dataset$shares
dataset$popularity <- NULL
dataset$log_shares <- NULL
dataset$shares <- NULL


# 
# 
# Test_shares_col= Test$shares
# Train_shares_col= Train$shares
# Train$shares <- NULL 
# Test$shares <- NULL



#### SCALING #######
#dataset_scaled=dataset
dataset_scaled <- scale(dataset, center = TRUE, scale= TRUE)
head(dataset_scaled)



#mycols <- c("global_subjectivity", "global_sentiment_polarity" ,  "global_rate_positive_words", "global_rate_negative_words", "avg_positive_polarity", "avg_negative_polarity", "abs_title_subjectivity" ,  "abs_title_sentiment_polarity" )

dim(dataset_scaled)
#new <- dataset[mycols]

#dataset_scaled=new

#Hierarchical Clustering
# d <- dist(Train,method = "euclidean") #distance matrix
# h_clust <- hclust(d, method = "ward") #clustering
# plot(h_clust) #dendrogram #,labels = dataset$urls
# 
# rect.hclust(h_clust,k=4)
# 
# #extract clusters
# groups <- cutree(h_clust,k=4)
# groups


# #pca
# pcmp <- princomp(Train_scaled)
# pred_pc <- predict(pcmp, newdata=Train_scaled)[,1:2]
# 
#comp_dt <- cbind(as.data.table(pred_pc),cluster = as.factor(groups)) #, Labels = water_data$V1
# 
# ggplot(comp_dt,aes(Comp.1,Comp.2))+
#   geom_point(aes(color = cluster),size=3)

#kmeans
fit <- kmeans(dataset_scaled,centers = 2,iter.max = 1000)

 #install.packages("factoextra")
 library("factoextra")
 fviz_cluster(fit, data = dataset_scaled)
 
 fit
 
 
 
 # Train_scaled %>%
 #   as_tibble() %>%
 #    mutate(cluster = fit$cluster,
 #           state = row.names(Train_scaled)) %>%
 #   ggplot(aes(global_rate_positive_words, self_reference_max_shares, 
 #              color = factor(cluster) , label = state)) +
 #   geom_text()


 
 ### 
 # get cluster means
 aggregate(dataset_scaled,by=list(fit$cluster),FUN=mean)
 
 # append cluster assignment
 dataset_withCluster <- data.frame(dataset_scaled, fit$cluster) 
 head(dataset_withCluster)

 dataset_withCluster$fit.cluster<- as.factor(dataset_withCluster$fit)
summary(dataset_withCluster$fit.cluster)

head(dataset_withCluster) 
dataset_withCluster$popularity <- popularity 

head(dataset_withCluster)




cluster1df <- dataset_withCluster[ which(dataset_withCluster$fit.cluster==1), ]
cluster2df <- dataset_withCluster[ which(dataset_withCluster$fit.cluster==2), ]

# summary(dataset_withCluster$popularity[dataset_withCluster$fit.cluster==1])
# summary(dataset_withCluster$popularity[dataset_withCluster$fit.cluster==2])


table(fit$cluster, dataset_withCluster$popularity)
# unpopular normal popular
# 1     22013      0    4535
# 2     11772      0    1324

#mean 


# vary parameters for most readable graph
library(cluster)
clusplot(dataset_scaled, fit$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(dataset_scaled, fit$cluster) 


fit$centers

c1<-colMeans(dataset[fit$cluster==1,])
c2<-colMeans(dataset[fit$cluster==2,])



comb<-cbind(c1,c2)
comb
