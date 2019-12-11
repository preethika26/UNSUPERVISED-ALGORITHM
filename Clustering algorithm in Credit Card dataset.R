##CLUSTERING USING CREDIT CARD(CC) DATASET


##LOAD THE DATA
credit<-read.csv(file.choose())
summary(credit)

##PREPROCESSING
library(Hmisc)
credit$CREDIT_LIMIT<-impute(credit$CREDIT_LIMIT,median)
credit$MINIMUM_PAYMENTS<-impute(credit$MINIMUM_PAYMENTS,median)

##check any missing values,even after replacing
any(is.na(credit))


##remove the creditid column from the data
credit<-credit[,-1]

#######################################################################################################################

##BUILDING THE HIERARCHICAL CLUSTERING MODEL

##scale the data
credit_scaled<-scale(credit)

##find the euclident distance
credit_dist<-dist(credit_scaled,method="euclidean")

##build hierarchical clustering
credit_hc<-hclust(credit_dist,method="complete")

##create dendogram object from hclust variable
credit_dend<-as.dendrogram((credit_hc))

##plot dendogram
plot(credit_dend)

##create cluster
credit_cut<-cutree(credit_hc,h=40)
credit_cut

##generate the segmented the credit data frame
library(dplyr)
credit_clust<-mutate(credit,credit_cut)
credit_clust
count(credit_clust,credit_cut)
#There are 3 cluster in hierarchical clustering.

#######################################################################################################################

##BULIDIND K MEAN CLUSTERING MODEL

library(cluster)
library(factoextra)

##elbow plot
wssplot <- function(credit, nc=10, seed=1234){
  wss <- (nrow(credit)-1)*sum(apply(credit,2,var))
  for (i in 1:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(credit, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(credit, nc=10) 

##fit the model
k5<-kmeans(credit,centers=7,iter.max = 10, nstart = 1)

##ploting the cluster
fviz_cluster(k5, geom = "point",  data = credit) + ggtitle("k = 7")

##generate the segmented the credit data frame
data=cbind(credit,cluster=k5$cluster)

write.csv(data,"CC.csv")

#######################################################################################################################