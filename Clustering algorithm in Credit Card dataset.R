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
##elbow plot
library(purrr)

tot_withinss<-map_dbl(1:20,function(k){
  model=kmeans(x=credit_scaled,centers=k)
  model$tot.withinss
})
elbow_credit<-data.frame(
  k=1:20,
  tot_withinss<-tot_withinss
)

elbow_credit

library(ggplot2)
ggplot(elbow_credit,aes(x=k,y=tot_withinss))+geom_line()+scale_x_continuous(breaks=1:20)
#There are 14 clusters in K mean 

##fit the model
credit_final_model<-kmeans(credit_scaled,centers=7)
credit_final_model

#######################################################################################################################