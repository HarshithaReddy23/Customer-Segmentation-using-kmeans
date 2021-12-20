customer_data=read.csv("/Mall_Customers.csv")

cust.sc<-scale(customer_data[,c(4,5)])
#Finding best K for K mean using Elbow Method:
wss <- function(data, maxCluster = 10) {
  # Initialize within sum of squares
  SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i] <- sum(kmeans(data, centers = i)$withinss) 
  }
  plot(1:maxCluster, SSw, type = "o", xlab = "Number of Clusters", ylab ="Within groups sum of squares", pch=19)
}
set.seed(100)
wss(cust.sc)


# loading required packages
library(factoextra)
library(NbClust)
# Elbow method using fviz_nbclust
set.seed(123)
fviz_nbclust(cust.sc, kmeans, method = "wss")



#Average Silhouette Method
library(cluster) 
library(gridExtra)
library(grid)


k2<-kmeans(customer_data[,3:5],2,iter.max=100,nstart=50,algorithm="Lloyd")
s2<-plot(silhouette(k2$cluster,dist(customer_data[,3:5],"euclidean")),col="#3683ff")

k3<-kmeans(customer_data[,3:5],3,iter.max=100,nstart=50,algorithm="Lloyd")
s3<-plot(silhouette(k3$cluster,dist(customer_data[,3:5],"euclidean")),col="#3683ff")

k4<-kmeans(customer_data[,3:5],4,iter.max=100,nstart=50,algorithm="Lloyd")
s4<-plot(silhouette(k4$cluster,dist(customer_data[,3:5],"euclidean")),col="#3683ff")

k5<-kmeans(customer_data[,3:5],5,iter.max=100,nstart=50,algorithm="Lloyd")
s5<-plot(silhouette(k5$cluster,dist(customer_data[,3:5],"euclidean")),col="#3683ff")

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
s6<-plot(silhouette(k6$cluster,dist(customer_data[,3:5],"euclidean")),col="#3683ff")

k7<-kmeans(customer_data[,3:5],7,iter.max=100,nstart=50,algorithm="Lloyd")
s7<-plot(silhouette(k7$cluster,dist(customer_data[,3:5],"euclidean")),col="#3683ff")

k8<-kmeans(customer_data[,3:5],8,iter.max=100,nstart=50,algorithm="Lloyd")
s8<-plot(silhouette(k8$cluster,dist(customer_data[,3:5],"euclidean")),col="#3683ff")

k9<-kmeans(customer_data[,3:5],9,iter.max=100,nstart=50,algorithm="Lloyd")
s9<-plot(silhouette(k9$cluster,dist(customer_data[,3:5],"euclidean")),col="#3683ff")

k10<-kmeans(customer_data[,3:5],10,iter.max=100,nstart=50,algorithm="Lloyd")
s10<-plot(silhouette(k10$cluster,dist(customer_data[,3:5],"euclidean")),col="#3683ff")


library(factoextra)
library(NbClust)
fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")



# Gap Static Method
library(factoextra)
library(NbClust)
library(FunCluster)
set.seed(123)
stat_gap <- clusGap (customer_data[,3:5], FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(stat_gap, method = "firstmax")
fviz_gap_stat(stat_gap)
