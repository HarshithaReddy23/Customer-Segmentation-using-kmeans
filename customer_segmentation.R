#Customer Segmentation for Shopping Mall
#importing customer data set
customer_data=read.csv("/Mall_Customers.csv")

dim(customer_data)
str(customer_data)
names(customer_data)

#displaying data set values
head(customer_data)
tail(customer_data)

summary(customer_data)

# caluculating standard deviation 
sd(customer_data$Age)
sd(customer_data$Annual.Income..k..)
sd(customer_data$Spending.Score..1.100.)


#visualization of age distribution

hist(customer_data$Age,
     col="green",
     main="Histogram that Show's Count of Age Class",
     ylim = c(0,40),
     xlab="Age",
     ylab="Frequency",
     labels=TRUE,
     breaks = 10,
     las=1 )


#boxplot
boxplot(customer_data$Age,
        col="red",
        ylab='Age',
        main="Boxplot for Descriptive Analysis of Age")

#gender visualization
a=table(customer_data$Genre)
a
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))

pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%")
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")


#creating a visualizations to analyze the annual income of the customers. 
summary(customer_data$Annual.Income..k..)

#ploting a histogram and then proceed to examine this data using a density 
hist(customer_data$Annual.Income..k..,
     col="#0867cc",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE,
     ylim=c(0, 40),
     las=1)


#density
plot(density(customer_data$Annual.Income..k..),
     col="red",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density",lwd=4)
polygon(density(customer_data$Annual.Income..k..),
        col="#257b8a")


#analyzing spending score of the customer
summary(customer_data$Spending.Score..1.100.)
#boxplot
boxplot(customer_data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="#7e42f5",
        main="BoxPlot for Descriptive Analysis of Spending Score")


#histogram
hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="#8103ab",
     ylim=c(0,45),
     labels=TRUE,las=1)


#optimal Clusters
cust.sc<-scale(customer_data[,c(4,5)])
#Finding best K for K mean using Elbow Method:
#calculating WSS (with in cluster sum of square)
wss <- function(data, maxCluster = 10) {
  # Initialize within sum of squares
  SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i] <- sum(kmeans(data, centers = i)$withinss) 
  }
  plot(1:maxCluster, SSw, type = "o", xlab = "Number of Clusters", ylab ="Within groups sum of squares", pch=19)
}

wss(cust.sc)


# loading required packages
library(factoextra)
library(NbClust)
# Elbow method using fviz_nbclust
set.seed(23)
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


#kmeans

library(cluster) 
library(gridExtra)
library(grid)
library(ggplot2)

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6
#cluster - This is a vector of several integers that denote the cluster which has an allocation of each point.
#totss - This represents the total sum of squares.
#centers - Matrix comprising of several cluster centers
#withinss - This is a vector representing the intra-cluster sum of squares having one component per cluster.
#tot.withinss - This denotes the total intra-cluster sum of squares.
#betweenss - This is the sum of between-cluster squares.
#size - The total number of points that each cluster holds.


#Visualizing the Clustering Results using the First Two Principle Components Code: 
pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",breaks=c("1", "2", "3", "4", "5","6"),labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")


#From the above visualization, we observe that there is a distribution of 6 clusters as follows
#Cluster 1 and 5 - These clusters represent the customer_data with the medium income salary as well as the medium annual spend of salary.
#Cluster 4 - This cluster represents the customer_data having a high annual income as well as a high annual spend.
#Cluster 2 - This cluster denotes the customer_data with low annual income as well as low yearly spend of income.
#Cluster 3 - This cluster denotes a high annual income and low yearly spend.
#Cluster 6 - This cluster represents a low annual income but its high yearly expenditure.

kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means PCA1",ylab="PCA2")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))

#Cluster 5 and 1 - These two clusters consist of customers with medium PCA1 and medium PCA2 score.
#Cluster 4 - This cluster represents customers having a high PCA2 and a low PCA1.
#Cluster 3 - In this cluster, there are customers with a medium PCA1 and a low PCA2 score.
#Cluster 2 - This cluster comprises of customers with a high PCA1 income and a high PCA2.
#Cluster 6 - This comprises of customers with a high PCA2 and a medium annual spend of income.