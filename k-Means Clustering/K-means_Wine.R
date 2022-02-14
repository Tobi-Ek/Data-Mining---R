# ***********************************************
# Wine DATA: k-means algorithm      #############
# ***********************************************

# install.packages("rattle")
library(rattle)  # Package containing wine dataset
library(cluster)   #clustering algorithms
library(factoextra) #clustering algorithms and visualizations
library(NbClust) # Package for determining the best number of clusters

data(wine)

# ---- Exploring the data ----
head(wine)
summary(wine)

# ---- Transformation ----
# z-score standardization
wine.new <- scale(wine[-1])                                        

# ----- Determine number of clusters [Option 1] -----
# Define function
wssplot <- function(data, nc=15){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="l", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(wine.new)  

# ----- Determine number of clusters [Option 2] -----
nCluster <- NbClust(wine.new, min.nc=2, max.nc=15, method="kmeans")

table(nCluster$Best.nc[1,])

barplot(table(nCluster$Best.nc[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Best Number of Clusters")


# ----- k-means algorithm -----
set.seed(1234)
km_result <- kmeans(wine.new, 3, nstart=2)                          
km_result


# ---- Visualising Cluster Plot ----
fviz_cluster(km_result, data = wine.new, geom = "point")

clusplot(wine.new, km_result$cluster, color=T, shade=T, labels= 4)

