# ***********************************************
# US Arrests: k-means algorithm       ###########
# ***********************************************

# install.packages("cluster")
library(datasets)
library(cluster)   #clustering algorithms
library(factoextra) #clustering algorithms and visualizations
library(NbClust) # Package for determining the best number of clusters
library(tidyverse) #toolkit for data analysis

data("USArrests")
arrest <- USArrests

# ---- Exploring the data ----
str(arrest)
plot(arrest)
# checking for NAs
mapply(anyNA, arrest)


# ---- Transformation ----
# z-score standardization
arrest <- scale(arrest)

head(arrest)
summary(arrest)


# ----- Determine number of clusters -----
nCluster <- NbClust(arrest, min.nc=2, max.nc=15, method="kmeans")

table(nCluster$Best.nc[1,])

barplot(table(nCluster$Best.nc[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Best Number of Clusters")


# ---- k-means algorithm ----
k2 <- kmeans(arrest, 2, nstart = 2)
k2


# ---- Visualising cluster plots ----
fviz_cluster(k2, data = arrest)
fviz_cluster(k2, data = arrest, geom = "point")


# Visualising using data (UrbanPop and Rape)
# tibble 
arrest %>%
  as_tibble() %>%
  mutate(cluster=k2$cluster, state=row.names(USArrests)) %>%
  ggplot(aes(UrbanPop,Rape,color=factor(cluster),label=state)) + geom_text()


# comparing the different results, different k values
k3 <- kmeans(arrest, centers = 3, nstart = 25)
k4 <- kmeans(arrest, centers = 4, nstart = 25)
k5 <- kmeans(arrest, centers = 5, nstart = 25)

# plots to compare kmeans
p1 <- fviz_cluster(k2, geom = "point", data= arrest) + ggtitle("k=2")
p2 <- fviz_cluster(k3, geom = "point", data= arrest) + ggtitle("k=3")
p3 <- fviz_cluster(k4, geom = "point", data= arrest) + ggtitle("k=4")
p4 <- fviz_cluster(k5, geom = "point", data= arrest) + ggtitle("k=5")

# Grid the cluster plots 
library(gridExtra)

grid.arrange(p1,p2,p3,p4,nrow=2)


# --- Calculate distance matrix ---
?get_dist
# get_dist(): Computes a distance matrix between the rows of a data matrix. 
# fviz_dist(): Visualizes a distance matrix
distance <- get_dist(arrest)
fviz_dist(distance, gradient = list(low ="#00AFBB", mid ="white", high="#FC4E07"))
