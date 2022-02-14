# ***********************************************
# IRIS DATA: k-means algorithm        ###########
# ***********************************************

install.packages("cluster") 
library("datasets")
data("iris")

# ---- Exploring the data ----
str(iris)
summary(iris)
plot (iris)


# ---- Pre-processing ----
iris.new <- iris[,c(1,2,3,4)]   
iris.class <- iris[,"Species"]  

head(iris.new)
head(iris.class)

summary(iris.new)


# ---- Transformation ----
# min-max normalization
normalise <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

# Run this function to normalize all the columns
iris.new$Sepal.Length <- normalise(iris.new$Sepal.Length)
iris.new$Sepal.Width <- normalise(iris.new$Sepal.Width)
iris.new$Petal.Length <- normalise(iris.new$Petal.Length)
iris.new$Petal.Width <- normalise(iris.new$Petal.Width)


# ---- k-means algorithm ----
?kmeans
result <- kmeans(iris.new, 3, iter.max = 20, nstart = 2 )
result

# Check components
result$size #Number of points in each cluster

result$centers

result$cluster #Compare it with class labels


# ---- Visualising Cluster Plot ----
par(mfrow=c(2,2), mar=c(5,4,1,2)) #setup positioning of plots

# sepal length and sepal width cluster plot
plot(iris.new[c(1,2)], col= result$cluster)
plot(iris.new[c(1,2)], col= iris.class)

# petal length and petal width cluster plot
plot(iris.new[c(3,4)], col= result$cluster)
plot(iris.new[c(3,4)], col= iris.class)

# cross-tabulation
table(result$cluster, iris.class)
summary(iris)


# ------ Using the Cluster package ------
library(cluster)

# Using only petal length and width columns
plw <- iris[,3:4] 
head(plw)

plw$Petal.Length <- normalise(plw$Petal.Length)
plw$Petal.Width <- normalise(plw$Petal.Width)


# --- k-means algorithm ---
km_model <- kmeans(plw,3,nstart = 2)
km_model
par(mfcol=c(1,1))
# petal length and petal width cluster plot
clusplot(plw, km_model$cluster)
# petal length and petal width cluster plot
clusplot(plw, km_model$cluster, color=T, shade=T, labels= 4)

# cross-tabulation
table(km_model$cluster, iris.class)

