# ***************************************************
# Old Faithful Geyser DATA: k-means     #############
# ***************************************************

library("datasets")
data("faithful")

# ---- Exploring the data ----
str(faithful)
plot(faithful)

faithful.new <- faithful
summary(faithful.new)

# ---- Transformation ----
# min-max Normalization
normalise <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

# Normalise the columns
faithful.new$eruptions <- normalise(faithful.new$eruptions)
faithful.new$waiting <- normalise(faithful.new$waiting)

# ---- k-means algorithm ----
result <- kmeans(faithful.new, 2, iter.max = 10)
result

# Components
result$withinss
result$betweenss
result$tot.withinss
result$cluster 

# ---- Cluster Plot ----
plot(faithful.new, col= result$cluster, pch = result$cluster)
