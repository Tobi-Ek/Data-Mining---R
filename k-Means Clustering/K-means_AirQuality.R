# ***********************************************
# Air Quality: k-means algorithm     ############
# ***********************************************

# install.packages("cluster")
library(datasets)
library(class)
library(cluster)   #clustering algorithms
library(factoextra) #clustering algorithms and visualizations
library(NbClust) # Package for determining the best number of clusters
library(tidyverse) #toolkit for data analysis


data("airquality")

# ---- Exploring the data ----
head(airquality)
str(airquality)

# checking for NAs
mapply(anyNA, airquality)

# Count missing NAs
sum(is.na(airquality))

# ---- Pre-processing ----
# removing extra attributes
airquality$Day <- NULL

head(airquality)

# airquality.new <- na.omit(airquality) # To omit missing values

# Fix NAs: Imputing using mean
# Example
imputedMean <- airquality$Ozone
mean <- mean(imputedMean, na.rm = T)
imputedMean <- ifelse(is.na(imputedMean),mean,imputedMean)
summary(imputedMean)
summary(airquality$Ozone)

# imputing using mean by Month
airquality.new <- airquality

for(i in 1:nrow(airquality.new)){
  if(is.na(airquality.new[i,"Ozone"])){
    airquality.new[i,"Ozone"]<- mean(airquality.new[which(airquality.new[,"Month"]==airquality.new[i,"Month"]),"Ozone"],na.rm = TRUE)
  }
  if(is.na(airquality.new[i,"Solar.R"])){
    airquality.new[i,"Solar.R"]<- mean(airquality.new[which(airquality.new[,"Month"]==airquality.new[i,"Month"]),"Solar.R"],na.rm = TRUE)
  }
}

summary(airquality.new)

# ---- Transformation ----
# min-max normalization
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

airquality.new$Ozone <- normalize(airquality.new$Ozone)
airquality.new$Solar.R <- normalize(airquality.new$Solar.R)
airquality.new$Wind <- normalize(airquality.new$Wind)
airquality.new$Temp <- normalize(airquality.new$Temp)

# --- remove month ---
airquality.new[,5]<- NULL
head(airquality.new)

# ---- k-means algorithm ----
result <- kmeans(airquality.new[c(1,2,3,4)],3)
result

# Check components
result$size

# ---- Visualising Cluster Plot ----
par(mfrow=c(1,2),mar=c(5,4,2,2))  #setup positioning of plots
plot(airquality.new[,1:2],col=result$cluster)
plot(airquality.new[,3:4],col=result$cluster)

plot(airquality.new[,],col=result$cluster)

fviz_cluster(result, data = airquality.new, geom = "point")

par(mfrow=c(1,1),mar=c(5,4,2,2))  #setup positioning of plots
clusplot(airquality.new, result$cluster, color=T, shade=T, labels= 4)

