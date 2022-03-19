# ****************************************************
# IRIS Dataset: Decision Trees                ########
# ****************************************************

# C5.0 Algorithm
library(C50)

# ---- Step 1: Import the data ----

data(iris)
new.iris <- ceiling(0.1*nrow(iris))
new.iris

# ---- Step 2: Split the data ----

set.seed(3)
test.index <- sample(1:nrow(iris), new.iris)
iris.test <- iris[test.index, ]
iris.train <- iris[-test.index, ]

# ---- Step 3: C.50 Algorithm ----

c <- C5.0Control(subset = FALSE,
                 bands = 0,
                 winnow = FALSE,
                 noGlobalPruning = FALSE,
                 CF = 0.25,
                 minCases = 2,
                 fuzzyThreshold = FALSE,
                 sample = 0,
                 seed = sample.int(4096, size = 1) -1L,
                 earlyStopping = TRUE )

# ---- Step 4: Decision Tree ----

iris_treeModel <- C5.0(x = iris.train[, -5], y = iris.train$Species, control =c)
summary(iris_treeModel)

plot(iris_treeModel)

# ---- Step 5: Predict using DT Model ----
test.output <- predict(iris_treeModel, iris.test[, -5], type = "class")
n <- length(test.output)
number = 0
for ( i in 1:n){
  if(test.output[i] == iris.test[i, 5])
  {
    number=number+1}
}
test.accuracy <- number/n*100
test.accuracy
