# **************************************************
# Support Vector Machines Tutorial - Part 2   ######
# **************************************************

# ---- Step 1: Load required libraries, packages ----
install.packages("ISLR", dependencies = TRUE)

# set pseudo-random number generator
set.seed(10) # explicitly set the seed to allow for reproducible results

# Attach Packages
library(tidyverse)    # data manipulation and visualization
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
library(ISLR)         # contains example data set "Khan"
library(RColorBrewer) # customized coloring of plots


# ---- Step 2: Maximal Marginal Classifier ----
# Construct sample data set - completely separated

# create a 2 column matrix with 20 observation for x an
x <- matrix(rnorm(20*2), ncol = 2) 
y <- c(rep(-1,10), rep(1,10)) # replicates the value in X 

x[y==1,] <- x[y==1,] + 3/2 # where y equal to 1 add 3/2

dat <- data.frame(x=x, y=as.factor(y)) # factorise the variable dat
print(dat) # print the dataframe


# ---- Step 3: Plot the dataset ----
# Plot data
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none") 

#The command is ggplot(data = "name of dataset or vector", 
#aes is called aesthetic mapping, so we map on the x axis the values of x.1 
#and values of x.2 on the y axis. 

#The geom_point sets the size of the points and 
#scale_color_manual is the hex color codes


# ---- Step 4: Support Vector Machine implementation ----
#Fit Support Vector Machine model to data set

# svm(y~.) y is the predictor value and "." is the remaining classes. 
#In this case the x factor. Kernel tells the SVM function to create a linear
#boundary and scale is set to False to stop the setting of output to scaled 
#values, i.e. 0-1.
svmfit <- svm(y~., data = dat, kernel = "linear", scale = FALSE) 
# Plot Results
plot(svmfit, dat) # just a standard plot command to plot svmfit and data


# ---- Step 5: Use kernLab SVM package ----
# fit model and produce plot
kernfit <- ksvm(x, y, type = "C-svc", kernel = 'vanilladot')
plot(kernfit, data = x)


# ---- Step 6: Now move on to the next type of SVM the SVM Classifier ----
# Construct sample data set - not completely separated
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1 # elements where y=1
dat <- data.frame(x=x, y=as.factor(y))

# Plot data set
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")


# ---- Step 7: Use SVM() from e1071 package ----
# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 10) 
# predictor variable is y, Kernel says that we are using linear regression 
# and cost of 10. Cost is how we try to limit misclassifications. 

#If your Cost is large, the SVM will try to find a hyperplane and margin so that
#there are few very points within the margin, which could mean an overly complex
#model with a small margin if the points aren't easily separable.

#A lower Cost gives higher error on the training set, but finds a larger margin
#that might be more robust.

# Plot Results
plot(svmfit, dat)


# ---- Step 8: Let's increase the COST from 10 to 100 and repeat ----
# Fit Support Vector Machine model to data set with cost = 100
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 100)

# Plot Results
plot(svmfit, dat)


# ---- Step 9: Let's use the kernLab SVM algorithm to compare ----
# Fit Support Vector Machine model to data set
kernfit <- ksvm(x,y, type = "C-svc", kernel = 'vanilladot', C = 100)

# Plot results
plot(kernfit, data = x)
#This time we have added the parameter for Cost. It is labelled as C and
#is set to 100.


# ---- Step 10: Tune the algorithm to get the best value for Cost ----
# find optimal cost of misclassification
# using a grid search heuristic algorithm - (tune)
tune.out <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
#We set the range parameter to a list of costs from 0.001 up to 100

# output values
tune.out

# extract the best model
bestmod <- tune.out$best.model


# ---- Step 11: Create a predictive model ----
# Create a table of misclassified observations
ypred <- predict(bestmod, dat)
misclass <- table(predict = ypred, truth = dat$y)
misclass

#bestmod is the value we got of 0.1 for cost
#Sets the positive as the y value

# ---- Now let's move further on and into full Support Vector Machines ----
#Let's demonstrate a non-linear boundary by again creating a dummy dataset

# construct larger random data set
x <- matrix(rnorm(200*2), ncol = 2) # create 400 X observations in 2 columns
x[1:100,] <- x[1:100,] + 2.5 # where X in range 1 - 100 add 2.5
x[101:150,] <- x[101:150,] - 2.5 # where X in range 101 - 150 subtract 2.5
y <- c(rep(1,150), rep(2,50)) # replicate 
dat <- data.frame(x=x, y=as.factor(y)) # create a dataframe 

dat # print dataframe dat

# Plot data
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")


# ---- Step 12: Use Radial Kernel ----
# set pseudorandom number generator
set.seed(123) # set the seed value so we can reproduce the results

# sample training data and fit model
train <- base::sample(200, 100, replace = FALSE) # select a random

# sample of 100 observations without replacement
# set the kernel parameter to "radial" and a gamma value of 1 with a cost of 1
svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)

#Gama value the number of data dimensions and is a required parameter for all
#kernels except linear.

# plot classifier
plot(svmfit, dat)

#Let's try different values of gamma and cost. Set gamma to 3 and cost to 5
# set pseudorandom number generator
set.seed(123) # set the seed value so we can reproduce the results

# sample training data and fit model
train <- base::sample(200,100, replace = FALSE) # select a random

# sample of 100 observations without replacement
#set the kernel parameter to "radial" and a gamma value of 3 with a cost of 5
svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 3, cost = 5)

#Gama value the number of data dimensions and is a required parameter for all 
#kernels except linear.

# plot classifier
plot(svmfit, dat)


# ---- Step 13: Use the kernLab package on the same dataset ----
# Fit radial-based SVM in kernlab
kernfit <- ksvm(x[train,],y[train], type = "C-svc", kernel = 'rbfdot', C = 1, 
                scaled = c())
# Plot training data
plot(kernfit, data = x[train,])

# tune model to find optimal cost, gamma values
tune.out <- tune(svm, y~., data = dat[train,], kernel = "radial",
                 ranges = list(cost = c(0.1,1,10,100,1000),
                 gamma = c(0.5,1,2,3,4)))
#We set a range for cost and also for gamma this time around to find the best model
#to use

# show best model
tune.out$best.model

# validate model performance
(valid <- table(true = dat[-train,"y"], pred = predict(tune.out$best.model,
                                             newx = dat[-train,])))


# ---- Step 14: Now for SVMs for Multiple Classes using e1071 ----
# construct data set
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0,50))
x[y==0,2] <- x[y==0,2] + 2.5
dat <- data.frame(x=x, y=as.factor(y))

# plot data set
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000","#FF0000","#00BA00")) +
  theme(legend.position = "none")

# fit model
svmfit <- svm(y~., data = dat, kernel = "radial", cost = 10, gamma = 1)

# plot results
plot(svmfit, dat)

#construct table
ypred <- predict(svmfit, dat)
misclass <- table(predict = ypred, truth = dat$y)


# ---- Step 15: Now use the kernLab package --- 
# fit and plot
kernfit <- ksvm(as.matrix(dat[,2:1]),dat$y, type = "C-svc", kernel = 'rbfdot', 
                C = 100, scaled = c())

# Create a fine grid of the feature space
# generate a regular sequence
x.1 <- seq(from = min(dat$x.1), to = max(dat$x.1), length = 100) 
x.2 <- seq(from = min(dat$x.2), to = max(dat$x.2), length = 100)

#Create a data frame from all combinations of the supplied vectors or factors. 
x.grid <- expand.grid(x.2, x.1) 

# Get class predictions over grid
pred <- predict(kernfit, newdata = x.grid)

# Plot the results
cols <- brewer.pal(3, "Set1") #create nice looking colour pallettes
#Set1 is the brewer pal pallette for ordered data that progresses from
#low to high values
plot(x.grid, pch = 19, col = adjustcolor(cols[pred], alpha.f = 0.05))

classes <- matrix(pred, nrow = 100, ncol = 100)
contour(x = x.2, y = x.1, z = classes, levels = 1:3, labels = "", add = TRUE)
#Contour creates a contour map
points(dat[, 2:1], pch = 19, col = cols[predict(kernfit)])
