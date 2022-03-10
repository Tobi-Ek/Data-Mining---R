# **************************************************
# Support Vector Machines Tutorial - Part 1   ######
# **************************************************


# ---- Step 1: Create a two dimensional dataset using x & y co-ordinates ----
#Create x & y values
x <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
y <- c(3,4,5,4,8,10,10,11,14,20,23,24,32,34,35,37,42,48,53,60)

#Combine x & y into a dataframe called train
#Create a data frame of the data
train <- data.frame(x,y)

#Inspect or view the dataframe "train"
View(train)

#Now let's plot the dataframe to see what our data looks like
# pch returns a symbol size between "." and "16". Think of it as font size
plot(train,pch=16) 


# -------Step 2: Use a linear regression algorithm on the train dataset ----
#Linear regression

# lm is the linear model function where y is the predictor variable
# and x is the target variable 
model <- lm(y ~ x, train) 
 
#Plot the model using abline
plot(model)

# abline is a function that adds one or more straight lines through the current point
abline(model) 


# ---- Step 3: Now we use the SVM Algorithm on the same dataset ----
#SVM
 
library(e1071) # load the library for e1071

#Fit a model. The function syntax is very similar to lm function
# svm is the support vector machine function in the e1071 package 
model_svm <- svm(y ~ x , train) 
 
#Use the predictions on the data
# We have created model_svm as the output of the SVM function.
# We now use the predict function against the model and train dataset
pred <- predict(model_svm, train) 
 
#Plot the predictions and the plot to see our model fit
plot(pred) 

# we colour the x point in the train dataset in blue with the col = "blue command"
# and set the size to "4"
points(train$x, pred, col = "blue", pch=4) 

#The same plot is displayed with colour of "green" and size of "8" below
plot(pred) 
points(train$x, pred, col = "green", pch=8)

