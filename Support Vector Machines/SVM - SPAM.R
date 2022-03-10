# **************************************************
# Comparing DT, LogR and SVM - SPAM Dataset   ######
# **************************************************

# ---- Import Libraries/Load Data ----
set.seed(1)

library(kernlab)
library(rpart)
library(nnet)
library(caret)
# Load spam data
data(spam)

# ---- Split data into training & test set ----
N <- nrow(spam)
indtrain <- sample(1:N,size = N/2)
indtrain <- sort(indtrain)
indtest  <- setdiff(1:N,indtrain)

# ---- Run a Classification Tree Model ----
fitc <- rpart(type~., data=spam[indtrain,])

# ---- Run a Logistic Regression Model ----
fitl <- multinom(type~., data=spam[indtrain,])

# ---- Run a Kernel SVM Model ----
fitsvm <- ksvm(type~., data=spam[indtrain,])


# ---- Compare Results ----
predc   <- predict(fitc, newdata=spam, type="class")
predl   <- predict(fitl, newdata=spam, type="class")
predsvm <- predict(fitsvm,spam)

table(spam$type[indtest], predc[indtest])
table(spam$type[indtest], predl[indtest])
table(spam$type[indtest], predsvm[indtest])

# ---- Evaluation ----
confusionMatrix(spam$type[indtest],predc[indtest])
confusionMatrix(spam$type[indtest],predl[indtest])
confusionMatrix(spam$type[indtest],predsvm[indtest])

