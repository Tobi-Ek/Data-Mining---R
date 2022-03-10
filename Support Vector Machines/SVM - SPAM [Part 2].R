# ********************************************
# SVM - SPAM dataset [Part 2]           ######
# ********************************************

# ---- Import Libraries/Load Data ----
library(kernlab)
library(caret)
# Load spam data
data(spam)


# ---- Explore Data ----
str(spam)
table(spam$type)

## oversampling or under sampling (SMOTE) for imbalanced data
summary(spam)
# Skip the pre-processing because our data seems fine


# ---- Train and Test set ----
N <- nrow(spam)
set.seed(123)
indtrain <- sample(1:N,size=N/2) # Index splitting
indtrain <- sort(indtrain)
indtest <- setdiff(1:N,indtrain)


# ---- Data Mining ----
fitsvm <- ksvm(type~., data=spam[indtrain,])
predsvm <- predict(fitsvm,spam)
?ksvm

# ---- Evaluation ----
confusionMatrix(spam$type[indtest],predsvm[indtest])


# ---- Data Mining using linear kernel ----
fitsvm1 <- ksvm(type~., data=spam[indtrain,], kernel="vanilladot")
predsvm1 <- predict(fitsvm1,spam)
?ksvm

# ---- Evaluation ----
confusionMatrix(spam$type[indtest],predsvm1[indtest])

# ---- F1 score: without kernel ----
precision <- posPredValue(predsvm[indtest],spam$type[indtest],positive = "nonspam")
recall <- sensitivity(predsvm[indtest],spam$type[indtest],positive = "nonspam")
F1 <- (2*precision*recall)/(precision+recall)
F1

# ---- F1 score: with kernel ----
precisionk <- posPredValue(predsvm1[indtest],spam$type[indtest],positive = "nonspam")
recallk <- sensitivity(predsvm1[indtest],spam$type[indtest],positive = "nonspam")
F1k <- (2*precisionk*recallk)/(precisionk+recallk)
F1k



