# ******************************************************
# Support Vector Machines - Basic Implementation   #####
# ******************************************************

# Source: https://www.datacamp.com/community/tutorials/support-vector-machines-r

# ---- Import Libraries/Load Data ----
library(e1071)
library(mda)

# ---- Linear SVM Classifier ----

set.seed(10111)
x <- matrix(rnorm(40), 20, 2)
y <- rep(c(-1, 1), c(10, 10))
x[y == 1,] <- x[y == 1,] + 1

# plot
plot(x, col = y + 3, pch = 19)

# SVM Model
my_data <- data.frame(x, y = as.factor(y))
svmfit  <- svm(y ~ ., data = my_data, kernel = "linear", cost = 10, scale = FALSE)

# Summary of SVM Model
print(svmfit)

plot(svmfit, my_data)

# Create a grid of values or a lattice of values for x1 and x2 
# that covers the whole domain on a fairly fine lattice
make.grid <- function(x, n = 75) {
  grange <- apply(x, 2, range)
  x1 <- seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 <- seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

# Let's take a look at the first few values of the lattice from 1 to 10.
xgrid <- make.grid(x)
xgrid[1:10,]


# Include index (a component of svmfit) in the plot
ygrid <- predict(svmfit, xgrid)
plot(xgrid, col = c("red","blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)

# Get the linear coefficients for the svm function
beta <- drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0 <- svmfit$rho

# Decision Boundary
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2)
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2)


# ---- Non-Linear SVM Classifier ----
data(ESL.mixture)
names(ESL.mixture)

# remove x and y in the data [ESL.mixture]
rm(x, y)
attach(ESL.mixture)

# plot
plot(x, col = y + 1)

dat <- data.frame(y = factor(y), x)
fit <- svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)

xgrid <- expand.grid(X1 = px1, X2 = px2)
ygrid <- predict(fit, xgrid)

# Decision Boundary
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)

# plotting a non-linear decision boundary in 2 dimensions
func <- predict(fit, xgrid, decision.values = TRUE)
func <- attributes(func)$decision

xgrid <- expand.grid(X1 = px1, X2 = px2)
ygrid <- predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)

contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE)
contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2)