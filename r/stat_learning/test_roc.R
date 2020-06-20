# Create the data set and plot
set.seed(1)

# Create the data set and Split in training and test set
X <- matrix(rnorm(200 * 2), ncol = 2) # random sample matrix from a normal distribution with mean = 0 and standard deviation = 1
y <- c(rep(1, 100), rep(2, 100)) # two classes y = 1 and y = 2
X[y == 1, ] <- X[y == 1, ] + 1 # move apart the class with y = 1
train <- sample(200, 100) # random integers for index of train data
dat <- data.frame(x = X, y = as.factor(y))
plot(X, col = y, xlab = "X1", ylab = "X2")

library(e1071)
# Fit the training data using svm with radial kernel
#svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 2, cost = 1)
# Fit the training data using svm with  linear kernel
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "linear", cost = 6, scale = TRUE) 

plot(svmfit, dat[train, ])

# Function definition to Plot the ROC curves
library(ROCR)
rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, measure = "tpr", x.measure = "fpr") 
  plot(perf, col = "blue", colorize = TRUE, ...)
  auc <- performance(predob, measure = "auc")
  abline(a=0, b= 1)
  return(auc)
}

par(mfrow = c(1,2))
# Predict the training data and plot the ROC curve
fitted <- attributes(predict(svmfit, dat[train, ], decision.values = TRUE))$decision.values
auc <- rocplot(fitted, dat[train, "y"], main = "Training Data")
auc@y.values

# Predict the test data and plot the ROC curve
fitted <- attributes(predict(svmfit, dat[-train, ], decision.values = TRUE))$decision.values
auc <-rocplot(fitted, dat[-train,"y"], main = "Test Data")
auc@y.values

