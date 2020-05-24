library(ISLR)

# 5.3.2 Leave-One-Out Cross-Validation
library(boot)
cv.error <- rep(0, 5)

for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

plot(1:5, cv.error, type = "b", xlab = "degree of polynomial", ylab = "mean squared error" )

# 5.3.3 k-Fold Cross-Validation
set.seed(17)
cv.error = rep(0, 10)
for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

plot(1:10, cv.error, type = "b", xlab = "degree of polynomial", ylab = "mean squared error" )

# The Bootstrap
# Estimating the accuracy of a linear regression model
boot.fn <- function(data, index) {
  coef(lm(mpg ~ horsepower, data = data, subset = index))
}

boot.fn(Auto, 1:392)

boot.fn(Auto, sample(392, 392, replace = TRUE))

# We can use the boot() function to compute the standard error of 1000 bootstrap estimates for the coefficients 
# of the linear model. The bootstrap samples are randomly taken from the original data set with replacement. 
boot(Auto, boot.fn, 1000)

# Question 5.R.R3
load("data/5.R.RData")

beta_coef <- function(data, index) {
  coef(lm(y ~ X1 + X2, data = data, subset = index)) 
}

beta_coef(Xy,1:100)

boot(Xy, beta_coef, 1000)

# Question 5.R.R4
# resample in blocks of 100 observations
new.rows = c(501:600, 401:500, 101:200, 901:1000, 301:400, 1:100, 601:700, 801:900, 201:300, 701:800)
new.Xy = Xy[new.rows, ]
boot(new.Xy, beta_coef, 1000)

