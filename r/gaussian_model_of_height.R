# R code 4.7 rethinking book
library(rethinking)
data("Howell1")
d <- Howell1 # dataframe
precis(d)

d2 <- d[d$age >= 18, ]
# plot the height distribution
#dens(d2$height)

#curve(dnorm(x, 178, 20), from = 100, to = 250)

#curve(dunif(x, 0, 50), from = -10, to = 60)

# prior probability distribution for the mean 
sample_mu <- rnorm(1e4, 178, 20)
# prior probability distribution for the standard deviation
sample_sigma <- runif(1e4, 0, 50)
# joint prior probability distribution of height, before seeing the data
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
# plot the joint prior probability distribution
#dens(prior_h) 

# grid approximation of the posterior distribution
mu.list <- seq(from = 150, to = 160, length.out = 100)
sigma.list <- seq(from = 7, to = 9, length.out = 100)
post <- expand.grid(mu = mu.list, sigma = sigma.list)
post$LL <- sapply(1:nrow(post), function(i) sum(
  dnorm(d2$height, post$mu[i], post$sigma[i], log = TRUE)))
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) + dunif(post$sigma, 0, 50, TRUE)
post$prob <- exp(post$prod - max(post$prod))

# plot the posterior distribution
#contour_xyz(post$mu, post$sigma, post$prob)
#image_xyz(post$mu, post$sigma, post$prob)

sample.rows <- sample(1:nrow(post), size = 1e4, replace = TRUE, prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]
#plot(sample.mu, sample.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.5))

#dens(sample.mu)
#dens(sample.sigma)
HPDI(sample.mu)

# test the standard deviation with a small sample size
# to see that the sigma posterior distribution is not Gaussian
d3 <- sample(d2$height, size = 20)
# grid approximation of the posterior distribution
mu.list <- seq(from = 150, to = 160, length.out = 100)
sigma.list <- seq(from = 7, to = 9, length.out = 100)
post2 <- expand.grid(mu = mu.list, sigma = sigma.list)
post2$LL <- sapply(1:nrow(post2), function(i) sum(
  dnorm(d3, post2$mu[i], post2$sigma[i], log = TRUE)))
post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, TRUE) + dunif(post2$sigma, 0, 50, TRUE)
post2$prob <- exp(post2$prod - max(post2$prod))

# plot the posterior distribution
#contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post2$mu, post2$sigma, post$prob)

sample2.rows <- sample(1:nrow(post2), size = 1e4, replace = TRUE, prob = post2$prob)
sample2.mu <- post2$mu[sample2.rows]
sample2.sigma <- post2$sigma[sample2.rows]
plot(sample2.mu, sample2.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.5),
     xlab = 'mu', ylab = 'sigma')
dens(sample2.sigma, norm.comp = TRUE)
