# R code 3.11 rethinking book
# This script shows how to summarize a posterior distributions
# by computing point estimates (mode, mean, median) and
# intervals

data_size <- 1000 # size of the dataset
# creates a sequence of <data_size> values between 0 and 1,
# that represent possible values of a parameter
p_grid <- seq(from = 0, to = 1, length.out = data_size)

# creates a sequence of ones that represent the
# prior probability of each possible value of the parameter
prior <- rep(1, data_size)

# computes the likelihood of each value of the paramenter
# under the binomial distribution and the observations
likelihood <- dbinom(3, size = 3, prob = p_grid)
#plot(likelihood)

# computes the posterior distribution
posterior <- likelihood * prior

# normalization of the posterior distribution
posterior <- posterior / sum(posterior)

# creates a sample of <sample_size> values from the distribution
samples_size = 1e4
samples <- sample(p_grid, prob = posterior, size = samples_size, replace = TRUE)
#plot(samples)
library(rethinking)
dens(samples)

# computes the probabilities  between 25 % and 75 % of the interval
PI(samples, prob = 0.5)

# computes the 50 % highest density interval, the narrowest interval
# containing the specified probability mass, e.g. 50 %
HPDI(samples, prob = 0.5)

# computes the parameter value with highest posterior probability (grid approximation)
# it is called mode or maximum a posteriori (MAP)
p_grid[which.max(posterior)]

# same but from samples taken from the posterior distribution 
chainmode(samples, adj = 0.01)

# computes the mean
mean(samples)

# computes the median
median(samples)

# defines a loss function that computes the weighted distance between
# a parameter value x and any other value in the distribution
loss <- function(x) sum(posterior*abs(x - p_grid))

# computes the loss in assuming 0.5 as the value of the parameter
loss(0.5)

# computes the total loss for any possible value of the parameter
tot_loss <- sapply(p_grid, loss)

# finds the parameter value with minimum loss
p_grid[which.min(tot_loss)]


