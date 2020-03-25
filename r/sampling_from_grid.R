# R code from R code snippet 3.2 to .. (rethinking book)
# Usually a model has many parameters and cannot be handled analytically
# so the parameters are computed numerically using samples taken from
# posterior distribution.This script shows how to build a sample from a
# statistical model with only one parameter (like in the Globe example)
# and how to summarize the posterior distribution computing for example
# the value with highest probability or the probability for the parameter
# to have any value below(above) a defined one. 
data_size <- 1000 # size of the dataset

# creates a sequence of <data_size> values between 0 and 1,
# that represent possible values of a parameter
p_grid <- seq(from = 0, to = 1, length.out = data_size)  

# creates a sequence of ones that represent the
# prior probability of each possible value of the parameter
prob_p <- rep(1, data_size) 

# computes the likelihood of each value of the paramenter
# under the binomial distribution and the observations
prob_data <- dbinom(6, size = 9, prob = p_grid)

# computes the posterior distribution
posterior <- prob_data * prob_p

# normalization of the posterior distribution
posterior <- posterior / sum(posterior)
#plot(posterior)

# creates a sample of <sample_size> values from the distribution
samples_size = 1e4
samples <- sample(p_grid, prob = posterior, size = samples_size, replace = TRUE)
#plot(samples) # uncomment to plot the data

library(rethinking)
# plot the (density) posterior distribution 
#dens(samples) # uncomment to plot the data

# Sampling to summarize
# posterior probability for the parameter to be below 0.5 (without sampling)
sum(posterior[p_grid < 0.5])
# same using the samples
sum(samples < 0.5) / samples_size

# posterior probability for the parameter between 0.5 and 0.75, using ther samples
sum(samples > 0.5 & samples < 0.75) / samples_size


