# R code from snippet 3.23 (rethinking book).
# We use the binomial distribution with, e.g. parameter p = 0.7,
# to create simulations.

obs <- 1e5
size <- 9
# Generates <obs> observations from a sample of size <size> 
# Each observation tells, e.g. how many waters we had from each sample of size <size>
dummy_w <- rbinom(obs, size = size, prob = 0.7) 

# cumputes how many times we had one of the possible
# values from each sample, e.g. from samples of two tosses (size = 2) we can have
# water 0, 1 or 2 times. The values are normalized. 
table(dummy_w) / obs

# plot the histogram of the distribution in the simulation data
simplehist(dummy_w, xlab = 'dummy water count')


