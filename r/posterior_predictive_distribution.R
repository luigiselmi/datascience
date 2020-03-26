# R code 3.26 rethinking book
# Once we have a model we want to use it to make predictions.
# In this script we create a model then we collect samples from it
data_size <- 1000 # size of the dataset
# creates a sequence of <data_size> values between 0 and 1,
# that represent possible values of a parameter
p_grid <- seq(from = 0, to = 1, length.out = data_size)

# creates a sequence of ones that represent the
# prior probability of each possible value of the parameter
prior <- rep(1, data_size)

# computes the likelihood of each value of the paramenter
# under the binomial distribution and the observations
likelihood <- dbinom(6, size = 9, prob = p_grid)
#plot(likelihood)

# computes the posterior distribution
posterior <- likelihood * prior

# normalization of the posterior distribution
posterior <- posterior / sum(posterior)

# creates samples of <sample_size> from the posterior distribution
samples_size = 1e4
samples <- sample(p_grid, prob = posterior, size = samples_size, replace = TRUE)

# there is uncertainty over the parameter so we propagate through all
# the possible values to create predictions
w <- rbinom(1e4, size = 9, prob = samples)

# plot the predictions
simplehist(w)



