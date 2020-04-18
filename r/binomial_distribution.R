# Binomial distribution
# Estimation of the bias of a coin.
# We assume that a coin can be biased so that the probability 
# theta that after a toss it shows head or tail 
# may not be 0.5. We assume at the beginning that theta can have any
# value between 0 and 1 but with different probabilities. We have some
# sound even if not complete information about the plausible values of
# theta. In our example we assume a triangular prior p(theta). We use 
# the binomial distribution as the likelihood. Then we set the sample 
# size n and we set the number of successes k. Finally, we compute the 
# posterior distribution as the product of the prior and the likelihood 
# distributions.

# Example 1 from Kruschke ch.5 par. 5.3
# range of values of the parameter
theta <- seq(from = 0, to = 1, by = 0.1)

# define the prior distribution for each value of theta according to our
# knowledge before seeing the data.
p1 <- 0.4 * theta[1:6]
p2 <- 0.4 - 0.4 * theta[7:11]
prior <- c(p1,p2)

plot(theta, prior, type = "h", col = "skyblue")

# sample the likelihood at each value of the parameter theta
# for one toss. The binomial distribution used as the likelihood
# is also called Bernoulli distribution when the sample size n = 1.
# The way in which we extract the sample from the likelihood distribution
# is called grid approximation because the elements of the sample are taken
# from one data point and a set of equally spaced values of theta. This 
# approximation works because we are dealing with only one parameter and
# values in a limited interval. Other approximation are quadratic and MCMC.
n = 1 # sample size
k = 1 # number success events out the sample 
likelihood <-dbinom(k, size = n, prob = theta)
plot(theta, likelihood, ylab = "likelihood p(x | theta)", type = "h", col = "skyblue")

# compute the marginal likelihood p(D)
marginal <- sum(likelihood * prior)

# compute the posterior distribution for theta using the Bayes rule
posterior <- likelihood * prior / marginal

# compute the posterior mode (value with most occurrences) 
mode_posterior <- theta[which.max(posterior)]

plot(theta, posterior, ylab = "posterior p(theta | x)", type = "h", col = "skyblue")

# Example 2 from Kruschke ch.5 par. 5.3.1
# Influence of sample size. 
theta <- s <- seq(from = 0, to = 1, by = 0.001)
# define the prior distribution for each value of theta
p1 <- 0.4 * theta[1:500]
p2 <- 0.4 - 0.4 * theta[501:1001]
prior <- c(p1,p2)
plot(theta, prior, ylab = "prior_1000", type = "h", col = "skyblue")

# compute likelihood at each value of the parameter theta
n = 40 # sample size
k = 10 # number success events out the sample 
likelihood <-dbinom(k, size = n, prob = theta)

# compute the likelihood mode (value with most occurrences) 
mode_likelihood <- theta[which.max(likelihood)]

plot(theta, likelihood, ylab = "likelihood_1000 p(x | theta)", type = "h", col = "skyblue")
text( .5 , 0.1 , paste("mode =", mode_likelihood))

# compute the marginal likelihood p(D)
marginal <- sum(likelihood * prior)

# compute the posterior distribution for theta
posterior <- likelihood * prior / marginal

# compute the posterior mode (value with most occurrences) 
mode_posterior <- theta[which.max(posterior)]

plot(theta, posterior, ylab = "posterior_1000 p(theta | x)", type = "h", col = "skyblue")
text( .7 , 0.0020 , paste("mode =", mode_posterior))

