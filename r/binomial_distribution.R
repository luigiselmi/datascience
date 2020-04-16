# Binomial distribution
# Example from Kruschke ch.5 par. 5.3
theta <- s <- seq(from = 0, to = 1, by = 0.1)

# define the prior distribution for each value of theta
p1 <- 0.4 * theta[1:6]
p2 <- 0.4 - 0.4 * theta[7:11]
prior <- c(p1,p2)
plot(theta, prior)

# compute likelihood at each value of the parameter theta
# after one toss
n = 1 # sample size
k = 1 # number success events out the sample 
likelihood <-dbinom(k, size = n, prob = theta)
plot(theta, likelihood, ylab = "likelihood p(x | theta)")

# compute the marginal p(D)
marginal <- sum(likelihood * prior)

# compute the posterior distribution for theta
posterior <- likelihood * prior / marginal

plot(theta, posterior, ylab = "posterior p(theta | x)")

