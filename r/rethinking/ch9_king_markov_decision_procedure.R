# R code 9.1 rethinking book
# King Markov decision procedure
num_weeks <- 1e5
positions <- rep(0, num_weeks)
current <- 10
for (i in 1:num_weeks) {
  # record current position
  positions[i] <- current
  
  # flip coin to generate proposal
  proposal <- current + sample(c(-1,1), size = 1)
  # make sure he loops around the archipelago
  if (proposal < 1) proposal <- 10
  if (proposal > 10) proposal <- 1
  
  # move ?
  prob_move <- proposal / current
  current <- ifelse(runif(1) < prob_move, proposal, current)
}
plot( 1:1000 , positions[1:1000] )
plot( table( positions ) )

# Many dimensions problem. A probability density function
# in a hyperspace most of the points are close the its surface
# far from the mean. So sampling close to it won't be efficient.
library(rethinking)
D <- 1000
T <- 1e4
Y <- rmvnorm(T, rep(0, D), diag(D)) # transforms univariate normal distribution, with 0 mean, to a multivariate distribution
rad_dist <- function(Y) sqrt(sum(Y^2))
Rd <- sapply(1:T, function(i) rad_dist(Y[i, ]))
dens(Rd)