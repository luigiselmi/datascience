# R code 11.1 rethinking book
# Binomial regression - Prosocial chimpanzees experiment.
# We want to see whether a chimpazee shares food with others or not.
library(rethinking)
data("chimpanzees")
d <- chimpanzees

d$treatment <- 1 + d$prosoc_left + 2 * d$condition

xtabs(~ treatment + prosoc_left + condition, d)

m11.1 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a, 
    a ~ dnorm(0, 1.5) 
  ) ,
  data = d
)

m11.2 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + b[treatment] , 
    a ~ dnorm(0, 1.5) ,
    b[treatment] ~ dnorm(0, 10)
  ) ,
  data = d
)

m11.3 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + b[treatment] , 
    a ~ dnorm(0, 1.5) ,
    b[treatment] ~ dnorm(0, 0.5)
  ) ,
  data = d
)

set.seed(1999)
prior <- extract.prior(m11.3, n = 1e4)
p <- sapply(1:4, function(k) inv_logit(prior$a + prior$b[, k]))
dens(abs(p[, 1] - p[, 2]), adj = 0.1)

# prior trimmed data list
dat_list <- list(
  pulled_left = d$pulled_left , 
  actor = d$actor ,
  treatment = as.integer(d$treatment)
)

# particles in 11-dimensional space
m11.4 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p) ,
    logit(p) <- a[actor] + b[treatment] , 
    a[actor] ~ dnorm(0, 1.5) ,
    b[treatment] ~ dnorm(0, 0.5)
  ) ,
  data = dat_list, chains = 4
)

precis(m11.4, depth = 2)

post <- extract.samples(m11.4)
p_left <- inv_logit(post$a)
labs <- c("R/N", "L/N", "R/P", "L/P")
plot(precis(m11.4, depth = 2, pars = "b"), labels = labs) # not what expected
#plot(precis(as.data.frame(p_left)), xlim = c(0,1)) # error
