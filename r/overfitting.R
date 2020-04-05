# R code 7.1 rethinking book
sppnames <- c("afarensis", "africanus", "habilis", "boisei",
              "rudolfensis", "ergaster", "sapiens")
brainvolcc <- c(438, 452, 612, 521, 752, 871, 1350)
masskg <- c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)
d <- data.frame(species = sppnames, brain = brainvolcc, mass = masskg)
# We start modeling the relationship between brain volumes and body mass 
# as a linear function. 
# We standardize the variables
library(rethinking)
d$mass_std <- (d$mass - mean(d$mass)) / sd(d$mass)
d$brain_std <- d$brain / max(d$brain)
m7.1 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ) ,
    mu <- a + b * mass_std , 
    a ~ dnorm(0.5, 1) ,
    b ~ dnorm(0, 10),
    log_sigma ~ dnorm(0, 1)
  ) ,
  data = d
)

# We see how the model gets close to the data. The frequentist idea
# is that the model is as good as the variance of the residuals is close
# or smaller than the vairance of the data itself. A residula is the
# difference between an observation and a value predicted by the model.
set.seed(12)
s <- sim(m7.1)
r <- apply(s, 2, mean) - d$brain_std
resid_var <- var2(r)
outcome_var <- var2(d$brain_std)
1 - resid_var / outcome_var # computes R^2

R2_is_bad <- function(quap_fit) {
  s <- sim(quap_fit, refresh = 0)
  r <- apply(s, 2, mean) - d$brain_std # residuals
  1 - var2(r) / var2(d$mass_std)
}

# We want to compare how different models fit the data. We build
# them using polynomials
m7.2 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ) ,
    mu <- a + b[1] * mass_std + b[2] * mass_std^2 , 
    a ~ dnorm(0.5, 1) ,
    b ~ dnorm(0, 10),
    log_sigma ~ dnorm(0, 1)
  ) ,
  data = d, start = list(b = rep(0, 2))
)
m7.3 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ) ,
    mu <- a + b[1] * mass_std + b[2] * mass_std^2 + b[3] * mass_std^3, 
    a ~ dnorm(0.5, 1) ,
    b ~ dnorm(0, 10),
    log_sigma ~ dnorm(0, 1)
  ) ,
  data = d, start = list(b = rep(0, 3))
)
m7.4 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ) ,
    mu <- a + b[1] * mass_std + b[2] * mass_std^2 + b[3] * mass_std^3 + b[4] * mass_std^4, 
    a ~ dnorm(0.5, 1) ,
    b ~ dnorm(0, 10),
    log_sigma ~ dnorm(0, 1)
  ) ,
  data = d, start = list(b = rep(0, 4))
)
m7.5 <- quap(
  alist(
    brain_std ~ dnorm( mu , exp(log_sigma) ) ,
    mu <- a + b[1] * mass_std + b[2] * mass_std^2 + b[3] * mass_std^3 + b[4] * mass_std^4 + b[5] * mass_std^5 , 
    a ~ dnorm(0.5, 1) ,
    b ~ dnorm(0, 10),
    log_sigma ~ dnorm(0, 1)
  ) ,
  data = d, start = list(b = rep(0, 5))
)
m7.6 <- quap(
  alist(
    brain_std ~ dnorm( mu , 0.001 ) ,
    mu <- a + b[1] * mass_std + b[2] * mass_std^2 +
              b[3] * mass_std^3 + b[4] * mass_std^4 + 
              b[5] * mass_std^5 + b[6] * mass_std^6 , 
    a ~ dnorm(0.5, 1) ,
    b ~ dnorm(0, 10),
    log_sigma ~ dnorm(0, 1)
  ) ,
  data = d, start = list(b = rep(0, 6))
)
# plot the models
# linear model
post <- extract.samples(m7.1)
mass_seq <- seq(from = min(d$mass_std), to = max(d$mass_std), length.out = 100)
l <- link(m7.1, data = list(mass_std = mass_seq))
mu <- apply(l, 2, mean)
ci <- apply(l, 2, PI)
plot(brain_std ~ mass_std, data = d)
lines(mass_seq, mu)
shade(ci, mass_seq)
R2_is_bad(m7.1)
# order 2 polynomial model
post <- extract.samples(m7.2)
l <- link(m7.2, data = list(mass_std = mass_seq))
mu <- apply(l, 2, mean)
ci <- apply(l, 2, PI)
plot(brain_std ~ mass_std, data = d)
lines(mass_seq, mu)
shade(ci, mass_seq)
R2_is_bad(m7.2)
# order 5 polynomial model
post <- extract.samples(m7.5)
l <- link(m7.5, data = list(mass_std = mass_seq))
mu <- apply(l, 2, mean)
ci <- apply(l, 2, PI)
plot(brain_std ~ mass_std, data = d)
lines(mass_seq, mu)
shade(ci, mass_seq)
R2_is_bad(m7.5)
# order 6 polynomial model
post <- extract.samples(m7.6)
l <- link(m7.6, data = list(mass_std = mass_seq))
mu <- apply(l, 2, mean)
ci <- apply(l, 2, PI)
plot(brain_std ~ mass_std, data = d)
lines(mass_seq, mu)
shade(ci, mass_seq)
R2_is_bad(m7.6)
# ordinary least squares (uses the R lm() function)
m7.1_OLS <- lm(brain_std ~ mass_std, data = d)
post <- extract.samples(m7.1_OLS) # how to plot this ?

# Measure distance from target model
set.seed(1)
sapply(list(m7.1, m7.2, m7.3, m7.4, m7.5, m7.6), function(m) sum(lppd(m)))