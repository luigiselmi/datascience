# R code 2.6, 2.7 rethinking book

library(rethinking)

globe.qa <- quap(
  alist(
    W ~ dbinom(W + L, p), # binomial likelihood
    p ~ dunif(0,1) # uniform prior
  ),
  data = list(W = 6, L = 3)
)

precis(globe.qa)

# analytical calculation
W <- 12
L <- 6

curve(dbeta(x, W + 1, L + 1), from = 0, to = 1)
#quadratic approximation
curve(dnorm(x, 0.67, 0.16), lty = 2, add = TRUE)
