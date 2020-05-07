# R code 5.18 rethinking book
# Masked relationship
# We investigate the relationships between the milk kilocalories (K) and two 
# other variables: neocortex percentage (N) and log body mass (M). We first build
# two bivariate models (K ~ N) and (K ~ N) from which we see that taken separately
# the two variables N and M have a weak relationship with K. Then we build a 3rd
# multivariate model taking into account both N and M together to show that they
# have a strong relationship with K.
library(rethinking)
data(milk)
d <- milk
str(d)
d$K <- scale(d$kcal.per.g)
d$N <- scale(d$neocortex.perc) # contains NA values
d$M <- scale(log(d$mass))

# remove NA values
dcc <- d[complete.cases(d$K, d$N, d$M), ]
# ---------------------------------------------------------------------
# 1) builds the first model kilocalories (K) - neocortex percentage (N)
# ---------------------------------------------------------------------
m5.5_draft <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN * N ,
    a ~ dnorm( 0 , 1 ) ,
    bN ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp(1)
  ) ,
  data = dcc
)

# builds a 2nd model kilocalories (K) - neocortex percentage (N)
# with smaller interval values for the predictors a and bN
# This model should produce more reasonable priors and as a 
# consequence also posterior.
m5.5 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN * N ,
    a ~ dnorm( 0 , 0.2 ) ,
    bN ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ) ,
  data = dcc
)


#prior <- extract.prior(m5.5_draft)
prior <- extract.prior(m5.5)

# plot the prior regression lines
xseq <- c(-2,2)
mu <- link(m5.5, post = prior, data = list(N = xseq))
plot(NULL, xlim = xseq, ylim = xseq) # plot the frame
for (i in 1:50)
  lines(xseq, mu[i, ], col = col.alpha("black", 0.3)) # plausible regression lines

# Let's look at the posterior
# It shows that the relationship between milk kilocalories and neocortex is weak:
# small value of the bN parameter and standard deviation almost twice the mean.
precis(m5.5)

# Let's plot the posterior
xseq <- seq(from = min(dcc$N) - 0.15, to = max(dcc$N) + 0.15, length.out = 30)
mu <- link(m5.5, data = list(N = xseq))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(K ~ N, data = dcc)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)


# --------------------------------------------------------
# 2) builds the 2nd model kilocalories (K) - body mass (M)
# --------------------------------------------------------
m5.6 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ) ,
  data = dcc
)

prior <- extract.prior(m5.6)

# plot the prior regression lines
xseq <- c(-2,2)
mu <- link(m5.6, post = prior, data = list(M = xseq))
plot(NULL, xlim = xseq, ylim = xseq) # plot the frame
for (i in 1:50)
  lines(xseq, mu[i, ], col = col.alpha("black", 0.3)) # plausible regression lines

# Let's look at the posterior
# It shows that also the relationship between milk kilocalories and body mass is weak:
# small (negative) value of the bM parameter and comparable standard deviation.
precis(m5.6)

# Let's plot the posterior
xseq <- seq(from = min(dcc$M) - 0.15, to = max(dcc$M) + 0.15, length.out = 30)
mu <- link(m5.6, data = list(M = xseq))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(K ~ N, data = dcc)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

# ------------------------------------------------------------------------------------------------
# 3) builds the 3nd multivariate model kilocalories (K) depends on neocortex (N) and body mass (M)
# ------------------------------------------------------------------------------------------------
# We build a multivariate linear model with milk kilocalories (K) that depends linearly by both the
# neocortex percentage (N) and the body mass (M)
m5.7 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN * N + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bN ~ dnorm( 0 , 0.5 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ) ,
  data = dcc
)
# We can see, from the posterior mean and standard deviation, that K depends strongly on both N and M
precis(m5.7)
#plot(coeftab(m5.5, m5.6, m5.7), pars = c("bM", "bN")) # this doesn't work
pairs(~K + M + N, dcc)

# Let's now draw the counterfactual plot of the multivariate model.
# Here we keep the neocortex percentage (N) constant at 0 (N = 0) so that we can see
# the relation between K and the body mass M. We can see that the relation is stronger
# in the multivariate model.
xseq <- seq(from = min(dcc$M) - 0.15, to = max(dcc$M) + 0.15, length.out = 30)
mu <- link(m5.7, data = data.frame(M = xseq, N = 0))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(NULL, xlim = range(dcc$M), ylim = range(dcc$K))
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

# Here we keep the body mass (M) constant at 0 (M = 0) so that we can see
# the relation between K and the neocortex percentage (N). We can see also in this case
# that the relation is stronger in the multivariate model.
xseq <- seq(from = min(dcc$N) - 0.15, to = max(dcc$N) + 0.15, length.out = 30)
mu <- link(m5.7, data = data.frame(N = xseq, M = 0))
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)
plot(NULL, xlim = range(dcc$M), ylim = range(dcc$K))
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)
