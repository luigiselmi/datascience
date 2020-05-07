# R code 8.1 rethinking book
# We want to model the relationship between a country's GDP and
# the ruggedness of its territory.
library(rethinking)
data(rugged)
d <- rugged

# make log version of the outcome
d$log_gdp <-- log(d$rgdppc_2000)

# extract countries with GDP data
dd <- d[complete.cases(d$rgdppc_2000), ]

# rescale variables
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)

# split countries into Africa and not-Africa
d.A1 <- dd[dd$cont_africa == 1, ] # Africa
d.A0 <- dd[dd$cont_africa == 0, ] # not Africa

# we model the relationship between GDP and ruggedness
# for Africa
m8.1 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + b * (rugged_std - 0.215), 
    a ~ dnorm(1, 0.1) ,
    b ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ) ,
  data = d.A1
)

# Let#s look at the priors to see if they are plausible.
# If not we change their standard deviation.
set.seed(7)
prior <- extract.prior(m8.1)
# set up the plot dimensions
plot(NULL, xlim = c(0,1), ylim = c(0.5, 1.5),
     xlab = "ruggedness", ylab = "log GDP")
abline(h = min(dd$log_gdp_std), lty = 2)
abline(h = max(dd$log_gdp_std), lty = 2)
# draw 50 lines from the prior
rugged_seq <- seq(from = -0.1, to = 1.1, length.out = 30)
mu <- link(m8.1, post = prior, data = data.frame(rugged_std = rugged_seq))
for (i in 1:50)
  lines(rugged_seq, mu[i, ], col = col.alpha("black", 0.3))

# we model the relationship between GDP and ruggedness
# for not-Africa
m8.2 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + b * (rugged_std - 0.215), 
    a ~ dnorm(1, 0.1) ,
    b ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ) ,
  data = d.A0
)

# we look at the posteriors for the parameters in both models
# and we see that the relationship (parameter b) is different
# positive for Africa and negative for not-Africa
precis(m8.1) # Africa
precis(m8.2) # not-Africa

# Now we want to see how to reach the same result using only
# one model and one single data set without splitting it.
# We start building a model like the previous ones but with 
# the full dataset. Then we build another model that will use
# different intercets for African and not-African countries within
# the same model. We will then compare these two models.
m8.3 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a + b * (rugged_std - 0.215), 
    a ~ dnorm(1, 0.1) ,
    b ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ) ,
  data = dd
)
# we use an indexed intercpt instead of a dummy variable to distinguish
# between African and not-African countries
# create a variable to index Africa (1) or not (2)
dd$cid <- ifelse(dd$cont_africa == 1, 1, 2) 
m8.4 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b * (rugged_std - 0.215), 
    a[cid] ~ dnorm(1, 0.1) ,
    b ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ) ,
  data = dd
)
# We compare the two models that use the full dataset
compare(m8.3, m8.4)
# we look at the posterior parameters
precis(m8.4, depth = 2)
# let's plot the posterior predictions
rugged_seq <- seq(from = -0.1, to = 1.1, length.out = 30)
# compute mu over samples, fixing cid = 2 (not-Africa)
mu.NotAfrica <- link(m8.4, data = data.frame(cid = 2, rugged_std = rugged_seq))
# compute mu over samples, fixing cid = 1 (Africa)
mu.Africa <- link(m8.4, data = data.frame(cid = 1, rugged_std = rugged_seq))
# summarize to means and intervals
mu.NotAfrica_mu <- apply(mu.NotAfrica, 2, mean)
mu.NotAfrica_ci <- apply(mu.NotAfrica, 2, PI, prob = 0.97)
mu.Africa_mu <- apply(mu.Africa, 2, mean)
mu.Africa_ci <- apply(mu.Africa, 2, PI, prob = 0.97)
# we can see that the model with the indexed intercept (a[cid]) is not a better model because
# the interaction used for the intercept doesn't help explaining the different
# role of ruggedness on GDP we saw at the beginning for African and not-African countries.
# We have to make the parameter b also dependent on being Africa or not-Africa
m8.5 <- quap(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid] * (rugged_std - 0.215), 
    a[cid] ~ dnorm(1, 0.1) ,
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ) ,
  data = dd
)
precis(m8.5, depth = 2)
# Let's see how much adding the interaction at the slope improves the model
compare(m8.3, m8.4, m8.5)
# plot Africa data points, cid = 1
plot(d.A1$rugged_std, d.A1$log_gdp_std, pch = 16, col = rangi2,
     xlab = "ruggedness (standardized)", ylab = "log GDP (as proportion of mean)",
     xlim = c(0,1))
mu <- link(m8.5, data = data.frame(cid = 1, rugged_std = rugged_seq))
mu_mean <- apply(mu, 2, mean)
mu_ci <- apply(mu, 2, PI, prob = 0.97)
lines(rugged_seq, mu_mean, lwd = 2)
shade(mu_ci, rugged_seq, col = col.alpha(rangi2, 0.3))
mtext("African nations")

# plot not-Africa data points, cid = 2
plot(d.A1$rugged_std, d.A1$log_gdp_std, pch = 16, col = rangi2,
     xlab = "ruggedness (standardized)", ylab = "log GDP (as proportion of mean)",
     xlim = c(0,1))
mu <- link(m8.5, data = data.frame(cid = 2, rugged_std = rugged_seq))
mu_mean <- apply(mu, 2, mean)
mu_ci <- apply(mu, 2, PI, prob = 0.97)
lines(rugged_seq, mu_mean, lwd = 2)
shade(mu_ci, rugged_seq, col = col.alpha(rangi2, 0.3))
mtext("Non-African nations")