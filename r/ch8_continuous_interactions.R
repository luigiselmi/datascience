# R code 8.19 rethinking book
# We create two models of the bloom of flowers that depends 
# on water and shade. The first model is a linear combination
# of the two, the second model contains an interaction term
# between water and shade.
library(rethinking)
data("tulips")
d <- tulips
str(d)
# standardize the variables
d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)

# after some reasoning about the plausible values of the parameters'
# prior distribution, we have the 1st model (without interaction)
m8.6 <- quap(
  alist(
    blooms_std ~ dnorm( mu , sigma ) ,
    mu <- a + bw * water_cent + bs * shade_cent, 
    a ~ dnorm(0.5, 0.25) ,
    bw ~ dnorm(0, 0.25),
    bs ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ) ,
  data = d
)

# after some additional thinking about the parameter of the interaction
# term we have the 2nd model
m8.7 <- quap(
  alist(
    blooms_std ~ dnorm( mu , sigma ) ,
    mu <- a + bw * water_cent + bs * shade_cent + bws * water_cent * shade_cent, 
    a ~ dnorm(0.5, 0.25) ,
    bw ~ dnorm(0, 0.25),
    bs ~ dnorm(0, 0.25),
    bws ~ dnorm(0, 0.25),
    sigma ~ dexp(1)
  ) ,
  data = d
)

# Let's plot a triptych ("trittico" in Italian) of the posterior prediction for blooms for the 
# 1st model (without interaction), The plot will show the relation between water and blooms for
# three different values of shade.
par(mfrow = c(1,3)) # 3 plots in a row
for (s in -1:1) {
  idx <- which(d$shade_cent == s)
  plot(d$water_cent[idx], d$blooms_std[idx], xlim = c(-1,1), ylim = c(0,1),
       xlab = "water", ylab = "blooms", pch = 16, col = rangi2)
  title(paste("shade ", s))
  mu <- link(m8.6, data = data.frame(shade_cent = s, water_cent = -1:1))
  for (i in 1:20)
    lines(-1:1, mu[i, ], col = col.alpha("black", 0.3))
}

# Now we plot a triptych of the posterior prediction for blooms for the 2nd model (with interaction).
# Again, the plot will show the relation between water and blooms for three different values of shade.
par(mfrow = c(1,3)) # 3 plots in a row
for (s in -1:1) {
  idx <- which(d$shade_cent == s)
  plot(d$water_cent[idx], d$blooms_std[idx], xlim = c(-1,1), ylim = c(0,1),
       xlab = "water", ylab = "blooms", pch = 16, col = rangi2)
  title(paste("shade ", s))
  mu <- link(m8.7, data = data.frame(shade_cent = s, water_cent = -1:1))
  for (i in 1:20)
    lines(-1:1, mu[i, ], col = col.alpha("black", 0.3))
}

# Now we plot the prior predictive simulations
set.seed(7)
prior <- extract.prior(m8.6)
par(mfrow = c(1,3)) # 3 plots in a row
for (s in -1:1) {
  idx <- which(d$shade_cent == s)
  plot(d$water_cent[idx], d$blooms_std[idx], xlim = c(-1,1), ylim = c(0,1),
       xlab = "water", ylab = "blooms", pch = 16, col = rangi2)
  title(paste("shade ", s))
  mu <- link(m8.6, data = data.frame(shade_cent = s, water_cent = -1:1), post = prior)
  for (i in 1:20)
    lines(-1:1, mu[i, ], col = col.alpha("black", 0.3))
}


prior <- extract.prior(m8.7)
par(mfrow = c(1,3)) # 3 plots in a row
for (s in -1:1) {
  idx <- which(d$shade_cent == s)
  plot(d$water_cent[idx], d$blooms_std[idx], xlim = c(-1,1), ylim = c(0,1),
       xlab = "water", ylab = "blooms", pch = 16, col = rangi2)
  title(paste("shade ", s))
  mu <- link(m8.7, data = data.frame(shade_cent = s, water_cent = -1:1), post = prior)
  for (i in 1:20)
    lines(-1:1, mu[i, ], col = col.alpha("black", 0.3))
}