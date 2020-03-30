# R code 4.64 rethinking book
# The same approach used to compute the posterior distribution
# of a variable that depends linearly on another can be used when
# the dependency is not linear. As in the linear case when the model
# learnt from the data the values of the two parameters, slope and
# intercept, of the linear relationship, it can learn the parameters
# of a basis of polynomials that represents the non-linear dependency.
# e.g. mu = a + b1x + b2x^2 
library(rethinking)
data("Howell1")
d <- Howell1
str(d)
# plot height against weight
plot(d$height ~ d$weight)

# defines the model
d$weight_s <- (d$weight - mean(d$weight)) / sd(d$weight) # standardize the predictor variable
d$weight_s2 <- d$weight_s^2
# 2nd order polynomial
m4.5 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm(0, 1) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d )

# 3rd order polynomial
d$weight_s3 = d$weight_s^3
m4.6 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 + b3*weight_s3 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm(0, 1) ,
    b3 ~ dnorm(0, 1) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d )

precis(m4.5)

# plot the 2nd order polynomial model
weight.seq <- seq(from = -2.2, to = 2, length.out = 30)
pred_dat <- list(weight_s = weight.seq, weight_s2 = weight.seq^2)
mu <- link(m4.5, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(m4.5, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

plot(height ~ weight_s, d, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)


# plot the 3rd order polynomial model
weight.seq <- seq(from = -2.2, to = 2, length.out = 30)
pred_dat <- list(weight_s = weight.seq, weight_s2 = weight.seq^2, weight_s3 = weight.seq^3)
mu <- link(m4.6, data = pred_dat)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
sim.height <- sim(m4.6, data = pred_dat)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)

plot(height ~ weight_s, d, col = col.alpha(rangi2, 0.5))
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
shade(height.PI, weight.seq)