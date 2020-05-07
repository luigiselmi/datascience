# R code 4.37 rethinking book
library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18,]
#plot(d2$height ~ d2$weight) # uncomment to plot

set.seed(2971)
N <- 100
a <- rnorm(N, 178, 20)
#b <- rnorm(N, 0, 10)

b <- rlnorm(1e4, 0, 1)
dens(b, xlim = c(0,5), adj = 0.1) # plot the log-normal distribution

# plot N lines with random slopes and intercepts
b <- rlnorm(N, 0, 1) # limits the slope to positive values (log-normal distribution)
plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) , xlab="weight" , ylab="height" )
abline( h=0 , lty=2) # horizontal line
abline( h=272 , lty=1 , lwd=0.5 ) # horizontal line, height of the tallest known person
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
                        from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
                        col=col.alpha("black",0.2) )

# load data again, since it's a long way back
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
# define the average weight, x-bar
xbar <- mean(d2$weight)
# fit model
m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - xbar ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d2 )

# plots the line using mean values for alpha (a) and beta (b)
plot(height ~ weight, data = d2, col = rangi2)
post <- extract.samples(m4.3)
a_map <- mean(post$a)
b_map <- mean(post$b)
curve(a_map + b_map*(x - xbar), add = TRUE)

# we look at the marginal posterior distributions of the parameters
precis(m4.3) # mean and standard deviations
round(vcov(m4.3), 3) # covariances
#pairs(m4.3) #
# samples from the posterior distribution (height)
post <- extract.samples(m4.3)

N <- 352
dN <- d2[1:N, ]
mN <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight - mean(weight)),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ), data = dN
)
# extract 20 samples from posterior 
post <- extract.samples(mN, n = 20)
# display raw data and sample size
plot(dN$weight, dN$height,
     xlim = range(d2$weight), ylim = range(d2$height),
     col = rangi2, xlab = "weight", ylab = "height")
mtext(concat("N = ", N))

# plot the lines from the sample, with transparency
for (i in 1:20)
  curve(post$a[i] + post$b[i]*(x - mean(dN$weight)),
        col = col.alpha("black", 0.3), add = TRUE)

# plotting regression intervals and contours
# first for weight = 50
post <- extract.samples(m4.3)
mu_at_50 <- post$a + post$b * (50 - xbar)
dens(mu_at_50, col = rangi2, lwd = 2, xlab = "mu|weight=50")
PI(mu_at_50, prob = 0.89)

mu <- link(m4.3)
str(mu)

# we want to compute the distribution of the height (mu) for each
# value of the weight 
# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq(from = 25, to = 70, by = 1) # 46 weight values
# use link to compute mu
# for each sample from posterior and for each weight in weight.seq
mu <- link(m4.3, data = data.frame(weight = weight.seq))
#str(mu)
plot(height ~ weight, d2, type = "n")
for (i in 1:100)
  points(weight.seq, mu[i,], pch = 16, col = col.alpha(rangi2, 0.1))

# summarize the distribution for each wight
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)

# plot the summaries on top of the data
# fading out points to make line and interval more visible
plot(height ~ weight, data = d2, col = col.alpha(rangi2, 0.5))
# plot the MAP line, aka the mean mu for each weight
lines(weight.seq, mu.mean)
# plot a shaded region for 89 % PI
shade(mu.PI, weight.seq)

# Prediction intervals R code 4.60
sim.height <- sim(m4.3, data = list(weight = weight.seq))
str(sim.height)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)
# plot raw data
plot(height ~ weight, d2, col = col.alpha(rangi2, 0.5))
# draw MAP line
lines(weight.seq, mu.mean)
# draw HPDI region for line
shade(mu.HPDI, weight.seq)
# draw PI region for simulated heights
shade(height.PI, weight.seq)

