# R code 6.2 rethinking book
# We set up a simulation to showcase the problem when using predictors 
# that are strongly correlated.
N <- 100
set.seed(909)
height <- rnorm(N, 10, 2) # total height
leg_prop <- runif(N, 0.4, 0.5) # leg as proportion of height
leg_left <- leg_prop * height + rnorm(N, 0, 0.02) # left leg as proportion + error
leg_right <- leg_prop * height + rnorm(N, 0, 0.02) # right leg as proportion + error
d <- data.frame(height, leg_left, leg_right)

m6.1 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl *  leg_left + br * leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp(1)
  ) ,
  data = d
)

#plot(precis(m6.1)) # doesn't show what it should
post <- extract.samples(m6.1)
plot(bl ~ br, post, col = col.alpha(rangi2, 0.1), pch = 16)

sum_blbr <- post$bl + post$br
dens(sum_blbr, col = rangi2, lwd = 2, xlab = "sum of bl and br")

# Now we show the problem using real data, when one might not know in 
# advance that two predictor are in fact strongly correlated.
# We will model the dependency of the milk total energy content (K)
# from fat (F) and lactose (L). 
library(rethinking)
data(milk)
d <- milk
d$K <- scale(d$kcal.per.g) # Kilocalories (energy content)
d$F <- scale(d$perc.fat) # Fat
d$L <- scale(log(d$perc.lactose)) # lactose (a carbohidrate)
# we start by creating two bivariate models. The first investigates 
# the dependency of kilocalories (K) from fat (F)
m6.3 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bF * F ,
    a ~ dnorm( 0 , 0.2 ) ,
    bF ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ) ,
  data = d
)
# The 2nd model investigates the dependency of kilocalories (K) from lactose (L)
m6.4 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bL * L,
    a ~ dnorm( 0 , 0.2 ) ,
    bL ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ) ,
  data = d
)
# mean values show strong correlation of fat (positive) and lactose (negative)
# with kilocalories.
precis(m6.3)
precis(m6.4)
# Now we build a multivariate regression model for kilocalories using fat and lactose together.
m6.5 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bF * F + bL * L,
    a ~ dnorm( 0 , 0.2 ) ,
    bF ~ dnorm( 0 , 0.5 ) ,
    bL ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ) ,
  data = d
)
# mean values of the two parameter are now smaller that would imply a weaker contribution to energy 
# and also less precise as the standard deviations are bigger
precis(m6.5)
pairs(~ kcal.per.g + perc.fat + perc.lactose, data = d, col = rangi2) # plot the correlations between each pair of variables
cor(d$perc.fat, d$perc.lactose) # computes the correlation between fat and lactose


