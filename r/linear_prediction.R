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


