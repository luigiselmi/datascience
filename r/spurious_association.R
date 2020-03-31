# R code 5.1 rethinking book
# load data
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce

# standardize variables
d$A <- scale(d$MedianAgeMarriage)
d$D <- scale(d$Divorce)

# 1) builds the model divorce rate D - age of marriage A
m5.1 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ) ,
  data = d 
)

# plot the priors
set.seed(10)
prior <- extract.prior(m5.1)
mu <- link(m5.1, post = prior, data = list(A = c(-2,2)))
plot(NULL, xlim = c(-2,2), ylim = c(-2,2))
for (i in 1:50)
  lines(c(-2,2), mu[i, ], col = col.alpha("black", 0.4))

# compute percentile interval of mean
A_seq <- seq(from = -3, to = 3.2, length.out = 30)
mu <- link(m5.1, data = list(A = A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot the posterior predictions
plot(D ~ A, data = d, col = rangi2)
lines(A_seq, mu.mean, lwd = 2)
shade(mu.PI, A_seq)

precis(m5.1)

# 2) builds the model divorce rate D - marriage rate M
# standardize variable marriage rate
d$M <- scale(d$Marriage)
m5.2 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ) ,
  data = d 
)

# compute percentile interval of mean
M_seq <- seq(from = -3, to = 3.2, length.out = 30)
mu <- link(m5.2, data = list(M = M_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

# plot the posterior predictions
plot(D ~ M, data = d, col = rangi2)
lines(M_seq, mu.mean, lwd = 2)
shade(mu.PI, M_seq)

# draw a directed acyclic graph (DAG) that represents
# a causal relationship between the variables
#install.packages('dagitty')
library(dagitty)
dag5.1 <- dagitty("dag {
  A -> D
  A -> M
  M -> D
  }"
)
coordinates(dag5.1) <- list(x = c(A = 0, D = 1, M = 2), y = c(A = 0, D = 1, M = 0))
plot(dag5.1)

# 3) multiple regression model
m5.3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M + bA * A,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ) ,
  data = d 
)
precis(m5.3)
# plot the posterior distributions of the two weights (parameters)
# for age of marriage (bA) and marriage rate (bM) to see the changes
# from bivariate models (m5.1, m5.2) to a multivariate model (m5.3)
# compute percentile interval of mean
plot(coeftab(m5.1, m5.2, m5.3), par = c("bA", "bM"))
