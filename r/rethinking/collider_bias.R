# R code 6.22 rethinking book
library(rethinking)
d <- sim_happiness(seed = 1977, N_years = 1000)
precis(d)
d2 <- d[d$age > 17, ] # only adults
d2$A <- (d2$age - 18) / (65 - 18) # rescale age interval from 65-18 to 0-1
d2$mid <- d2$married + 1 # married = 2, not married = 1
m6.9 <- quap(
  alist(
    happiness ~ dnorm( mu , sigma ) ,
    mu <- a[mid] + bA * A , # here we consider marriage status
    a[mid] ~ dnorm(0, 1) ,
    bA ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ) ,
  data = d2
)

precis(m6.9, depth = 2)

m6.10 <- quap(
  alist(
    happiness ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A , # here we do not consider marriage status
    a ~ dnorm(0, 1) ,
    bA ~ dnorm(0, 2),
    sigma ~ dexp(1)
  ) ,
  data = d2
)

precis(m6.10)



