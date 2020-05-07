# R code 5.34 rethinking book
library(rethinking)
data("Howell1")
d <- Howell1
str(d)

# we create an index variable to represent the sex
d$sex <- ifelse(d$male == 1, 2, 1)

# We build a model with an index variable as prior
m5.8 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a[sex] ,
    a[sex] ~ dnorm( 178 , 20 ) ,
    sigma ~ dunif(0, 50)
  ) ,
  data = d
)
precis(m5.8, depth = 2)

# Let's extract a sample from the posterior distribution
post <- extract.samples(m5.8)
post$diff_fm <- post$a[, 1] - post$a[, 2]
precis(post, depth = 2)

# Let#s use again the primate milk dataset
data("milk")
d <- milk
unique((d$clade))
d$clade_id <- as.integer(d$clade)
# we build a model to measure the average energy in each clade (cialda)
d$K <- scale(d$kcal.per.g)
m5.9 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a[clade_id] ,
    a[clade_id] ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp(1)
  ) ,
  data = d
)
labels <- paste("a[", 1:4, "]:", levels(d$clade), sep = "")
plot(precis(m5.9, depth = 2, pars = "a"), labels = labels)
