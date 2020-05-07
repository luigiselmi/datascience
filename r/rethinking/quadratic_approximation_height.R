# R code 4.26 rethinking book
library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18,]

# model definition
flist <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(156, 10),
  sigma ~ dunif(0, 50)
)

m4.1 <- quap(flist, data = d2)
precis(m4.1)

# variance-covariance matrix
vcov(m4.1)
# variances
diag(vcov(m4.1))
cov2cor(vcov(m4.1))

# samples from the multi-dimensional posterior
post <- extract.samples(m4.1, n = 1e4)
head(post)
precis(post)