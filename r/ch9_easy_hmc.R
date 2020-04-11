# R code 9.9 rethinking book
# Example using Hamiltonian Monte Carlo
library(rethinking)
data("rugged")
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000), ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse(dd$cont_africa == 1, 1, 2)

dat_slim <- list(
  log_gdp_std = dd$log_gdp_std,
  rugged_std = dd$rugged_std,
  cid = as.integer(dd$cid)
)

str(dat_slim)

m9.1 <- ulam(
  alist(
    log_gdp_std ~ dnorm( mu , sigma ) ,
    mu <- a[cid] + b[cid] * (rugged_std - 0.215), 
    a[cid] ~ dnorm(1, 0.1) ,
    b[cid] ~ dnorm(0, 0.3),
    sigma ~ dexp(1)
  ) ,
  data = dat_slim,
  chains = 4,
  cores = 4,
  iter = 1000
)

precis(m9.1, depth = 2)

pairs(m9.1)
traceplot(m9.1, chains = 1)

