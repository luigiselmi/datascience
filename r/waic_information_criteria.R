# R code 7.33 rethinking book
library(rethinking)
# We want to investigate the influence of body mass (M) and 
# brain volume (B) on longevity (L)
data("Primates301")
d <- Primates301
# we standardize the three variables we are going to use
d$log_L <- scale(log(d$longevity))
d$log_B <- scale(log(d$brain))
d$log_M <- scale(log(d$body))
# we look for missing values first
sapply(d[, c("log_L", "log_B", "log_M")], function(x) sum(is.na(x)))
d2 <- d[complete.cases(d$log_L, d$log_M, d$log_B), ] # removed rows with missing values
# Let's define the model to infer the direct influence of brain volume (B) on longevity (L).
# According to the causal graph we have to control the body mass variable (M) to close a 
# (pipe) backdoor M->B->L. Controlling a variable means adding it to the model.
m7.8 <- quap(
  alist(
    log_L ~ dnorm( mu , sigma ) ,
    mu <- a + bM *  log_M + bB * log_B, 
    a ~ dnorm(0, 0.1) ,
    bM ~ dnorm(0, 0.5),
    bB ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ) ,
  data = d2
)
# We also define two simpler models to evaluate the accuracy of each one.
m7.9 <- quap(
  alist(
    log_L ~ dnorm( mu , sigma ) ,
    mu <- a + bB * log_B, 
    a ~ dnorm(0, 0.1) ,
    bB ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ) ,
  data = d2
)
m7.10 <- quap(
  alist(
    log_L ~ dnorm( mu , sigma ) ,
    mu <- a + bM *  log_M , 
    a ~ dnorm(0, 0.1) ,
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ) ,
  data = d2
)
# We compare the Widely Applicable Information Criterion (WAIC) of all the 
# models. The WAIC provides an approximation of the out-of-sample deviance
# of a model. The smaller WAIC value the better because it means the model 
# is closer to the target one.
set.seed(301)
compare(m7.8, m7.9, m7.10)
# Let's compare the posterior distributions of the models' parameters
coeftab(m7.8, m7.9, m7.10)
# Let's see how body mass and brain volume correlate
cor(d2$log_B, d2$log_M)
