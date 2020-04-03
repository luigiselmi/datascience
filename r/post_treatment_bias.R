# R code 6.16 rethinking book
# We first build a set of simulated data then we build the model
# that will use the data.
# 1) data simulation
set.seed(71)
# number of plants
N <- 100

# simulates initial heights
h0 <- rnorm(N, 10, 2)

# assign treatments and simulate fungus and growth
treatment <- rep(0:1, each = N / 2)
fungus <- rbinom(N, size = 1, prob = 0.5 - treatment * 0.4)
h1 <- h0 + rnorm(N, 5 - 3 * fungus)

# compose a dataframe
d <- data.frame(h0 = h0, h1 = h1, treatment = treatment, fungus = fungus)

precis(d)

# 2) build the model that contains our hipothetical rule of plants' height
# p represents the proportion of growth. In this model it doesn't depends on 
# other predictors.
m6.6 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ) ,
    mu <- p * h0 , 
    p ~ dlnorm( 0 , 0.25 ) ,
    sigma ~ dexp(1)
  ) ,
  data = d
)

precis(m6.6)

# now we add the two predictors: treatment and fungus
m6.7 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ) ,
    mu <- p * h0 , 
    p ~ a + bt * treatment + bf * fungus ,
    a ~ dlnorm( 0 , 0.2 ) ,
    bt ~ dnorm(0, 0.5) ,
    bf ~ dnorm(0, 0.5) ,
    sigma ~ dexp(1)
  ) ,
  data = d
)
# from this model it looks like treatment doesn't have any effect on the plant's growth
precis(m6.7)
# If we want to know the impact of treatment on growth we must remove fungus as
# a predictor
m6.8 <- quap(
  alist(
    h1 ~ dnorm( mu , sigma ) ,
    mu <- p * h0 , 
    p ~ a + bt * treatment ,
    a ~ dlnorm( 0 , 0.2 ) ,
    bt ~ dnorm(0, 0.5) ,
    sigma ~ dexp(1)
  ) ,
  data = d
)

precis(m6.8)
# Let#s plot the directed acyclic graph (DAG) of the model
library(dagitty)
plant_dag <- dagitty("dag {
                H0 -> H1
                F -> H1
                T -> F
              }"
)
coordinates(plant_dag) <- list(x = c(H0 = 0, T = 2, F = 1.5, H1 = 1),
                               y = c(H0 = 0, T = 0, F = 1, H1 = 2))
plot(plant_dag)
dseparated(plant_dag, "T", "H1")
dseparated(plant_dag, "T", "H1", "F")
