# R code from snippet 4.1 (rethinking book).
# We look at different processes that end up in a normal distribution.
# 1) Normal by addition
# We imagine an individual that takes a certain number of steps, 
# whose length is taken from the uniform distribution within an 
# interval between -1 and 1, and finally, we look at its final 
# position that means we sum up all the steps.
# We repeat this experiment 1000 times. All this can be
# done in R with one single line of code.
pos <- replicate(1000, sum(runif(16, -1, 1)))

# The important result is that "adding together random values
# from the same distribution, uniform or others, converges to 
# a normal distribution.
#hist(pos) # uncomment to plot

# 2) Normal by multiplication
# Other processes can be described by a multiplication of values 
# that fluctuate randomly by a small amount, e.g. 0.1, about a 
# central point
deviation <- 0.1
growth <- replicate(10000, prod(1 + runif(12, 0, 0.1)))

# We still get a normal distribution for the process.
library(rethinking)
#dens(growth, norm.comp = TRUE) # uncomment to plot

# 3) Normal by log-multiplication
# Multiplying large random deviations are not normal distributed
# but they logarithm are since they are transformed in additions.
deviation <- 0.5
log.big <- replicate(10000, log(prod(1 + runif(12, 0, 0.5))))
#dens(log.big, norm.comp = TRUE) # uncomment to plot                     


curve(exp(-x^2), from = -3, to = 3) # Gaussian distribution