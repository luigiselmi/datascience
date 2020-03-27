# R code 4.7 rethinking book
library(rethinking)
data("Howell1")
d <- Howell1 # dataframe
precis(d)

d2 <- d[d$age >= 18, ]
