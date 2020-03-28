# R code 4.37 rethinking book
library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18,]
plot(d2$height ~ d2$weight)