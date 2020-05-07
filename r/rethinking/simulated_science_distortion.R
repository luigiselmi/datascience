# R code 6.1 rethinking book
set.seed(1914)
N <- 200 # num grant proposals
P <- 0.1 # proportion to select
# uncorrelated newsworthiness and trustworthiness
nw <- rnorm(N)
tw <- rnorm(N)
# select top 10 % of combined scores
s <- nw + tw # total score
q <- quantile(s, 1 - p) # top 10 % threshold
selected <- ifelse(s >= q, TRUE, FALSE)
cor(tw[selected], nw[selected])
plot(tw[selected], nw[selected])
