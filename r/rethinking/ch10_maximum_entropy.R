# R code 10.1 rethinking book
# Example 5 buckets, 10 pebbles.
p <- list()
# we define 5 different distributions
p$A <- c(0, 0, 10, 0, 0)
p$B <- c(0, 1, 8, 1, 0)
p$C <- c(0, 2, 6, 2, 0)
p$D <- c(1, 2, 4, 2, 1)
p$E <- c(2, 2, 2, 2, 2)

# we define the probability distribution by normalizing them
p_norm <- lapply(p, function(q) q / sum(q))

# We can compute the information entropy. We can see from it that distribution E
# is the most likely and has the biggest entropy.
(H <- sapply(p_norm, function(q) - sum(ifelse(q == 0, 0, q * log(q)))))