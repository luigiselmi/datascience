# R code 9.1 rethinking book
# King Markov decision procedure
num_weeks <- 1e5
positions <- rep(0, num_weeks)
current <- 10
for (i in 1:num_weeks) {
  # record current position
  positions[i] <- current
  
  # flip coin to generate proposal
  proposal <- current + sample(c(-1,1), size = 1)
  # make sure he loops around the archipelago
  if (proposal < 1) proposal <- 10
  if (proposal > 10) proposal <- 1
  
  # move ?
  prob_move <- proposal / current
  current <- ifelse(runif(1) < prob_move, proposal, current)
}
plot( 1:1000 , positions[1:1000] )
plot( table( positions ) )