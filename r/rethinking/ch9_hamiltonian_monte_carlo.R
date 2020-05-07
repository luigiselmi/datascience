# R code 9.3 rethinking book
# Hamiltonian Monte Carlo example.
# In this example we'll use a two variable model. 
# We need:
# 1) a function of the log-probability
# 2) the gradient
# 3) the step size
# 4) the number of leapfrog steps

# function U of the log-probability of the data.
# Returns the neg-log-probability
U <- function(q, a = 0, b = 1, k = 0, d = 1) {
  muy <- q[1]
  mux <- q[2]
  U <- sum(dnorm(y, muy, 1, log = TRUE)) + sum(dnorm(x, mux, 1, log = TRUE)) +
    dnorm(muy, a, b, log = TRUE) + dnorm(mux, k, d, log = TRUE)
}

# U gradient function
# Sum of partial derivatives with respect to parameters mux and muy
U_gradient <- function(q, a = 0, b = 1, k = 0, d = 1) {
  muy <- q[1]
  mux <- q[2]
  G1 <- sum(y - muy) + (a - muy) / b^2 # dU/d(muy)
  G2 <- sum(x - mux) + (k - mux) / d^2 # dU/d(mux)
  return(c(-G1, -G2)) # negative because energy is neg-log-prob
}
# test data
y <- rnorm(50)
x <- rnorm(50)
x <- as.numeric(scale(x))
y <- as.numeric(scale(y))

library(shape) # for fancy arrows
Q <- list()
Q$q <- c(-0.1,0.2)
pr <- 0.3
plot( NULL , ylab="muy" , xlab="mux" , xlim=c(-pr,pr) , ylim=c(-pr,pr) )
step <- 0.03

L <- 11 # 0.03/28 for U-turns --- 11 for working example
n_samples <- 4
path_col <- col.alpha("black",0.5)
points( Q$q[1] , Q$q[2] , pch=4 , col="black" )
for ( i in 1:n_samples ) {
  Q <- HMC2( U , U_gradient , step , L , Q$q )
  if ( n_samples < 10 ) {
    for ( j in 1:L ) {
      K0 <- sum(Q$ptraj[j,]^2)/2 # kinetic energy
      lines( Q$traj[j:(j+1),1] , Q$traj[j:(j+1),2] , col=path_col , lwd=1+2*K0 )
    }
    points( Q$traj[1:L+1,] , pch=16 , col="white" , cex=0.35 )
    Arrows( Q$traj[L,1] , Q$traj[L,2] , Q$traj[L+1,1] , Q$traj[L+1,2] ,
            arr.length=0.35 , arr.adj = 0.7 )
    text( Q$traj[L+1,1] , Q$traj[L+1,2] , i , cex=0.8 , pos=4 , offset=0.4 )
  }
  points( Q$traj[L+1,1] , Q$traj[L+1,2] , pch=ifelse( Q$accept==1 , 16 , 1 ) ,
          col=ifelse( abs(Q$dH)>0.1 , "red" , "black" ) )
}