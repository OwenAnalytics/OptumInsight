##################################################
#' Program name: simulation_tree_data.R
#' 
#' Description: The program simulates a Dirichlet Process prior
#' 
#' Author: Mengbing Li
#' 
#' Created: 11/30/2018
#' 
##################################################

### Stick breaking construction ------------------------------
set.seed(2018)

alpha0 <- 3 # parameter for beta distribution
pi <- c() # probability vector
epsilon <- 1e-6 # error of approximating infinite process
V <- c() # stick-breaking process
sum_pi <- 0 # sum of the probability vector pi
G0 <- function(n) rnorm(n, 0, 10) # base measure

# construct the probability vector pi
while(TRUE){
  vj <- rbeta(1, 1, alpha0)
  V <- c(V, vj)
  if (length(V) == 1){
    pij <- vj # first draw
  } else {
    L <- length(V)
    pij <- V[L] * prod(1 - V[1:(L-1)])
  }
  pi <- c(pi, pij)
  sum_pi <- sum_pi + pij
  if(sum_pi > 1-epsilon) break
}

n <- length(pi) # number of clusters?
phi <- G0(n)


# How to deal with random measures?
theta <- sample(phi, prob = pi, replace = TRUE)
