##################################################
#' Program name: simulation_recurrentEvent.R
#' 
#' Description: Simulate one-dimensional Hawkes 
#' process.
#' 
#' Reference: Exact simulation of Hawkes process 
#' with exponentially decaying intensity. Electron. 
#' Commun. Probab 2013. Angelos Dassios, Hongbiao Zhao.
#' 
#' Author: Mengbing Li
#' 
#' Created: 11/14/2018
#' 
##################################################



## One-dimensional Hawkes process with exponentially 
## decaying intensity conditional on baseline intensity ---------------

# let the length total time be L = 10
L <- 100

## Initial condition 
time0 <- 0 # initial time
lambda0 <- 0.8 # initial intensity

# intensity of the counting process
# at baseline, left limit = right limit = lambda0
lambda_minus <- lambda0 
lambda_plus <- lambda0 

delta <- 3 # rate of exponential decay
a <- 0.3 # constant reversion level, a < lambda

jumpTimes <- time0 # time1 = time of the next jump
los <- c()
N0 <- 0 # counting process
seed <- 1 # to use in generating random numbers
while(jumpTimes[length(jumpTimes)] <= L){
  
  ## Simulate the interarrival times
  set.seed(seed)
  u0 <- runif(1, 0, 1)
  s0 <- -1/a * log(u0)
  set.seed(seed+100)
  u1 <- runif(1, 0, 1)
  d <- 1 + delta * log(u1) / (lambda_plus - a)
  
  if (d > 0){
    s1 <- -1/delta * log(d)
    interarrivalTime <- min(s0, s1)
  } else interarrivalTime <- s0
  
  ## record the next jump time
  currentJumpTime <- jumpTimes[length(jumpTimes)]
  nextJumpTime <- currentJumpTime + interarrivalTime
  if ( nextJumpTime > L) break
  jumpTimes <- c(jumpTimes, nextJumpTime)
  
  ## change at nextJumpTime in the intensity with exponential decay
  lambda_minus <- (lambda_plus-a)*exp(-delta*interarrivalTime)+a

  lambda_plus <- lambda_minus + 1
  
  ## record the change at nextJumpTime in the counting process
  N0 <- N0 + 1
  
  seed <- seed + 1
  
}

plot(x = jumpTimes, y = 1:length(jumpTimes), type = "s",
     xlab = "Time", ylab = "Cumulative count",
     main = "Hawkes process path")





# Plot intensity function -------------------------------------------------
n_gridPoints <- 10000
times <- seq(0, L, length.out = n_gridPoints)

intensity <- rep(NA, n_gridPoints)
for(i in 1:n_gridPoints){
  # find the jumpTime < t
  beforeIndex <- which(jumpTimes < times[i])
  if (length(beforeIndex) == 0){
    intensity[i] <- a + (lambda0-a)*exp(-delta*times[i])
  } else{
    maxJumpTimeIndex <- max(beforeIndex)
    exp.sum <- sum(exp(delta*jumpTimes[1:maxJumpTimeIndex]))
    intensity[i] <- a + (lambda0-a+exp.sum)*exp(-delta*times[i])
  }
}

plot(x = times, y = intensity, type = 'l',
     xlab = "Time", y = "Intensity")






