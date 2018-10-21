## simulate a homogeneous Poisson process by time T_end ---------------
## interval times are independent exponential(lambda) 


# number of paths
n <- 1
# rate
lambda <- 0.1
# the end time
Tmax <- 50
# draw N independent times X ~ exp(lambda) when the
# total time is less than the end time
X <- c()
for(j in 1:n){
  total_time <- 0
  times <- 0
  while(TRUE){
    this.time <- rexp(1, rate = lambda)
    total_time <- total_time + this.time
    if (total_time > Tmax) break
    times <- c(times, total_time)
  }
  index.path <- rep(j, length(times))
  X <- rbind(X, cbind(index.path, times))
}

# find the number of the most frequent event
freq <- table(X[,1])
y.max <- freq[which.max(freq)]

plot(x = X[X[,1]==1, 2], y = 0:(sum(X[,1]==1)-1), type = "s",
     xlim = c(0, max(X[, 2])), ylim = c(0, y.max-1),
     xlab = "Time", ylab = "Cumulative count",
     main = paste("Homogeneous Poisson process paths with rate", lambda,
                  "by time", Tmax))
axis(2, at = 0:(y.max-1))
if(n > 1){
  for(j in 2:n){
    lines(x = X[X[,1]==j, 2], y = 0:(sum(X[,1]==j)-1), type = "s", col = j)
  }
}






## simulate an inhomogeneous 1-dim Poisson process -----------------------
## using thinning
## reference: Lewis and Shedler, 1979. Simulation of nonhomogeneous 
## poisson processes by thinning

# number of paths
n <- 1
# maximum time
Tmax <- 3
# intensity function, where t is one-dimensional
lambda <- function(t) 0.5*t^2 + t 
# dominating homogeneous poisson intensity
lamstar <- optimize(lambda, interval=c(0, Tmax), maximum=TRUE)$objective

X <- c()
for(j in 1:n){
  total_time <- 0
  times <- 0
  while(TRUE){
    # simulate a time from homogeneous pp with lamstar
    this.time <- rexp(1, rate = lamstar) 
    total_time <- total_time + this.time
    if (total_time > Tmax) break else{ # stop if maximum time is exceeded
      # otherwise accept the intensity function value with 
      # probability lambda(total_time) / lamstar
      if (runif(1, 0, 1) <= lambda(total_time)/lamstar){ 
        times <- c(times, total_time)
      }
    }
  }
  index.path <- rep(j, length(times))
  X <- rbind(X, cbind(index.path, times))
}

# find the number of the most frequent event
freq <- table(X[,1])
y.max <- freq[which.max(freq)]

plot(x = X[X[,1]==1, 2], y = 0:(sum(X[,1]==1)-1), type = "s",
     xlim = c(0, Tmax), ylim = c(0, y.max-1), yaxt="n",
     xlab = "Time", ylab = "Cumulative count",
     main = paste("Non-homogeneous Poisson process paths", 
                  "by time", Tmax))
axis(2, at = 0:(y.max-1))
if(n > 1){
  for(j in 2:n){
    lines(x = X[X[,1]==j, 2], y = 0:(sum(X[,1]==j)-1), type = "s", col = j)
  }
}











## simulate an inhomogeneous 2-dim Poisson process -----------------------
## based on Tim Johnson's note on Spatial Point Processes 
## but the note on simulating a pp at the bottm of page 4 is misleading


# 1. Define an upper bound λmax for the intensity function λ(s, t).
# 2. Simulate a homogeneous Poisson process with intensity λmax.
# 3. “Thin” the simulated process as follows,
# (a) Compute p = λ(s, t)/λmax for each point (s, t) of the homogeneous 
# Poisson process.
# (b) Generate a sample u from the uniform distribution on (0, 1).
# (c) Retain the locations for which u ≤ p.
# reference: https://people.smp.uq.edu.au/DirkKroese/ps/MCSpatial.pdf

# x = (x1, x2)

lambda <- function(x) (300*(x[,1]^2 + x[,2]^2))
lamstar <- 600
N <- rpois(1, lamstar)
x <- matrix(runif(N*2, 0, 1), ncol = 2)
ind <- which(runif(N) < lambda(x) / lamstar)
xa <- x[ind, ]
xa <- matrix(xa, ncol = 2)
plot(xa[,1], xa[,2], 
     xlab = "x1", ylab = "x2", 
     main = paste("Inhomogeneous spatial Poisson process"),
     xlim = c(0,1), ylim = c(0,1))







## simulate a homogeneous Poisson process with a fixed number of events --------
## interval times are independent exponential(lambda) 

# number of paths
n <- 1
# number of events
N <- 11
# rate
lambda <- 10
# draw N independent times X ~ exp(lambda)
X <- c()
for(j in 1:n){
  times <- rexp(N, rate = lambda)
  cum.times <- c(0, cumsum(times))
  index.path <- rep(j, N+1)
  X <- rbind(X, cbind(index.path, cum.times))
}

plot(x = X[X[,1]==1, 2], y = 0:N, type = "s",
     xlim = c(0, max(X[, 2])), ylim = c(0, N),
     xlab = "Time", ylab = "Cumulative count",
     main = paste("Homogeneous Poisson process paths with rate", lambda,
                  "and", N, "events"))
axis(2, at = 0:N)
if(n > 1){
  for(j in 2:n){
    lines(x = X[X[,1]==j, 2], y = 1:N, type = "s")
  }
}


