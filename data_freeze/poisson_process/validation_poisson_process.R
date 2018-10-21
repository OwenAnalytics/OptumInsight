#' validates non-homogeneous Poisson point process generated
#' in poisson_point_process.R
#' Things to check: 
#' E[N(t)]~Poisson(m(t)) with mean m(t)
#' 1. Ensure the Dispersion = E[N(t)] / Var[N(t)] = 1.
#' 2. Match the rate function: E[N(t)] = m(t).

# args = commandArgs(trailingOnly=TRUE)
# 
# ## args is now a list of character vectors
# ## First check to see if arguments are passed.
# ## Then cycle through each element of the list and evaluate the expressions.
# if(length(args)==0){
#   stop("At least one argument must be supplied (input file).n", call.=FALSE)
#   print("No arguments supplied. Default values are supplied: ")
#   print("Number of paths to generate: 100")
#   print("Number of time points to examine: 200")
#   print("Intensity function lambda(t) = 2*t^2")
#   print("Time to stop: 10")
#   ##supply default values
#   n <- 100
#   n.cuts <- 200
#   lambda <- function(t) 2*t^2
#   Tmax <- 10
#   } else{
#     for(i in 1:length(args)){
#       # eval(parse(text=args[[i]]))
#       n = args[1]
#       n.cuts = args[2]
#       lambda = args[3]
#       Tmax <- args[4]
#   }
# }


## simulate an inhomogeneous 1-dim Poisson process -----------------------
## use thinning
## reference: Lewis and Shedler, 1979. Simulation of nonhomogeneous 
## poisson processes by thinning

# number of paths
n <- 2000
# maximum time
Tmax <- 10
# intensity function, where t is one-dimensional
lambda <- function(t) pmin(t/3, 1)

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



## Validation ----------------------------------------------------

# check dispersion -------------------

# number of time points t to look at
n.cuts <- 200
cuts <- seq(0, Tmax, length.out = n.cuts)

# store counts by each time point to a dataframe
recurrences <- data.frame(matrix(nrow = n.cuts, ncol = n+1))
recurrences[, 1] <- cuts
colnames(recurrences) <- c("time", paste0("path", 1:n))

## remove records at time 0 from X
df <- X[X[,2] != 0, ]

## Y stores counts by each cut point
Y <- data.frame(matrix(nrow = nrow(df), ncol = 2))
Y[, 1] <- df[, 1]
colnames(Y) <- c("path", "occurrences")
for(j in 1:n.cuts){
  # check if each event occurs by time t in cuts
  Y[, 2] <- (df[, 2] <= cuts[j])
  counts_by_t <- aggregate(occurrences ~ path, Y, sum)
  recurrences[j, 2:(n+1)] <- counts_by_t[, 2]
}

# calculate expectation and variance by each time point
recurrences$ENt <- rowMeans(recurrences[, 2:(n+1)])

RowVar <- function(x, ...) {
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2] - 1)
}

recurrences$VarNt <- RowVar(recurrences[, 2:(n+1)])

recurrences$dispersion <- recurrences$ENt / recurrences$VarNt
recurrences$mt <- sapply(recurrences$time, function(x) integrate(lambda, 0, x)$value)

pdf("dispersion_mean_pmin_t1_2000paths.pdf", width=8.5, height=5)
plot(x = recurrences$time, recurrences$dispersion, type = "l",
     ylim = c(0,1.2),
     xlab = "Time t", ylab = "E[N(t)] / Var[N(t)]",
     main = paste0("Sample dispersion with ", n, " paths \nIntensity f(t) = pmin(t/3, 1)"))
abline(h = 1, col="blue")



# check E[N(t)] = m(t) -----------------------------
plot(x = recurrences$mt, recurrences$ENt,
     xlab = "Mean m(t)", ylab = "E[N(t)]",
     main = paste0("Expected counts VS sample mean", n, " paths"))
abline(a = 0, b = 1, col = "red")
dev.off()