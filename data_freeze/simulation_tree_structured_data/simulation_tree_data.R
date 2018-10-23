##################################################
#' Program name: simulation_tree_data.R
#' 
#' Description: The program simulates tree-structured
#' ICD-9 codes.
#' 
#' Author: Mengbing Li
#' 
#' Created: 10/17/2018
#' 
#' Revisions: 10/21/2018 - Revise top-down structure of 
#' decreasing probability. Add temporal correlation.
#' 
##################################################

library(plyr)
library(MASS)

# create tree structure
mytree <- data.frame(matrix(c(rep(0, 10), # code does not appear
                              1, rep(0,9), # X1
                              1, 1, rep(0,8), # X1-X2
                              1, 1, 1, rep(0,7), # X1-X2-X3
                              1, 1, 0, 1, rep(0,6), # X1-X2-X4
                              1, 1, 0, 0, 1, rep(0,5), # X1-X2-X5
                              1, 1, 0, 0, 0, 1, rep(0,4), # X1-X2-X6
                              1, rep(0,5), 1, 0, 0, 0, # X1-X7
                              1, rep(0,5), 1, 1, 0, 0, # X1-X7-X8
                              1, rep(0,5), 1, 0, 1, 0, # X1-X7-X9
                              1, rep(0,8), 1), # X1-X10
                            ncol = 10, byrow = TRUE))

# create edge potential
prob <- c(20, # code does not appear
          40, 35, 25, 20, 25, 15, # X1, X1-X2, ...
          25, 20, 10, # X1-X7, ...
          5) # X1-X10
prob <- prob / sum(prob)


# create diagnosis data
N <- 50 # number of observations
n <- 1 # number of diagnosis codes
mydata <- data.frame(matrix(NA, nrow = N, ncol =10*n))
colnames(mydata) <- paste0("code", 1:n)


## introduce correlation between observatioins -----------

# Model: logit(pi_ij) = b_i ~ N_k (0, Sigma)

# generate a Toeplitz correlation structure
S <- toeplitz((N:1)/N)
bi <- mvrnorm(n = 1, mu = rep(0, N), Sigma = S)

pi_i <- 1 / (1+exp(-bi))

# assume that trees are independent for simplicity
for(j in 1:N){
  sampled <- mytree[sample(nrow(mytree), ceiling(n/10), replace = TRUE, prob = prob), ]
  mydata[j, ] <- as.vector(t(sampled))
}


# check sampling probabilities
counts <- count(test, vars = paste0("X", 1:10))
counts$freq / sum(counts$freq)


library(rpart)
fit <- rpart(Fraud ~ RearEnd, data = mytree, method = "class")




















