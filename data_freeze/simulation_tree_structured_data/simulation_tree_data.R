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

# create a rooted tree structure ----------------------------------------
#' This example contains 10 nodes. Xi = 1 if present, and -1 if not present.
#' Each row contains a path. The 1's in each node indicate nodes on the path.
#' This creates a basis for sampling one diagnosis code.
basis <- data.frame(matrix(c(rep(-1, 10), # code does not appear
                              1, rep(-1,9), # X1
                              1, 1, rep(-1,8), # X1-X2
                              1, 1, 1, rep(-1,7), # X1-X2-X3
                              1, 1, -1, 1, rep(-1,6), # X1-X2-X4
                              1, 1, -1, -1, 1, rep(-1,5), # X1-X2-X5
                              1, 1, -1, -1, -1, 1, rep(-1,4), # X1-X2-X6
                              1, rep(-1,5), 1, -1, -1, -1, # X1-X7
                              1, rep(-1,5), 1, 1, -1, -1, # X1-X7-X8
                              1, rep(-1,5), 1, -1, 1, -1, # X1-X7-X9
                              1, rep(-1,8), 1), # X1-X10
                            ncol = 10, byrow = TRUE))


# create edge potential --------------------------------------------
# top-down potential structure

# the number of nodes
n <- ncol(basis)

# identify nodes at each level
leaf_level <- rowSums(basis)

# variables _path are row indices
# variables _node are column indices
zero_path <- which.min(leaf_level)
root_path <- which(leaf_level == -1*n + 2)
root_node <- which(basis[root_path, ] == 1)

level2_path <- which(leaf_level == -1*n + 4)
level3_path <- which(leaf_level == -1*n + 6)

n_leaves_at_each_node <- colSums(basis)


# assign potentials to basis paths
set.seed(2018)
basis_potential <- rgamma(n+1, 10, 1)

# create sampling matrix with potentials ---------------------------------
all_paths <- data.frame(matrix(NA, nrow = n+1, ncol=n+1))
colnames(all_paths) <- c(paste0("X", 1:n), "phi")

# add zero path
all_paths[1,] <- cbind(basis[zero_path,], basis_potential[zero_path])

# construct paths of lengths 2 and 3
# paths of length 2 
for(j in level2_path){
  
  # find the node at level 2 on this path
  level2_node <- which(basis[j,] == 1)
  
  # find the nodes at level 3 connected to this node
  level2_node_level3_nodes_logical <- apply(basis[level3_path, level2_node] == 1, 1, all)
  level2_node_level3_nodes <- as.numeric(names(which(level2_node_level3_nodes_logical)))
  
  # length(level2_node_level3_nodes) = 0 if this path contains only two nodes
  if(length(level2_node_level3_nodes) != 0){
    
    # add paths of the first level 2 node
    all_paths_nontrivial_rows <- sum(!is.na(all_paths$X1))
    all_paths[(1+all_paths_nontrivial_rows):(all_paths_nontrivial_rows+length(level2_node_level3_nodes)), ] <- 
      cbind(basis[level2_node_level3_nodes,], basis_potential[level2_node_level3_nodes])
    
    # add path of up to level 2, with level 3 unknown denoted by 0
    level2_path_level3_unknown <- rep(0, n)
    level2_path_level3_unknown[level2_node] <- 1
    level2_path_level3_unknown_potential <- 
      sum(basis_potential[level2_node_level3_nodes]) + basis_potential[j]
    
    # find the number of constructed path potentials
    all_paths_nontrivial_rows <- sum(!is.na(all_paths$X1))
    all_paths[all_paths_nontrivial_rows+1, ] <- 
      c(level2_path_level3_unknown, level2_path_level3_unknown_potential)
    
  } else { # add this path of length 2 
    all_paths_nontrivial_rows <- sum(!is.na(all_paths$X1))
    all_paths[all_paths_nontrivial_rows+1, ] <- 
      c(basis[j,], basis_potential[j])
  }
}


# calcualte potential of path of length 1
# find the potential of partially observed rows
partial_potential_index0 <- which(apply(all_paths[,1:n], 1, function(x) any(x==0)))
# find the index of completely observed paths of length 2
partial_potential_index2 <- which(rowSums(all_paths[,1:n], na.rm = TRUE) == -6)
partial_potential <- sum(all_paths$phi[c(partial_potential_index0, partial_potential_index2)]) +
  basis_potential[root_path]

# add this path
level1_path_other_unknown <- rep(0, n)
level1_path_other_unknown[root_node] <- 1

all_paths_nontrivial_rows <- sum(!is.na(all_paths$X1))
all_paths[all_paths_nontrivial_rows+1, ] <- 
  c(level1_path_other_unknown, partial_potential)




# Sample diagnosis codes --------------------------------------------------
lambda <- 3
k <- rpois(1, lambda)

sampled <- all_paths[sample(nrow(all_paths), k, replace = TRUE, prob = all_paths$phi), ]







# 
# 
# 
# 
# 
# # create diagnosis data
# N <- 50 # number of observations
# n <- 1 # number of diagnosis codes
# mydata <- data.frame(matrix(NA, nrow = N, ncol =10*n))
# colnames(mydata) <- paste0("code", 1:n)
# 
# 
# ## introduce correlation between observatioins -----------
# 
# # Model: logit(pi_ij) = b_i ~ N_k (0, Sigma)
# 
# # generate a Toeplitz correlation structure
# S <- toeplitz((N:1)/N)
# bi <- mvrnorm(n = 1, mu = rep(0, N), Sigma = S)
# 
# pi_i <- 1 / (1+exp(-bi))
# 
# # assume that trees are independent for simplicity
# for(j in 1:N){
#   sampled <- mytree[sample(nrow(mytree), ceiling(n/10), replace = TRUE, prob = prob), ]
#   mydata[j, ] <- as.vector(t(sampled))
# }
# 
# 
# # check sampling probabilities
# counts <- count(test, vars = paste0("X", 1:10))
# counts$freq / sum(counts$freq)
# 
# 
# library(rpart)
# fit <- rpart(Fraud ~ RearEnd, data = mytree, method = "class")
# 



















