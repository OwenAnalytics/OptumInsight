##################################################
#' Program name: simulation_tree_icd9.R
#' 
#' Description: The function samples ICD-9 codes from the input 
#' tree structure.
#' Each ICD-9 code is a rooted tree with binary nodes. We need to 
#' create two levels of conditional probability distribution tables,
#' in which P(Y_c^(j) = y | Y_Pa(c)^{j-1} = 0) = 0 for y = 0, 1 and j = 2,3.
#' This means that if a node is not selected, then its children will not 
#' be selected. The joint distribution of all children of a node is modeled
#' as multivariate bernoulli.
#' 
#' Author: Mengbing Li
#' 
#' Created: 02/22/2019
#' 
#' Revisions:
##################################################

library(dplyr)
library(tidyr)
library(data.table)
library(MCMCpack)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110")

codeList <- readRDS("icd9_CompleteCodeList.rds")

# extract nodes in three levels 
source("functions/icd9_hierarchy.R")
codeList$root <- icd9_3digits(codeList, code)
codeList$icd9_4digits <- icd9_4digits(codeList, code, dot = FALSE)
codeList$icd9_5digits <- icd9_5digits(codeList, code, dot = FALSE)


### sample from ICD-9 code 564 ------------------------------------------------
# to visualize its tree structure:
# source("draw_tree.R")
# draw_tree("564")

# obtain the node labels and the code structure data
rootName <- "564"
subdata <- codeList[codeList$root==rootName,]



# set.seed(2039)
# simulate the root probability
pm <- runif(1)


# simulate the conditional probability distribution at level 2
level2Names <- unique(subdata$icd9_4digits)
n_level2Nodes <- length(level2Names) # number of nodes at level 2


#' n = number of columns
#' returns a multivariate binary matrix containing all combinations of a 
#' multivariate Bernoulli variable of dimension n
createCPDCombinations <- function(n){
  
  # create a multivariate binary table where the sum of each row is m
  # n = number of columns
  # m = total number of 1's in a row
  if(n==0) return(NULL) else{
    partialCombinations <- function(m,n){
      t(combn(n, m, tabulate, nbins=n))
    }
    
    # create a partial binary table for each 1:n
    partialCombinations_list <- lapply(0:n, partialCombinations, n)
    
    # combine all the partial binary tables
    do.call("rbind", partialCombinations_list)
  }
} 


# create a vector of conditional probabilities
# X is a binary matrix. The probability vector will be added to the last column of X
addProbability <- function(X){
  X <- cbind(X, c(rdirichlet(1, rep(1, nrow(X)))))
  colnames(X)[ncol(X)] <- "p"
  return(X)
}




### Create a conditional probability distribution for level 2 nodes ---------

Level2CPD <- createCPDCombinations(n_level2Nodes)
colnames(Level2CPD) <- level2Names
# add a vector of conditional probabilities
Level2CPD <- addProbability(Level2CPD)



### Create a conditional probability distribution for level 3 nodes ---------

level3Names <- unique(subdata$icd9_5digits[subdata$icd9_5digits!=""])
# count the number of children of each level 2 node
n_Level2Children <- table(subdata$icd9_4digits)
# a level 2 node that appears once has no children
# n_Level2Children[n_Level2Children==1] <- 0
n_Level2Children <- n_Level2Children[n_Level2Children!=1]
Level2NodesHavingChildren <- names(n_Level2Children)

# the level 3 cpd's are stored in a list, where the element names are the level 2 node labels
Level3CPD <- list()
for(j in 1:length(n_Level2Children)){
  Level3CPD[[j]] <- createCPDCombinations(n_Level2Children[j])
  
  # add level 3 labels to the binary tables
  level3LabelIndex <- cumsum(n_Level2Children)
  if(j == 1) {
    colnames(Level3CPD[[j]]) <- level3Names[1:level3LabelIndex[1]]
  } else{
    colnames(Level3CPD[[j]]) <- level3Names[(level3LabelIndex[j-1]+1):level3LabelIndex[j]]
  }
}
names(Level3CPD) <- Level2NodesHavingChildren

Level3CPD <- lapply(Level3CPD, addProbability)




### Sample codes from a tree ---------------------------------------------------

# x is a CPD table
sampleCodes <- function(x){
  sampledRowIndex <- sample(1:nrow(x), 1, prob = x[,ncol(x)])
  names(which(x[sampledRowIndex,]==1))
}

sampledCodes <- c()
Y1 <- rbinom(1,1,pm)
if(Y1==1L){ # if this tree is sampled, then proceed to higher level
  sampledCodes <- rootName
  Level2SampledCodes <- sampleCodes(Level2CPD)
  
  if(length(Level2SampledCodes) != 0){ # if some level 2 codes are sampled, then proceed
    # if some level 2 codes are sampled, then we remove the root from the sampled code
    sampledCodes <- Level2SampledCodes
    # set.seed(2009)
    
    # note that some sampled level 2 codes may have no children
    # proceed only if the sampled level 2 codes have children
    Level2SampledCodesHavingChildren <- sampledCodes[sampledCodes %in% Level2NodesHavingChildren]
    if(length(Level2SampledCodesHavingChildren) != 0){
      Level3SampledCodes <- lapply(Level3CPD[Level2SampledCodesHavingChildren], sampleCodes)

      for(j in 1:length(Level3SampledCodes)){
        # if some level 3 codes are sampled, then remove the level 2 parent and 
        # add the sampled level 3 codes to sampledCodes
        if(length(Level3SampledCodes[[j]]) != 0){
          Level2IndexToRemove <- which(sampledCodes==names(Level3SampledCodes[j]))
          sampledCodes <- sampledCodes[-Level2IndexToRemove]
          sampledCodes <- c(sampledCodes, Level3SampledCodes[[j]])
        }
      }
    }

  }
}
sampledCodes

