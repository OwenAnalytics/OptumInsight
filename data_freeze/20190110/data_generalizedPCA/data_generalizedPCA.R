##################################################
#' Program name: data_generalizedPCA.R
#' 
#' Description: PCA on the count matrix of the number of 3-digit ICD-9 
#' codes by patient.
#' 
#' Author: Mengbing Li
#' 
#' Created: 01/19/2019
#' 
#' Revisions: 
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/data_generalizedPCA")

library(dplyr)
library(tidyr)
library(data.table)
library(knitr)
library(ggplot2)
library(Matrix)
library(generalizedPCA)

load(".RData")

# extract 3-digit ICD-9 codes
source("../functions/icd9_hierarchy.R")
diagInpatient <- fread("../diagInpatient_long.csv")
diagInpatient$icd9_3digits <- icd9_3digits(diagInpatient, icd9_raw)


### Document-term matrix -----------------------------------------------

# count the number of times each ICD-9 code appears in each patient
# and store the counts in a sparse matrix
diagInpatientCount <- table(diagInpatient[, c("patid", "icd9_3digits")])
dimensions <- dim(diagInpatientCount)
# diagInpatientCount <- matrix(diagInpatientCount, nrow = dimensions[1], ncol = dimensions[2])
diagInpatientCount <- Matrix(diagInpatientCount, sparse = TRUE)


# Poisson PCA
## not run:
# generalizedPCA(diagInpatientCount, k = 2, M = 4, family = "poisson")





### PCA ---------------------------------------------------

### Split the documents into training and test sets
## 75% of the sample size
trainSize <- floor(0.75 * nrow(diagInpatientCount))

set.seed(123)
trainInd <- sample(seq_len(nrow(diagInpatientCount)), size = trainSize)
train <- diagInpatientCount[trainInd, ]
test <- diagInpatientCount[-trainInd, ]

# perform PCA on training data
pc <- prcomp(train)

# summarize PCA results
library(factoextra) 
library(graphics)

# summary(pc) ## why does it not give a summary?
fviz_eig(pc, addlabels = TRUE)
loadings(pc) ## why does it not give a result?
head(pc$scores) ## no result either

# After doing PCA with training data, do prediction with new data points
predict(pc, data.frame(test)) ## no result??




### sparse PCA ------------------------------------------------
library(nsprcomp)
library(MASS)

spPCA <- nsprcomp(diagInpatientCount, nneg = TRUE, scale. = TRUE)
#' note: cannot perform nsprcomp on training set because there are
#' columns that are all zeros.









### Co-occurrence matrix ----------------------------------------
diagCO <- crossprod(table(diagInpatient[, c("fst_dt", "icd9_3digits")]))
diag(diagCO) <- 0
# view(diagCO)

#  perform SVD on the cooccurrence matrix -------------
SVD <- svd(diagCO)

plot(cumsum(SVD$d)/sum(SVD$d)*100,
     main='Cumulative proportion of variance explained')


SVD$u[1:10,1:10] # matrix of eigenvectors 
SVD$d[1:20] # vector of eigenvalues




# perform PCA on cooccurrence matrix --------------------
pc_CO <- prcomp(diagCO)

# summarize PCA results
# summary(pc) ## why does it not give a summary?
fviz_eig(pc_CO, addlabels = TRUE)
loadings(pc_CO) ## why does it not give a result?
head(pc$scores) ## no result either


pc_CO2 <- princomp(diagCO)
summary(pc_CO2) ## why does it not give a summary?
fviz_eig(pc_CO2, addlabels = TRUE)
sort(loadings(pc_CO2))[1:10]



# sparse PCA -------------------------------------
# cannot use a smaller data because otherwise some columns are all zeros
spPCA_CO <- nsprcomp(diagCO, nneg = TRUE, scale. = TRUE)


# Poisson PCA -----------------------
# full cooccurrence matrix
# gPCA <- generalizedPCA(diagCO, k = 2, M = 4, family = "poisson") ## takes too long to run
# summary(gPCA)

# a smaller cooccurrence matrix
diagCO_sub <- crossprod(table(diagInpatient[1:1000, c("fst_dt", "icd9_3digits")]))
diag(diagCO_sub) <- 0
gPCA_sub <- generalizedPCA(diagCO_sub, k = 2, M = 4, family = "poisson")
print(gPCA_sub)
summary(gPCA_sub)

# Fits the low dimensional matrix of either natural parameters or response.
fitted(gPCA_sub)[1:50] 
# Plots the deviance trace by the number of iterations
plot(gPCA_sub, type = "trace")
# Plots the first two PC loadings
plot(gPCA_sub, type = "loading", main = "First two PC loadings")
# Plots the first two PC scores
plot(gPCA_sub, type = "score", main = "First two PC scores")


save.image()
