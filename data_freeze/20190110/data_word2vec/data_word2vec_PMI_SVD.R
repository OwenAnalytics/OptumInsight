##################################################
#' Program name: data_word2vec.R
#' 
#' Description: Learn ICD-9 code embeddings using word2vec
#' algorithm. The skip-gram model with negative sampling
#' (SGNS) is implicitly factorizing a shifted, positive
#' pointwise mutual information (PMI) matrix of 
#' word-context pairs.
#' PMI(w, c) = p(w,c) / (p(w) * p(c)),
#'   where p(w,c) = the number of times word w and 
#'   context-word c occur in the same context window,
#'   p(w) and p(c) are the singleton frequencies of w 
#'   and c, respectively.
#' We will factor the shifted positive pointwise mutual
#'  information matrix (SPPMI) to obtain word2vec
#'  embeddings, where
#' SPPMI(w,c) = max(PMI(w, c) - log(k), 0).
#' k = the number of negative samplings in word2vec
#' 
#' Author: Mengbing Li
#' 
#' Created: 02/10/2019
#' 
#' Revisions: 
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/data_word2vec")

library(dplyr)
library(tidyr)
library(data.table)
library(knitr)
library(ggplot2)
library(Matrix)
library(text2vec)

# extract 3-digit ICD-9 codes
source("../functions/icd9_hierarchy.R")
diagInpatient0 <- fread("../diagInpatient_long.csv")
diagInpatient0$icd9_3digits <- icd9_3digits(diagInpatient0, icd9_raw)
diagInpatient0$patid <- as.character(diagInpatient0$patid)

load("ctm.RData")

# Since we only retain three digits of the ICD-9 codes, we 
# consider a patient can only be diagnosed with a code at most 
# once in a day
diagInpatient <- unique(diagInpatient[,.(patid, fst_dt, icd9_3digits)])

### Co-occurrence matrix ----------------------------------------
patids <- unique(diagInpatient$patid)
icd9codes <- sort(unique(diagInpatient$icd9_3digits))

# dimension of the co-occurrence matrix
M <- length(icd9codes)

# calculates singleton frequencies
singletonFreq <- table(diagInpatient$icd9_3digits)

# calculate the PMI matrix
PMI <- Matrix(0, nrow = M, ncol = M)
# fill in the upper triangular part
for(i in 1:M){
  for(j in i:M){
    if(ctm[i,j] != 0){
      PMI[i,j] <- ctm[i,j] / (singletonFreq[i] * singletonFreq[j])
    }
  }
}
# copy the upper triangular part to the lower triangular part
PMI[lower.tri(PMI)] <- t(PMI)[lower.tri(PMI)]


# create SPPMI matrix
# specify the numbe of negative samples
k <- 1
SPPMI <- pmax(log(PMI*M) - log(k), 0)


## apply SVD on the SPPMI matrix
results <- svd(SPPMI)

summary(results)
plot( cumsum(results$d) / sum(results$d),
      main = "Cumulative proportion of variation explained")


W_SVD <- results$u %*% diag(sqrt(results$d))
C_SVD <- diag(sqrt(results$d)) %*% results$v

# image(ctm)
# heatmap(as.matrix(ctm))























### create a vocabulary ----------------------------------------------

# Create iterator over tokens
icd9codes_space <- paste(icd9codes, collapse = " ")
tokens = space_tokenizer(icd9codes_space)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it)

#' These words should not be too uncommon. For example we cannot 
#' calculate a meaningful word vector for a word which we saw 
#' only once in the entire corpus. Here we will take only words
#'  which appear at least five times. 
vocab = prune_vocabulary(vocab) # , term_count_min = 5L


ctm_matrix <- as.matrix(ctm)

ctm_dgTMatrix <- as(ctm_matrix, "dgTMatrix")

### factorize tcm matrix via the GloVe algorithm -----------------------

# text2vec uses a parallel stochastic gradient descent algorithm
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)

# fit_transform() obtains the main word vectors
wv_main = glove$fit_transform(ctm_dgTMatrix, n_iter = 10, convergence_tol = 0.01)
dim(wv_main)

summary(wv_main)



save.image()
