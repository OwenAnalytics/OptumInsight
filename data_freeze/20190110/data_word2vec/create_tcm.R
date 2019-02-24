##################################################
#' Program name: create_ctm.R
#' 
#' Description: Create a term-cooccurrence matrix
#' for learning word embeddings 
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


# Since we only retain three digits of the ICD-9 codes, we 
# consider a patient can only be diagnosed with a code at most 
# once in a day
diagInpatient <- unique(diagInpatient[,.(patid, fst_dt, icd9_3digits)])

### Co-occurrence matrix ----------------------------------------
patids <- unique(diagInpatient$patid)
icd9codes <- sort(unique(diagInpatient$icd9_3digits))

# dimension of the co-occurrence matris
M <- length(icd9codes)
ctm <- Matrix(0, nrow = M, ncol = M,
              dimnames = list(icd9codes, icd9codes))
for(patient in patids){
  
  # create counting matrix on each day
  diagCounts0 <- table(diagInpatient[patid==patient, c("fst_dt", "icd9_3digits")])
  diagCounts0 <- as.matrix(diagCounts0 > 0)
  # replace logical with numeric values
  # diagCounts <- matrix(apply(diagCounts0, c(1,2), as.numeric),
  #                      nrow = nrow(diagCounts0))
  diagCounts <- apply(diagCounts0, c(1,2), as.numeric)
  # colnames(diagCounts) <- colnames(diagCounts0)
  
  # find out the icd9 codes that did not appear in this patient
  icd9codesToAdd <- setdiff(icd9codes, colnames(diagCounts))
  zerosToAdd <- matrix(0, nrow = nrow(diagCounts), ncol = length(icd9codesToAdd))
  colnames(zerosToAdd) <- icd9codesToAdd
  
  # add the zero columns to counting matrix
  diagCounts <- cbind(diagCounts, zerosToAdd)
  
  # re-order the columns
  # if the matrix has only one row, R automatically converts it into
  # a column vector, so we need to transpose it
  if(nrow(diagCounts)==1){
    diagCounts <- t(as.matrix(diagCounts[, icd9codes]))
  } else{
    diagCounts <- diagCounts[, icd9codes]
  }
  
  # create the co-occurrence matrix
  ctm <- ctm + Matrix(crossprod(diagCounts), sparse = TRUE)
}
diag(ctm) <- 0


save(ctm, file = "ctm.RData")
