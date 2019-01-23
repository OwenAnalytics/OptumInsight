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

# extract 3-digit ICD-9 codes
source("../functions/icd9_hierarchy.R")
diagInpatient <- fread("../diagInpatient_long.csv")
diagInpatient$icd9_3digits <- icd9_3digits(diagInpatient, icd9_raw)

# count the number of times each ICD-9 code appears in each patient
# and store the counts in a sparse matrix
diagInpatientCount <- table(diagInpatient[, c("patid", "icd9_3digits")])
dimensions <- dim(diagInpatientCount)
diagInpatientCount <- matrix(diagInpatientCount, nrow = dimensions[1], ncol = dimensions[2])
diagInpatientCount <- Matrix(diagInpatientCount, sparse = TRUE)


# Poisson PCA
generalizedPCA(diagInpatientCount, k = 2, M = 4, family = "poisson")









