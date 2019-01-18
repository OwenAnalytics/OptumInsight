##################################################
#' Program name: data_icd9TreeStructured_list.R
#' 
#' Description: The program summarizes the tree-structured
#' ICD-9 codes for individual patients.
#' The output is a list containing medical diagnoses of each
#' patient, including code structures and counts.
#' 
#' Author: Mengbing Li
#' 
#' Created: 11/24/2018
#' 
#' Revisions: 
#' 12/06/2018 - 1. Add names of the ICD-9 codes;
#'  2. Find out diagnosis codes that are not available in the ICD-9 dictionary
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/data_LDADiagnosis")

library(tm)
library(topicmodels)

load("diagnosisText_DTM.RData")


### Run the LDA model -----------------------------------------------------

# specify number of topics
k <- c(2, 10, 20)

control_list_gibbs <- list(
  burnin = 1000,
  iter = 1000,
  thin = 100,
  seed = seq(length.out = length(k)),
  nstart = length(k),
  best = TRUE
)

diagnosisLDA <- list()
for(j in k){
  diagnosisLDA[paste("lda", j, sep="")] <- 
    LDA(diagnosisText_dtm, k = j, method = "Gibbs", control = control_list_gibbs)
}


rm(list=setdiff(ls(), "diagnosisLDA"))
save(diagnosisLDA, file="diagnosisLDA.RData")
