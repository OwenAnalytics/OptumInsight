##################################################
#' Program name: data_LDADiagnosis_createDTM.R
#' 
#' Description: The program creates a docoment-term matrix
#' for use in LDA.
#' 
#' Author: Mengbing Li
#' 
#' Created: 12/18/2018
#' 
#' Revisions: 1/10/2019 - updated counts of the words. Previously
#'   all counts were 1.
#'   04/21/2019 - run LDA by month
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/data_LDADiagnosis")

library(Matrix)
library(tidytext)
library(tm)

load("diagnosisInformation_list.RData")

### Create a document-term matrix -----------------------------------------
# create a data frame to store the diagnosis

diagnosisText <- c()
for(patient in unique(diagnosisInformation$patients)){
  patientIndex <- which(diagnosisInformation$patients == patient)
  # get the codes
  codes <- diagnosisInformation$roots[patientIndex]
  
  # get the count of each code
  counts <- unname(do.call("c",
              lapply(diagnosisInformation$counts[patientIndex], `[[`, 1)))
  diagnoses <- paste(rep(codes, counts), sep = " ", collapse = " ")
  diagnosisText <- c(diagnosisText, diagnoses)
}
diagnosisText <- as.data.frame(diagnosisText)
colnames(diagnosisText) <- "diagnoses"
diagnosisTextCorpus <- Corpus(VectorSource(diagnosisText$diagnoses)) 
diagnosisText_dtm <- DocumentTermMatrix(diagnosisTextCorpus)

rm(list=setdiff(ls(), "diagnosisText_dtm"))
save(diagnosisText_dtm, file="diagnosisText_DTM.RData")
