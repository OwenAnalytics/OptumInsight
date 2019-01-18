##################################################
#' Program name: data_icd9TreeStructured_list.R
#' 
#' Description: The program looks for the number of
#' topics in LDA.
#' 
#' Author: Mengbing Li
#' 
#' Created: 12/18/2018
#' 
#' Revisions: 
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/data_LDADiagnosis")

library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(data.tree)
library(Matrix)
library(ggplot2)
library(tidytext)
library(tm)
library(topicmodels)

load("diagnosisInformation_list.RData")

### Create a document-term matrix -----------------------------------------
# create a data frame to store the diagnosis

diagnosisText <- c()
for(patient in unique(diagnosisInformation$patients)){
  patientIndex <- which(diagnosisInformation$patients == patient)
  diagnoses <- paste(diagnosisInformation$roots[patientIndex],
                     sep = " ", collapse = " ")
  diagnosisText <- c(diagnosisText, diagnoses)
}
diagnosisText <- as.data.frame(diagnosisText)
colnames(diagnosisText) <- "diagnoses"
diagnosisTextCorpus <- Corpus(VectorSource(diagnosisText$diagnoses)) 
diagnosisText_dtm <- DocumentTermMatrix(diagnosisTextCorpus)


### Determine the number of topics ------------------------------------------
library(ldatuning)
control_list_gibbs <- list(
  burnin = 2500,
  iter = 5000,
  seed = 0:4,
  nstart = 5,
  best = TRUE
)

system.time(
  topic_number_diagnosis <- FindTopicsNumber(
    diagnosisText_dtm,
    topics = c(seq(from = 2, to = 9, by = 1), seq(10, 20, 2), seq(25, 50, 5)),
    metrics = c( "Griffiths2004", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = control_list_gibbs,
    mc.cores = 4L,
    verbose = TRUE
  )
)

FindTopicsNumber_plot(topic_number_diagnosis)

rm(list=setdiff(ls(), c("topic_number_diagnosis", "diagnosisText_dtm")))
save(file="data_LDADiagnosis_selectk.RData")