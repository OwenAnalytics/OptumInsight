##################################################
#' Program name: data_CTMDiagnosis_model.R
#' 
#' Description: The program runs a correlated topic
#' model on the diagnosis data.
#' 
#' Author: Mengbing Li
#' 
#' Created: 12/19/2018
#' 
#' Revisions: 
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_CTMDiagnosis")

library(dplyr)
library(tidyr)
library(data.table)
library(Matrix)
library(tidytext)
library(tm)
library(topicmodels)

load("diagnosisText_DTM.RData")

### Run the CTM model -----------------------------------------------------

# specify number of topics
k <- c(2, 10, 20)

SEED <- 2018

diagnosisCTM <- list()
for(j in k){
  diagnosisCTM[paste("ctm", j, sep="")] <- 
    CTM(diagnosisText_dtm, k = j, method = "VEM",
        control = list(
          seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3)))
}


rm(list=setdiff(ls(), "diagnosisCTM"))
save(diagnosisCTM, file="diagnosisCTM.RData")


load("diagnosisCTM.RData")
