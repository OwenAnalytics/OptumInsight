########################################################################
#' Program name: data_checkRepeatedMedicalWithDifferentProviders.R
#' 
#' Description: The program checks whether rows with repeated diagnosis
#' codes in medical claims data differ in providers
#' 
#' Author: Mengbing Li
#' 
#' Created: 09/11/2018
#' 
#' Revisions:
########################################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20180830")

library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)

medical <- fread("medicalClaims.txt",
                 select = c("patid", "Conf_Id", paste0("Diag", 1:25), "Fst_Dt", "Prov"),
                 colClasses = list(character=c("patid", "Conf_Id", paste0("Diag", 1:25), "Fst_Dt"),
                                   numeric="Prov"))
colnames(medical) <- tolower(colnames(medical))
medical$fst_dt <- as.Date(medical$fst_dt)

# obtain rows with repeated diagnosis codes to see whether prov's are different
repeatedRowIndices <- duplicated(medical[,c("patid", paste0("diag", 1:25))]) |
  duplicated(medical[,c("patid", paste0("diag", 1:25))], fromLast = TRUE)
medical.repeatedDiag <- medical[which(repeatedRowIndices),]







