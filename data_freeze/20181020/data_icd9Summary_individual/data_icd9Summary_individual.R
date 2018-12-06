##################################################
#' Program name: data_icd9Summary_individual.R
#' 
#' Description: The program summarizes individual-level
#' ICD-9 patterns. We ignore the temporal effect.
#' 
#' Author: Mengbing Li
#' 
#' Created: 12/05/2018
#' 
#' Revisions:
##################################################

setwd('C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_icd9TreeStructured')

library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(Matrix)

#' patterns:
#' 3-digit  4-digit  5-digit  pattern_code
#'       0        0        0             0
#'       1        0        0             1
#'       1        1        0             2
#'       1        1        1             3
#' output data:
#' columns: roots of the ICD-9 trees
#' rows: individual patients
#' cells: pattern_code

load("diagnosisInformation_list.RData")

# initialize data frame
roots <- diagnosisInformation$roots
patients <- unique(diagnosisInformation$patients)
patternInformation <- data.frame(matrix(NA, ncol = length(roots), nrow = length(patients)))
rownames(patternInformation) <- patients
colnames(patternInformation) <- roots



# the number of patients in the data is less than 14932
# checking whether some patients were left out becuase they only appeared
#   in confinement claims

### read in confinement data -------------------------------------------
confinement <- fread("../separateData/confinementClaims.txt",
                     colClasses = list(character = c("patid")))
colnames(confinement) <- tolower(colnames(confinement))
length(setdiff(confinement$patid, patients))


testIndex <- which(diagnosisInformation$patients=="802666500100523")
diagnosisInformation$roots[testIndex]




