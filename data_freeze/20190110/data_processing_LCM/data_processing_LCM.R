##################################################
#' Program name: data_processing_LCM.R
#' 
#' Description: Create a sparse data matrix. The rows are patients.
#' Each column is a diagnosis code at 3, 4, or 5-digit level. The entry 
#' X_ij = 1 if patient i had at least one code j throughout his/her record,
#' and = 0 otherwise. 
#' 
#' Author: Mengbing Li
#' 
#' Created: 02/15/2019
#' 
#' Revisions:
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/data_processing_LCM")
# load(".RData")
library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(Matrix)

### 1. read confinement claims data --------------------------------------------------

confinement <- fread("../confinementClaims.txt", na.strings="")
colnames(confinement) <- tolower(colnames(confinement))
confinement$pos <- NULL
confinement$prov <- NULL
confinement$disch_dt <- NULL
confinement$admit_dt <- as.Date(confinement$admit_dt)

# extract confinement claims after index VTE and before 2015-10-01
confinement <- confinement[admit_dt >= index_dt &
                           admit_dt < as.Date("2015-10-01")]

# remove the null diagnosis codes
diag_names <- grep("diag", colnames(confinement), value = TRUE)
# change empty diagnoses to NA
for (j in diag_names) 
  set(confinement, j = j, value = gsub("^$|-|^0{3,5}$", NA, confinement[[j]]))



### transform confinement diagnoses into long format -------------------------------

confinement_long <- melt(confinement, 
                         id.vars = c("patid", "index_dt", "conf_id", "admit_dt", "charge", "copay"),
                         measure.vars = diag_names,
                         value.name = "icd9_raw",
                         variable.name = "diag",
                         na.rm = TRUE)

# remove the extra ICD-10 codes
# ICD-10 codes start with letters other than E or V, or have 
# length longer than 5 digits
icd10Start <- which(!substr(confinement_long$icd9_raw, 1, 1) %in% c(0:9, "E", "V"))
diagCodeLength <- nchar(trimws(confinement_long$icd9_raw))
icd10Length <- which(diagCodeLength > 5)
icd10Index <- unique(c(icd10Start, icd10Length))
icd10confinement <- sort(confinement_long$icd9_raw[icd10Index])
if(!is.null(icd10confinement)){
  print(unique(icd10confinement))
  print(length(icd10confinement))
  print(length(unique(icd10confinement)))
  
  # review these records
  confinement_long_icd10 <- confinement_long[icd10Index,]
  write.table(confinement_long_icd10, "ICD10_confinement_long.csv",
              quote = FALSE, sep = ",", row.names = FALSE)
  
  # remove ICD-10 records
  confinement_long <- confinement_long[-icd10Index,]
}


### create table ---------------------------------------------------------
crossTable <- table(confinement_long$patid, confinement_long$icd9_raw)
# re-order columns so that columns are grouped into 3, 4, and 5 digits
codenames <- colnames(crossTable)
codenames3 <- sort(codenames[which(nchar(codenames)==3)])
codenames4 <- sort(codenames[which(nchar(codenames)==4)])
codenames5 <- sort(codenames[which(nchar(codenames)==5)])
length(codenames) # 3626
length(codenames3) #69
length(codenames4) #1584
length(codenames5) #1973
crossTable <- crossTable[, c(codenames3,codenames4,codenames5)]

# convert into binary value
crossTable <- as.matrix(crossTable > 0)

# convert into sparse matrix
crossTable2 <- Matrix(crossTable, sparse = TRUE)

saveRDS(crossTable2, file = "data_LCM.rds")

dim(crossTable2) # 11455 x 3626
sum(crossTable2) # 117162


# readRDS("data_LCM.rds")
