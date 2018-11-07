##################################################
#' Program name: combineData_wide.R
#' 
#' Description: The program checks whether the index_dt
#' are correct according to case definition. Patients
#' should not have VTE diagnoses within 12 months prior
#' to index VTE date.
#' 
#' Author: Mengbing Li
#' 
#' Created: 11/06/2018 
##################################################

library(data.table)
library(reshape2)
library(xlsx)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_checkVTEPriorToIndexdt")

## read in medical claims data --------------------------------------------------
medical <- fread("../diagData_20181020_freeze.csv",
                 colClasses = list(character = 1:73))

# filter diagnoses within one year before index VTE date
medical <- medical[fst_dt <= index_dt, ]
medical$index_dt <- as.Date(medical$index_dt)
medical$fst_dt <- as.Date(medical$fst_dt)
medical <- medical[fst_dt >= index_dt - 365, ]


# read in ICD9 codes for VTE

icd9_vte <- read.xlsx("../separateData/All_codes_ICD9_NDC_HCPCS.xlsx",
                      sheetName = "ICD9_VTE")
icd9_vte$ICD9_VTE <- as.character(icd9_vte$ICD9_VTE)

# transform diagnoses into long format
diag_names <- grep("diag", colnames(medical), value = TRUE)
diag.vars <- c("patid", "index_dt", diag_names, "fst_dt", "source")
medical_long <- melt(medical[, (diag.vars), with = FALSE],
                          id.vars = c("patid", "index_dt", 
                                      "fst_dt", "source"),
                          measure.vars = diag_names,
                          value.name = "icd9_raw",
                          variable.name = "diag",
                          na.rm = TRUE)
medical_long$diag <- NULL
medical_long$is.vte <- medical_long$icd9_raw %in% icd9_vte$ICD9_VTE

bad.patid <- unique(medical_long[is.vte==TRUE & fst_dt < index_dt, patid])
bad.data <- medical_long[patid %in% bad.patid & is.vte==TRUE,]

bad.data <- unique(bad.data[order(patid, fst_dt)])

write.csv(bad.data, "VTEBeforeIndexdt.csv")


# check individual data
medical.check <- medical[patid == "802666569186319",]
medical.check <- medical.check[order(patid, fst_dt),]
