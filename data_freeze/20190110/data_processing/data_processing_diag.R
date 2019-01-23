##################################################
#' Program name: data_processing.R
#' 
#' Description: 
#' 1. create an outpatient diagnosis data set in long format, 
#'   comprised of outpatient diagnoses from medical claims.
#' 2. create an inpatient diagnosis data set in long format, that
#'   combines inpatient diagnoses from medical claims and 
#'   confinement claims.
#' 
#' Author: Mengbing Li
#' 
#' Created: 01/15/2019
#' 
#' Revisions:
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/data_processing")

library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(ggplot2)

### 1. extract inpatient claims data --------------------------------------------------
#' inpatient medical claims + confinement claims have two characteristics:
#' 1) if inpatient medical claims and confinement claims match on fst_dt and conf_id, 
#'   then keep the medical claims and remove the confinement claims because the
#'   latter contains a subset information of the former.
#' 2) if patid in confinement claims does not appear in inpatient medical claims,
#'   then keep the confinement claims.
medical <- fread("../../20181020/diagData_20181020_freeze.csv",
                 colClasses = list(character = 1:73), na.strings="")

# extract outpatient medical claims after index VTE
medicalOutpatient <- medical[fst_dt >= index_dt &
                              fst_dt < as.Date("2015-10-01") &
                              source %in% c("medical.outpatient")]
medicalOutpatient$source <- NULL

rm(medical)

diag_names <- grep("diag", colnames(medicalOutpatient), value = TRUE)

# change empty diagnoses to NA
for (j in diag_names) 
  set(medicalOutpatient, j = j, value = gsub("^$|-|^0{3,5}$", NA, medicalOutpatient[[j]]))

### transform outpatient diagnoses into long format -------------------------------
diagWideToLong <- function(data){
  melt(data, #[, (diag.vars), with = FALSE]
       id.vars = c("patid", "index_dt", "fst_dt", "charge", "copay",
                   "pos", "description", "category"),
       measure.vars = diag_names,
       value.name = "icd9_raw",
       variable.name = "diag",
       na.rm = TRUE)
}

medicalOutpatient_long <- diagWideToLong(medicalOutpatient)
rm(medicalOutpatient)

# check whether there are ICD-10 codes
# ICD-10 codes have length greater than 5
diagCodeLength <- nchar(trimws(medicalOutpatient_long$icd9_raw))
icd10Outpatient <- medicalOutpatient_long$icd9_raw[which(diagCodeLength > 5)]
icd10Outpatient <- sort(icd10Outpatient)
if(!is.null(icd10Outpatient)){
  print(unique(icd10Outpatient))
  print(length(icd10Outpatient))
  print(length(unique(icd10Outpatient)))
  
  # review these records
  medicalOutpatient_long_icd10 <- medicalOutpatient_long[diagCodeLength > 5,]
  write.table(medicalOutpatient_long_icd10, "ICD10_diagOutpatient_long.csv",
              quote = FALSE, sep = ",", row.names = FALSE)
  
  # remove ICD-10 records
  medicalOutpatient_long <- medicalOutpatient_long[diagCodeLength <= 5, ]
}



write.table(medicalOutpatient_long, "../diagOutpatient_long.csv",
            quote = FALSE, sep = ",", row.names = FALSE)
rm(medicalOutpatient_long)



### transform inpatient diagnoses into long format ---------------------------------

medical <- fread("../../20181020/diagData_20181020_freeze.csv",
                 colClasses = list(character = 1:73), na.strings="")

# extract inpatient medical claims after index VTE
medicalInpatient <- medical[fst_dt >= index_dt &
                               fst_dt < as.Date("2015-10-01") &
                               source %in% c("medical.inpatient")]
medicalInpatient$source <- NULL

# change empty diagnoses to NA
for (j in diag_names) 
  set(medicalInpatient, j = j, value = gsub("^$|-|^0{3,5}$", NA, medicalInpatient[[j]]))
# remove columns that are all NA's
# medicalInpatient <- medicalInpatient[, 
#   which(unlist(lapply(medicalInpatient, function(x) !all(is.na(x))))), with=F]

# extract inpatient confinement claims after index VTE
confinement <- medical[fst_dt >= index_dt &
                         fst_dt < as.Date("2015-10-01") &
                         source %in% c("confinement.inpatient")]
confinement$source <- NULL
rm(medical)

# change empty diagnoses to NA
for (j in diag_names) 
  set(confinement, j = j, value = gsub("^$|-|^0{3,5}$", NA, confinement[[j]]))


## remove duplicate inpatient claims information -------------------------------
#' if confinement claims match medical claims on patid, fst_dt, and conf_id,
#' then keep the correponding medical claims and remove confinement claims 
#' because the former contains finer information than the latter.

# whether confinement claims can be found in medical claims
confinement$inMedicalInpatient <- 
  do.call(paste0, confinement[, .(patid, fst_dt, conf_id)]) %in%
  do.call(paste0, medicalInpatient[, .(patid, fst_dt, conf_id)])

# whether medical claims can be found in confinement claims
medicalInpatient$inConfinement <- 
  do.call(paste0, medicalInpatient[, .(patid, fst_dt, conf_id)]) %in%
  do.call(paste0, confinement[, .(patid, fst_dt, conf_id)])


medicalInpatient$MedOrConf <- 
  ifelse(medicalInpatient$inConfinement == TRUE, "both", "med")
medicalInpatient$inConfinement <- NULL


# merge medicalInpatient and confinement claims not found in medical claims
# extract confinement claims not found in medical claims 
confinementOnly <- confinement[inMedicalInpatient == FALSE, ]
confinementOnly$inMedicalInpatient <- NULL
confinementOnly$MedOrConf <- "conf"
inpatient <- data.table(rbind(medicalInpatient, confinementOnly))
inpatient <- inpatient[order(patid, fst_dt, conf_id), ]

# remove columns that are all NA's
# inpatient <- inpatient[,
#   which(unlist(lapply(inpatient, function(x) !all(is.na(x))))), with=F]

# convert from wide to long
inpatient_long <- melt(inpatient, 
     id.vars = c("patid", "index_dt", "conf_id", "fst_dt", "charge", "copay",
                 "pos", "description", "category", "MedOrConf"),
     measure.vars = diag_names,
     value.name = "icd9_raw",
     variable.name = "diag",
     na.rm = TRUE)

inpatient_long <- unique(inpatient_long)
rm(confinementOnly)


# check whether there are ICD-10 codes
# ICD-10 codes have length greater than 5
diagCodeLength <- nchar(trimws(inpatient_long$icd9_raw))
icd10Inpatient <- inpatient_long$icd9_raw[which(diagCodeLength > 5)]
icd10Inpatient <- sort(icd10Inpatient)
if(!is.null(icd10Inpatient)){
  print(unique(icd10Inpatient))
  print(length(icd10Inpatient))
  print(length(unique(icd10Inpatient)))
  
  # review these records
  inpatient_long_icd10 <- inpatient_long[diagCodeLength > 5,]
  write.table(inpatient_long_icd10, "ICD10_diagInpatient_long.csv",
              quote = FALSE, sep = ",", row.names = FALSE)
  
  # remove ICD-10 records
  inpatient_long <- inpatient_long[diagCodeLength <= 5, ]
}

write.table(inpatient_long, "../diagInpatient_long.csv",
            quote = FALSE, sep = ",", row.names = FALSE)
rm(inpatient_long)


# check: 
diagInpatient <- fread("../diagInpatient_long.csv")
nrow(diagInpatient)
table(diagInpatient$MedOrConf)

diagOutpatient <- fread("../diagOutpatient_long.csv")
nrow(diagOutpatient)

# different ICD-10 from outpatient and inpatient
unique(setdiff(icd10Inpatient, icd10Outpatient))
length(unique(setdiff(icd10Inpatient, icd10Outpatient)))




