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

write.table(medicalOutpatient_long, "diagOutpatient_long.csv",
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
     id.vars = c("patid", "index_dt", "fst_dt", "charge", "copay",
                 "pos", "description", "category", "MedOrConf"),
     measure.vars = diag_names,
     value.name = "icd9_raw",
     variable.name = "diag",
     na.rm = TRUE)
rm(confinementOnly)


write.table(inpatient_long, "diagInpatient_long.csv",
            quote = FALSE, sep = ",", row.names = FALSE)
rm(inpatient_long)
