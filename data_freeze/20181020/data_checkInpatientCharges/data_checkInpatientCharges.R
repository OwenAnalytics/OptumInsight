##################################################
#' Program name: data_checkInpatientCharges.R
#' 
#' Description: This program checks whether inpatient medical
#' charges add up to confinement charges.
#' 
#' Author: Mengbing Li
#' 
#' Created: 11/30/2018
#' 
#' Revisions: 
##################################################

library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(ggplot2)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_checkInpatientCharges")

### Obtain inpatient medical data
diagData <- fread("../diagData_20181020_freeze.csv", colClasses = list(character = 1:73))

# obtain inpatient records from medical data
medicalInpatient <- diagData[source == "medical.inpatient",]

medical_test <- medicalInpatient[1:10000,]
medical_test <- medical_test[order(patid, fst_dt, lst_dt)]

# obtain admission dates and lengths of stay
confinement <- diagData[source == "confinement.inpatient",
                        .(patid, index_dt, conf_id, fst_dt, lst_dt, 
                          charge, copay, pos, description, category)]

rm(diagData)

# By observation, medical data does not provide detailed enough breakdown of confinement charges.


