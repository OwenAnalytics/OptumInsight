##################################################
#' Program name: data_checkInpatientLabs.R
#' 
#' Description: This program checks whether inpatient
#' labs are available in facility data. A lab identified 
#' by proc_cd is considered an inpatient lab if:
#' 1) the proc_cd is a HCPCS/CPT code of lab and
#' 2) service date in facility data is within some 
#' hospitalization period in confinement data.
#' 
#' Author: Mengbing Li
#' 
#' Created: 10/02/2018
#' 
#' Revisions:
##################################################

library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(ggplot2)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20180830/data_checkInpatientLabs")

# read in data
mydata <- fread("../combineData.csv", 
                select = c("patid", "source", "fst_dt", "lst_dt", "diag", "icd9_raw", "proc_cd"),
                colClasses=list(character=1:5))

# keep hospitalization data and procedure data
mydata <- mydata[source %in% c("medical.inpatient", "confinement.inpati", "facility.inpatient"), ]

test <- mydata[patid %in% c("802666503180218", "802666507100198", "802666507111458"), ]
test$fst_dt <- as.Date(test$fst_dt)
test$lst_dt <- gsub("^ *$", NA, test$lst_dt)
test$lst_dt <- as.Date(test$lst_dt)

test_hospital <- test[source %in% c("medical.inpatient", "confinement.inpati"), ]
test_facility <- test[source == "facility.inpatient", ]


#' function check_range checks whether the input values in vec 
#' falls between any of hospital$fst_dt and hospital$lst_dt with
#' the same id. If yes then return TRUE, else return FALSE
check_range <- function(vec, id) {
  if(length(which(vec >= test_hospital$fst_dt &
                  vec <= test_hospital$lst_dt &
                  id == test_hospital$patid))) TRUE else FALSE
}
test_facility$inpatient.proc <- mapply(check_range,
                        test_facility$fst_dt, test_facility$patid)

# validate the function check_range
# subset_test_hospital <- unique(test_hospital[, 1:4])


#' function check_range checks whether the input values in vec 
#' falls between any of data1$day1 and data1$day2 when the id's
#' match. If yes then return TRUE, else return FALSE
check_range <- function(vec, id) {
  if(length(which(vec >= data1$day1 &
                  vec <= data1$day2 &
                  id == data1$id))) TRUE else FALSE
}
data2$day3_in_range <- mapply(check_range, data2$day3, data2$id)
