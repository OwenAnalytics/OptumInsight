##################################################
#' Program name: data_processing_proc.R
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

### 1. extract procedure data ------------------------------------------------
procedures <- fread("../../20181020/procData_20181020_freeze.csv",
                 colClasses = list(character = 1:112), na.strings="")

# add index_dt to procedure data
baseline <- fread("../../20181020/baseline_20181020_freeze.csv",
                  select = c("patid", "index_dt"),
                  colClasses = list(character = 1:2), na.strings="")
baseline <- unique(baseline)
procedures <- merge(x = procedures, y = baseline,
                    by = "patid", all.x = TRUE)

# since we only look at diagnoses before 2015-10-01, we set the same date range
# for procedures
procedures <- procedures[fst_dt >= index_dt &
                         fst_dt < as.Date("2015-10-01")]
# remove columns that are all NA's
procedures <- procedures[,
  which(unlist(lapply(procedures, function(x) !all(is.na(x))))), with=F]

proc_names <- grep("proc", colnames(procedures), value = TRUE)

# change empty diagnoses to NA
# for (j in proc_names) 
#   set(procedures, j = j, value = gsub("^$|-|^0{3,5}$", NA, procedures[[j]]))

### transform outpatient diagnoses into long format -------------------------------
diagWideToLong <- function(data){
  melt(data, #[, (diag.vars), with = FALSE]
       id.vars = c("patid", "index_dt", "fst_dt", "source", "conf_id", 
                   "pos", "description", "category"),
       measure.vars = proc_names,
       value.name = "proc_cd",
       variable.name = "proc",
       na.rm = TRUE)
}

procedures_long <- diagWideToLong(procedures)
procedures_long$proc <- as.numeric(procedures_long$proc)

rm(procedures)

write.table(procedures_long, "../procedures_long.csv",
            quote = FALSE, sep = ",", row.names = FALSE)

# check: 
# procedures <- fread("../diagInpatient_long.csv")
nrow(procedures_long)

rm(procedures_long)




