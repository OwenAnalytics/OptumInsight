##################################################
#' Program name: combineData.R
#' 
#' Description: The program merges medical, confinement, 
#' prescription, and member_detail data sets. The final
#' data set is ordered chronologically by each patient.
#' An example data structure is in combineData_structure.xlsx.
#' 
#' Author: Mengbing Li
#' 
#' Created: 09/16/2018
#' 
#' Revisions:
##################################################

library(data.table)
library(reshape2)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20180830/data_combineData")
# setwd("/home/mengbing/GSRA/")

## read in medical claims data --------------------------------------------------
medical <- fread("../separateData/medicalClaims.txt",
                 select = c("patid", "Conf_Id", "Copay", paste0("Diag", 1:25),
                            "Fst_Dt", "Lst_Dt", "Proc_Cd", "Prov"),
                 colClasses = list(character=c("patid", "Conf_Id", 
                            paste0("Diag", 1:25), "Fst_Dt", "Lst_Dt", "Proc_Cd"),
                            numeric = c("Copay"),
                            integer = c( "Prov")))
colnames(medical) <- tolower(colnames(medical))

medical <- medical[as.Date(medical$fst_dt) < as.Date("2015-10-01"),]

# change empty diagnoses to NA
for (j in paste0("diag", 1:25)) 
  set(medical, j = j, value = gsub("^$|-|^0{3,5}$", NA, medical[[j]]))

# medical[, (paste0("diag", 1:25)), with = FALSE] <- 
#   apply(medical[, (paste0("diag", 1:25)), with = FALSE], 2,
#         function(x) gsub("^$|-|0{4,5}|^0{3}$", NA, x))

# separate out inpatient claims and outpatient claims. 
# inpatient claims have non-empty confinement IDs
# test <- medical[1:50000,] #comment out
medical.inpatient <- unique(medical[conf_id != "",]) #change test to medical

# medical.inpatient <- medical.inpatient[1:1000,]

# transpose data into long format with one diagnosis per row
medical.inpatient_long <- melt(medical.inpatient,
      id.vars = c("patid", "conf_id", "copay", "fst_dt", "lst_dt",
                  "proc_cd", "prov"),
      measure.vars = paste0("diag", 1:25),
      value.name = "icd9_raw",
      variable.name = "diag",
      na.rm = TRUE)
# remove duplicate diagnoses and empty diagnoses
medical.inpatient_long <- unique(medical.inpatient_long)
medical.inpatient_long$source <- "medical.inpatient"


# outatient claims have empty confinement IDs
medical.outpatient <- unique(medical[conf_id == "",]) #change test to medical
# test_medical.outpatient <- medical.outpatient[1:1000,]

# transpose data into long format with one diagnosis per row
medical.outpatient_long <- melt(medical.outpatient,
      id.vars = c("patid", "conf_id", "copay", "fst_dt", "lst_dt", 
                  "proc_cd", "prov"),
      measure.vars = paste0("diag", 1:25),
      value.name = "icd9_raw",
      variable.name = "diag",
      na.rm = TRUE)
# remove duplicate diagnoses and empty diagnoses
medical.outpatient_long <- unique(medical.outpatient_long)
medical.outpatient_long$source <- "medical.outpatient"


# merge inpatient and outpatient claims from medical data
medical_long <- rbind(medical.inpatient_long, medical.outpatient_long)
medical_long <- medical_long[order(patid, fst_dt, lst_dt),]

rm(medical, medical.inpatient, medical.outpatient,
   medical.inpatient_long, medical.outpatient_long)

## read in confinement claims data ----------------------------------------------

confinement <- fread("confinementClaims.txt",
                     colClasses = list(character = c("patid")))
colnames(confinement) <- tolower(colnames(confinement))

# transpose data into long format with one diagnosis per row
confinement_long <- melt(confinement,
       id.vars = c("patid", "conf_id", "copay", "admit_dt", "disch_dt",
                   "prov"),
       measure.vars = paste0("diag", 1:5),
       value.name = "icd9_raw",
       variable.name = "diag",
       na.rm = TRUE)
# remove duplicate diagnoses and empty diagnoses
confinement_long <- unique(confinement_long[-grep("^$|-|^0$", icd9_raw),])
confinement_long$source <- "confinement.inpatient"

# format column names and order of confinement data to match those of medical data
colnames(confinement_long)[which(colnames(confinement_long) == "admit_dt")] <- "fst_dt"
colnames(confinement_long)[which(colnames(confinement_long) == "disch_dt")] <- "lst_dt"
confinement_long$proc_cd <- ""
setcolorder(confinement_long, colnames(medical_long))


# merge medical data and confinement data
med_conf <- rbind(medical_long, confinement_long)

rm(medical_long)

# med_conf <- med_conf[order(patid, fst_dt, lst_dt)]
med_conf <- unique(med_conf[order(patid, fst_dt, lst_dt)],
                   by = colnames(med_conf)[-which(colnames(med_conf)=="source")])
med_conf$prov <- as.character(med_conf$prov)



## read in facility claims data --------------------------------------------

facility <- fread("facilityClaims.txt",
                  select = c("patid", "fst_dt", "Proc_Cd"),
                  colClasses = list(character=c("patid", "fst_dt", "Proc_Cd")))
colnames(facility) <- tolower(colnames(facility))
facility <- facility[proc_cd != "", ]

facility <- facility[order(patid, fst_dt, proc_cd),]
facility <- unique(facility)
facility$fst_dt <- as.Date(facility$fst_dt)
facility$source <- "facility.inpatient"

# test_facility <- facility[1:10000,]


#' The reclass function unifies two data sets:
#' 1. Add columns that are found only found in data2 to data1, and
#' add columns that are only found in data 1 to data2;
#' 2. Change the added columns to the original column types
reclass <- function(data1, data2){
  # make shallow copies of data because shallow copy of data.table is passed in function
  data1.copy <- copy(data1)
  data2.copy <- copy(data2)
  # obtain columns in data1 but not in data2
  cols1 <- colnames(data1.copy)[which(!colnames(data1.copy) %in% colnames(data2.copy))]
  if(length(cols1) != 0) {
    data2.copy[, (cols1) := ""]
    
    # obtain the types of columns in data_from
    types1 <- unique(sapply(data1.copy, class))
    for(type in types1){
      # obtain the column names of type in data_from
      changeCols <- colnames(data1.copy)[which(as.vector(data1.copy[,
                        lapply(.SD, class)]) == type)]
      # assign type to corresponding columns in data_to
      to_type <- get(paste0("as.", type))
      data2.copy[, (changeCols):= lapply(.SD, to_type), .SDcols = changeCols]
    }
  }
  
  # obtain columns in data2.copy but not in data1
  cols2 <- colnames(data2.copy)[which(!colnames(data2.copy) %in% colnames(data1.copy))]
  if(length(cols2) != 0) {
    data1.copy[, (cols2) := ""]
    
    # obtain the types of columns in data_from
    types2 <- unique(sapply(data2.copy, class))
    for(type in types2){
      # obtain the column names of type in data_from
      changeCols <- colnames(data2.copy)[which(as.vector(data2.copy[,
                        lapply(.SD, class)]) == type)]
      # assign type to corresponding columns in data_to
      to_type <- get(paste0("as.", type))
      data1.copy[, (changeCols):= lapply(.SD, to_type), .SDcols = changeCols]
    }
  }
  
  # columns of data2.copy are in the same order as data1
  setcolorder(data2.copy, colnames(data1.copy))
  
  return(list(data1.copy, data2.copy))
}

reclass.out <- reclass(med_conf, facility)
med_conf2 <- reclass.out[[1]]
facility2 <- reclass.out[[2]]

rm(reclass.out)

med_conf_fac <- unique(rbind(med_conf2, facility2))
rm(facility, med_conf, med_conf2, facility2)

med_conf_fac <- med_conf_fac[order(patid, fst_dt, lst_dt, source), ]

# replace invalid proc_cd with empty string
med_conf_fac$proc_cd <- gsub("^0{5}$", "", med_conf_fac$proc_cd)


## read in anticoagulant claims data --------------------------------------------

patinfo <- fread("patient_data.txt")

anticoagulant <- patinfo[, .(patid, clmid, copay, fill_dt, gen_name, days_sup,
                             quantity, strength, npi)]
colnames(anticoagulant)[which(colnames(anticoagulant) == "fill_dt")] <- "fst_dt"
anticoagulant$source <- "anticoagulant"

reclass.out <- reclass(anticoagulant, med_conf_fac)
anticoagulant2 <- reclass.out[[1]]
med_conf_fac2 <- reclass.out[[2]]

rm(reclass.out)

med_conf_fac_ac <- rbind(med_conf_fac2, anticoagulant2)
med_conf_fac_ac <- med_conf_fac_ac[order(patid, fst_dt, lst_dt),]

rm(med_conf_fac, med_conf_fac2, anticoagulant2)


## add baseline covariates ----------------------------------------------------

variables.unwanted <- c(colnames(patinfo)[which(colnames(patinfo) %in% colnames(anticoagulant))],
                        "fill_dt", "category", "brand_name", "copay_sum")
add.variables <- colnames(patinfo)[which(!colnames(patinfo) %in% variables.unwanted)]
add.variables <- c("patid", add.variables)
baseline <- unique(patinfo[, (add.variables), with = FALSE])

# 
# write.table(med_conf_fac_ac, "data_combineData/med_conf_fac_ac.txt", row.names = FALSE,
#             sep = "\t", quote = FALSE, na = "NA")
write.csv(med_conf_fac_ac, "data_combineData/med_conf_fac_ac.csv", 
          quote = FALSE, row.names=FALSE, na="")

# write.table(baseline, "data_combineData/baseline.txt", row.names = FALSE,
#             sep = "\t", quote = FALSE, na = "NA")
write.table(baseline, "data_combineData/baseline.txt", sep = "\t",
            quote = FALSE, row.names=FALSE, na="")



#------------------------------ END HERE AND USE SAS TO MERGE -------------------------------------------






# merge temporal data and baseline covariates
#' mydata <- merge(x = med_conf_fac_ac, y = baseline, by = "patid", all.x = TRUE)
#' 
#' 
#' #' Build a tree structure of ICD-9 codes
#' #' The basic icd-9 code consists of 3 digits, while some are more specific
#' #' with 4 or five digits subdivision.
#' #' E codes and V codes represent external causes of injury and
#' #'  supplemental classification
#' #' V codes format: VXX(.XX)
#' #' E codes format: EXXX(.X)
#' mydata$icd9_3digits <- with(mydata,
#'                             ifelse(substr(icd9_raw, 1, 1)=="E",
#'                                    substr(icd9_raw, 1, 4),
#'                                    substr(icd9_raw, 1, 3)))
#' mydata$icd9_4digits <- with(mydata,
#'                             ifelse(substr(icd9_raw, 1, 1)=="E",
#'                                    ifelse(nchar(icd9_raw) < 5, "", substr(icd9_raw, 1, 5)),
#'                                    ifelse(nchar(icd9_raw) < 4, "", substr(icd9_raw, 1, 4))))
#' 
#' mydata$icd9_5digits <- with(mydata,
#'                             ifelse(substr(icd9_raw, 1, 1)=="E",
#'                                    "",
#'                                    ifelse(nchar(icd9_raw) < 5, "", substr(icd9_raw, 1, 5))))
#' 
#' 
#' 
#' # obtain length of stay for inpatient data
#' mydata$fst_dt <- as.Date(mydata$fst_dt)
#' mydata$lst_dt <- as.Date(mydata$lst_dt)
#' mydata$los <- with(mydata,
#'                    ifelse(source %in% c("medical.inpatient", "confinement.inpatient"),
#'                           lst_dt - fst_dt + 1,
#'                           -99))
#' 
#' # re-order columns
#' column.order <- c("patid", "source", "copay", "fst_dt", 
#'                   "conf_id", "lst_dt", "diag", "icd9_raw", "icd9_3digits", "icd9_4digits",
#'                   "icd9_5digits", "los", "proc_cd", "prov", # medical, confinement, and facility variables
#'                   "gen_name", "days_sup", "quantity", "strength", "npi") # anticoagulant variables
#' column.order <- c(column.order, colnames(mydata)[which(!colnames(mydata) %in% column.order)])
#' setcolorder(mydata, column.order)
#' 
#' print("The number of rows in the final data set is"); print(nrow(mydata))
#' 
#' write.table(mydata, "completeData_freeze.txt")
#' saveRDS(mydata, "completeData_freeze.rds")
#' 




