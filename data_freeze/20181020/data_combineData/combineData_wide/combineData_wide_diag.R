##################################################
#' Program name: combineData_wide.R
#' 
#' Description: The program merges medical, confinement, 
#' prescription, and member_detail data sets. The final
#' data set is in wide format, ordered chronologically by each patient.
#' An example data structure is in combineData_structure.xlsx.
#' 
#' Author: Mengbing Li
#' 
#' Created: 10/15/2018 - Convert long format to wide format,
#'     one day per row, including diagnoses, procedures, labs etc
#'     performed on that day.
#'  10/21/2018 - Separate diagnoses and proc_cd into two 
#'     different data sets. The duplicacy and incompatibility of proc_cd
#'     from facility claims and proc_cd from medical or confinement 
#'     claims make the two information hard to combine.
#'  10/23/2018 - Add charge variable from medical and confinement data
##################################################

library(data.table)
library(reshape2)
library(xlsx)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_combineData/combineData_wide")

## read in medical claims data --------------------------------------------------
medical <- fread("../../separateData/medicalClaims.txt",
                 select = c("patid", "Conf_Id", "Charge", "Copay", paste0("Diag", 1:25),
                            "Fst_Dt", "Lst_Dt", "Proc_Cd", "Pos"),
                 colClasses = list(character=c("patid", "Conf_Id", 
                            paste0("Diag", 1:25), "Fst_Dt", "Lst_Dt", "Proc_Cd", "Pos"),
                            numeric = c("Charge", "Copay")))
colnames(medical) <- tolower(colnames(medical))

# remove records after ICD-10 became effective
# medical$fst_dt <- as.Date(medical$fst_dt)
#medical <- medical[medical$fst_dt < "2015-10-01",]

# test <- medical[1:10000]

# example of multiplicity in diagnosis codes at deeper tree levels
# example <- test[patid == "802666500107193" & fst_dt == "2015-07-18", ]

# change empty diagnoses to NA
#for (j in paste0("diag", 1:25)) 
#  set(medical, j = j, value = gsub("^$|-|^0{3,5}$", NA, medical[[j]]))

# change empty or invalid proc_cd to NA
# range of invalid CPT codes: < 00100
#medical$proc_cd <- with(medical, ifelse(proc_cd < "00100", NA, proc_cd))



### read in confinement data -------------------------------------------
confinement <- fread("../../separateData/confinementClaims.txt",
                     colClasses = list(character = c("patid")))
colnames(confinement) <- tolower(colnames(confinement))

# remove records after ICD-10 became effective
confinement <- confinement[confinement$admit_dt < "2015-10-01",]

# keep wanted variables
confinement <- confinement[, .(patid, conf_id, admit_dt, disch_dt,
                               diag1, diag2, diag3, diag4, diag5,
                               charge, copay, los, pos)]

# transpose data into long format with one diagnosis per row
# change empty diagnoses to NA
for (j in paste0("diag", 1:5)) 
  set(confinement, j = j, value = gsub("^$|-|^0{3,5}$", NA, confinement[[j]]))

# confinement_long <- melt(confinement,
#        id.vars = c("patid", "conf_id", "copay", "admit_dt", "disch_dt", "pos"),
#        measure.vars = paste0("diag", 1:5),
#        value.name = "icd9_raw",
#        variable.name = "diag",
#        na.rm = TRUE)
confinement$source <- "confinement.inpatient"

# modify columns to match medical data set
colnames(confinement)[which(colnames(confinement) == "admit_dt")] <- "fst_dt"
colnames(confinement)[which(colnames(confinement) == "disch_dt")] <- "lst_dt"
colnames(confinement)[which(colnames(confinement) == "charge")] <- "charge_sum"
colnames(confinement)[which(colnames(confinement) == "copay")] <- "copay_sum"
colnames(confinement)[which(colnames(confinement) == "diag1")] <- "diag1_1"


## read in pos details -------------------------------------------------
pos_detail <- read.xlsx("../../separateData/All_codes_ICD9_NDC_HCPCS.xlsx",
                        sheetName = "POS", header = TRUE)
colnames(pos_detail) <- tolower(colnames(pos_detail))
pos_detail$pos <- as.character(pos_detail$pos)


patinfo <- fread("../../separateData/patient_data.txt")
patinfo <- unique(patinfo[, .(patid, index_dt)])


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
      changeCols <- colnames(data1.copy)[which(as.vector(data1.copy[, lapply(.SD, class)]) == type)]
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




### combine the data sets in split data because data is too large for RAM
patids <- unique(medical$patid)
group.size <- 500
x <- seq_along(patids)
patids.split <- split(patids, ceiling(x/group.size))


for(i in 1:length(patids.split)){
  med <- subset(medical, patid %in% patids.split[[i]])
  
  # reduce the size of medical data to save memory
  medical <- subset(medical, ! (patid %in% patids.split[[i]]))
  
  med <- med[med$fst_dt < "2015-10-01",]
  
  # change empty diagnoses to NA
  for (j in paste0("diag", 1:25)) 
    set(med, j = j, value = gsub("^$|-|^0{3,5}$", NA, med[[j]]))

  # change empty or invalid proc_cd to NA
  # range of invalid CPT codes: < 00100
  med$proc_cd <- with(med, ifelse(proc_cd < "00100", NA, proc_cd))
  
  # sum up copays across rows with the same date and diagnoses
  med[, `:=`(charge_sum = sum(charge),
             copay_sum = sum(copay)), by = c("patid", "fst_dt", "pos")]
  
  # # get proc_cd from medical data
  # # this step is moved here so that medical data can be deleted from memory
  # proc_med <- unique(med[, .(patid, conf_id, fst_dt, lst_dt, proc_cd)])
  # proc_med <- proc_med[is.na(proc_med$proc_cd) == FALSE, ]
  
  # rm(medical, data1)
  
  
  ### Dealing with diagnoses from medical data -------------------------------------
  
  # transform diagnoses
  # remove duplicate diagnoses in one day, if all are inpatient or all outpatient
  diag.vars <- c("patid", "conf_id", paste0("diag", 1:25), 
                 "fst_dt", "lst_dt", "charge_sum", "copay_sum", "pos")
  medical_diag_long <- melt(med[, (diag.vars), with = FALSE],
                            id.vars = c("patid", "conf_id", "charge_sum", "copay_sum", 
                                        "fst_dt", "lst_dt", "pos"),
                            measure.vars = paste0("diag", 1:25),
                            value.name = "icd9_raw",
                            variable.name = "diag",
                            na.rm = TRUE)
  # remove duplicate diagnoses and empty diagnoses
  medical_diag_long <- unique(medical_diag_long)
  
  rm(med)
  
  
  ## Deal with outpatient diagnoses from medical data ---------------
  # inpatient diagnoses will be operated together with confinement data
  
  #' Note that there may be repeated diagX (repeated in X)
  #' If multiple diag1's exist, then separate each into different
  #' columns. If multiple diagX (X >= 2) exist, then renumber
  #' the diagnoses so that one X corresponds to one diagnosis
  
  # deal with diag1
  medical_diag1_long <- medical_diag_long[diag == "diag1", ]
  
  # set the key to all columns
  # setkey(medical_diag1_long)
  # Get Unique lines in the data table
  medical_diag1_long <- medical_diag1_long[!duplicated(medical_diag1_long[,
    c("patid", "conf_id", "fst_dt", "lst_dt", "pos", "icd9_raw")]),]
  # medical_diag1_long <- unique(medical_diag1_long[
  #   list(patid, conf_id, fst_dt, lst_dt, pos, icd9_raw), nomatch = 0] ) 
  
  # transform from long to wide
  medical_diag1 <- dcast(setDT(medical_diag1_long),
     patid + conf_id + charge_sum + copay_sum + fst_dt + lst_dt + pos ~
       rowid(patid, fst_dt, lst_dt, conf_id, pos, charge_sum, copay_sum,
             prefix = "diag1_"), 
     # specify the names of the new variables generated by dcast
     value.var = "icd9_raw")
  
  rm(medical_diag1_long)
  
  # deal with remaining diagnoses
  medical_diagX_long <- medical_diag_long[diag != "diag1", ]
  rm(medical_diag_long)
  
  # set the key to all columns
  # setkey(medical_diagX_long)
  # Get Unique lines in the data table
  medical_diagX_long <- medical_diagX_long[!duplicated(medical_diagX_long[,
          c("patid", "conf_id", "fst_dt", "lst_dt", "pos", "icd9_raw")]),]
  
  medical_diagX_long$diag <- paste0("diag", rowidv(medical_diagX_long,
    cols = c("patid", "fst_dt", "lst_dt", "conf_id", "pos", "charge_sum", "copay_sum"))+1)
  
  # transform from long to wide
  medical_diagX <- dcast(setDT(medical_diagX_long),
     patid + conf_id + charge_sum + copay_sum + fst_dt + lst_dt + pos ~ diag, 
     value.var = "icd9_raw")

  rm(medical_diagX_long)
  
  
  # merge all diagnoses
  medical_all <- merge(x = medical_diag1, 
                       y = medical_diagX,
                       by = c("patid", "fst_dt", "lst_dt",
                              "conf_id", "charge_sum", "copay_sum", "pos"),
                       all.x = TRUE)
  
  rm(medical_diag1, medical_diagX)
  
  # re-order columns
  diag_names <- grep("diag", colnames(medical_all), value = TRUE)
  
  diag1_names <- paste0("diag1_", 1:length(grep("diag1_", diag_names)))
  diagX_names <- paste0("diag", 2:(length(diag_names[! (diag_names %in% diag1_names)])+1))
  colum_order <- c("patid", "fst_dt", "lst_dt", "conf_id", "charge_sum", "copay_sum", "pos",
                   diag1_names, diagX_names)
  medical_all <- medical_all[, colum_order]
  
  # add source variable
  medical_all$source <- ifelse(medical_all$conf_id == "", 
                               "medical.outpatient", "medical.inpatient")
  
  
  ## deal with diagnoses from confinement --------------------------
  confinement.split <- confinement[patid %in% patids.split[[i]], ]
  
  confinement <- subset(confinement, ! (patid %in% patids.split[[i]]))
  
  names_in_med_not_conf <- colnames(medical_all)[
    !colnames(medical_all) %in% colnames(confinement.split)]
  
  confinement.split <- confinement.split[, (names_in_med_not_conf) := NA]
  
  # re-order columns
  confinement.split <- confinement.split[, colnames(medical_all), with = FALSE]
  
  med_conf <- rbind(medical_all, confinement.split)
  
  rm(medical_all)
  
  med_conf <- med_conf[order(patid, fst_dt, lst_dt, source), ]
  
  
  ## add pos details -------------------------------------------------
  med_conf2 <- merge(x = med_conf,
                     y = pos_detail,
                     by = "pos", all.x = TRUE)
  
  rm(med_conf, confinement.split)
  
  colnames(med_conf2)[which(colnames(med_conf2) == "charge_sum")] <- "charge"
  colnames(med_conf2)[which(colnames(med_conf2) == "copay_sum")] <- "copay"
  
  
  ### Add index VTE dates ------------------------------------------------------
  pat_indexDt <- unique(patinfo[patid %in% patids.split[[i]], ])
  
  patinfo <- subset(patinfo, ! (patid %in% patids.split[[i]]))
  
  med_conf2 <- merge(x = med_conf2,
                       y = pat_indexDt,
                       by = c("patid"), all.x = TRUE)
  
  
  if(i == 1){
    final <- data.table(matrix(NA, nrow = 0, ncol = ncol(med_conf2)))
    colnames(final) <- c(colnames(med_conf2))
  }

  reclass.out <- reclass(final, med_conf2)
  final <- reclass.out[[1]]
  med_conf2 <- reclass.out[[2]]
  rm(reclass.out)

  final <- rbind(final, med_conf2)
  
  rm(med_conf2)
}


# change the order of columns
diag_names <- grep("diag", colnames(final), value = TRUE)
diag1_names <- paste0("diag1_", 1:length(grep("diag1_", diag_names)))
diagX_names <- paste0("diag", 2:(length(diag_names[! (diag_names %in% diag1_names)])+1))
column_order <- c("patid", "index_dt", "fst_dt", "lst_dt", "charge", "copay", "source", 
                  "conf_id", "pos", "description", "category",
                  diag1_names, diagX_names)
final <- final[, (column_order), with = FALSE]


write.csv(final, "diagData_20181020_freeze.csv",
          quote = FALSE, row.names=FALSE, na="")





### !!! Current problem : --------------------------------------------------
# the number of patients in the data is less than 14932
# checking whether some patients were left out becuase they only appeared
#   in confinement claims

### read in confinement data 
confinement <- fread("../separateData/confinementClaims.txt",
                     colClasses = list(character = c("patid")))
colnames(confinement) <- tolower(colnames(confinement))
length(setdiff(confinement$patid, patients))


testIndex <- which(diagnosisInformation$patients=="802666500100523")
diagnosisInformation$roots[testIndex]






# ### Check whether different levels of the same ICD9 code may appear  --------
# medical_diagX_long$icd9_3digits <- with(medical_diagX_long,
#                             ifelse(substr(icd9_raw, 1, 1)=="E",
#                                    substr(icd9_raw, 1, 4),
#                                    substr(icd9_raw, 1, 3)))
# medical_diagX_long$icd9_4digits <- with(medical_diagX_long,
#                             ifelse(substr(icd9_raw, 1, 1)=="E",
#                                    ifelse(nchar(icd9_raw) < 5, "", substr(icd9_raw, 1, 5)),
#                                    ifelse(nchar(icd9_raw) < 4, "", substr(icd9_raw, 1, 4))))
# 
# medical_diagX_long$icd9_5digits <- with(medical_diagX_long,
#                             ifelse(substr(icd9_raw, 1, 1)=="E",
#                                    "",
#                                    ifelse(nchar(icd9_raw) < 5, "", substr(icd9_raw, 1, 5))))
# 
# library(dplyr)
# icd9_only3 <- medical_diagX_long %>%
#   group_by(patid, conf_id, fst_dt, lst_dt, pos, icd9_3digits, icd9_4digits) %>%
#   mutate(n = n()) %>%
#   # count(icd9_3digits) %>%
#   arrange(patid, fst_dt, lst_dt, icd9_3digits, icd9_4digits) %>%
#   filter(icd9_4digits == "") %>%
#   select(patid, conf_id, fst_dt, lst_dt, pos, n)
# 
# icd9_only4 <- medical_diagX_long %>%
#   group_by(patid, conf_id, fst_dt, lst_dt, pos, icd9_3digits, icd9_4digits) %>%
#   mutate(n = n()) %>%
#   # count(icd9_3digits) %>%
#   arrange(patid, fst_dt, lst_dt, icd9_3digits, icd9_4digits) %>%
#   filter(icd9_4digits != "") %>%
#   select(patid, conf_id, fst_dt, lst_dt, pos, n)
# 
# 
# check_dup <- merge(x = icd9_only3, y = icd9_only4,
#                    by = c("patid", "conf_id", "fst_dt", "lst_dt", "pos"),
#                    all = FALSE)
# check_dup$three_not_four <- (check_dup$icd9_3digits.x == check_dup$icd9_3digits.y)


#' for(i in 1:length(patids.split)){
#'   med <- subset(medical, patid %in% patids.split[[i]])
#'   
#'   # sum up copays across rows with the same date and diagnoses
#'   med[, copay_sum := sum(copay), by = c("patid", "fst_dt", "pos")]
#'   
#'   # # get proc_cd from medical data
#'   # # this step is moved here so that medical data can be deleted from memory
#'   # proc_med <- unique(med[, .(patid, conf_id, fst_dt, lst_dt, proc_cd)])
#'   # proc_med <- proc_med[is.na(proc_med$proc_cd) == FALSE, ]
#'   
#'   # rm(medical, data1)
#'   
#'   
#'   ### Dealing with diagnoses from medical data -------------------------------------
#'   
#'   # transform diagnoses
#'   # remove duplicate diagnoses in one day, if all are inpatient or all outpatient
#'   diag.vars <- c("patid", "conf_id", paste0("diag", 1:25), 
#'                  "fst_dt", "lst_dt", "copay_sum", "pos")
#'   medical_diag_long <- melt(med[, (diag.vars), with = FALSE],
#'                             id.vars = c("patid", "conf_id", "copay_sum", 
#'                                         "fst_dt", "lst_dt", "pos"),
#'                             measure.vars = paste0("diag", 1:25),
#'                             value.name = "icd9_raw",
#'                             variable.name = "diag",
#'                             na.rm = TRUE)
#'   # remove duplicate diagnoses and empty diagnoses
#'   medical_diag_long <- unique(medical_diag_long)
#'   
#'   rm(med)
#'   
#'   
#'   ## Deal with outpatient diagnoses from medical data ---------------
#'   # inpatient diagnoses will be operated together with confinement data
#'   
#'   #' Note that there may be repeated diagX (repeated in X)
#'   #' If multiple diag1's exist, then separate each into different
#'   #' columns. If multiple diagX (X >= 2) exist, then renumber
#'   #' the diagnoses so that one X corresponds to one diagnosis
#'   
#'   # deal with diag1
#'   medical_diag1_long <- medical_diag_long[diag == "diag1", ]
#'   # medical_diag1_long <- medical_diag1_long[order(
#'   #   patid, fst_dt, lst_dt, conf_id, pos), ]
#'   
#'   # transform from long to wide
#'   medical_diag1 <- dcast(setDT(medical_diag1_long),
#'                          patid + conf_id + copay_sum + fst_dt + lst_dt + pos ~
#'                            rowid(patid, fst_dt, lst_dt, conf_id, pos, copay_sum,
#'                                  prefix = "diag1_"), 
#'                          # specify the names of the new variables generated by dcast
#'                          value.var = "icd9_raw")
#'   
#'   rm(medical_diag1_long)
#'   
#'   # deal with remaining diagnoses
#'   medical_diagX_long <- medical_diag_long[diag != "diag1", ]
#'   
#'   rm(medical_diag_long)
#'   
#'   medical_diagX_long$diag <- paste0("diag", rowidv(medical_diagX_long,
#'                cols = c("patid", "fst_dt", "lst_dt", "conf_id", "pos", "copay_sum"))+1)
#'   
#'   # transform from long to wide
#'   medical_diagX <- dcast(setDT(medical_diagX_long),
#'                          patid + conf_id + copay_sum + fst_dt + lst_dt + pos ~ diag, 
#'                          value.var = "icd9_raw")
#'   
#'   rm(medical_diagX_long)
#'   
#'   
#'   # merge all diagnoses
#'   medical_all <- merge(x = medical_diag1, 
#'                        y = medical_diagX,
#'                        by = c("patid", "fst_dt", "lst_dt",
#'                               "conf_id", "copay_sum", "pos"),
#'                        all.x = TRUE)
#'   
#'   rm(medical_diag1, medical_diagX)
#'   
#'   # re-order columns
#'   diag_names <- grep("diag", colnames(medical_all), value = TRUE)
#'   
#'   diag1_names <- paste0("diag1_", 1:length(grep("diag1_", diag_names)))
#'   diagX_names <- paste0("diag", 2:(length(diag_names[! (diag_names %in% diag1_names)])+1))
#'   colum_order <- c("patid", "fst_dt", "lst_dt", "conf_id", "copay_sum", "pos",
#'                    diag1_names, diagX_names)
#'   medical_all <- medical_all[, colum_order]
#'   
#'   # add source variable
#'   medical_all$source <- ifelse(medical_all$conf_id == "", 
#'                                "medical.outpatient", "medical.inpatient")
#'   
#'   
#'   ## deal with diagnoses from confinement --------------------------
#'   confinement.split <- confinement[patid %in% patids.split[[i]], ]
#'   
#'   names_in_med_not_conf <- colnames(medical_all)[
#'     !colnames(medical_all) %in% colnames(confinement.split)]
#'   
#'   confinement.split <- confinement.split[, (names_in_med_not_conf) := NA]
#'   
#'   # re-order columns
#'   confinement.split <- confinement.split[, colnames(medical_all), with = FALSE]
#'   
#'   med_conf <- rbind(medical_all, confinement.split)
#'   
#'   rm(medical_all)
#'   
#'   med_conf <- med_conf[order(patid, fst_dt, lst_dt, source), ]
#'   
#'   
#'   ## add pos details -------------------------------------------------
#'   med_conf2 <- merge(x = med_conf,
#'                      y = pos_detail,
#'                      by = "pos", all.x = TRUE)
#'   
#'   rm(med_conf, confinement.split)
#'   colnames(med_conf2)[which(colnames(med_conf2) == "copay_sum")] <- "copay"
#'   
#'   # ## deal with proc_cd -----------------------------------------------
#'   # 
#'   # # transform from long to wide
#'   # proc_med_wide <- dcast(setDT(proc_med),
#'   #                        patid + conf_id + fst_dt + lst_dt~
#'   #                          rowid(patid, conf_id, fst_dt, lst_dt, prefix = "proc_cd"),
#'   #                        value.var = "proc_cd")
#'   # 
#'   # rm(proc_med)
#'   # 
#'   # # re-order columns
#'   # proc_names <- grep("proc_cd", colnames(proc_med_wide), value = TRUE)
#'   # proc_names_order <- paste0("proc_cd", 1:length(proc_names))
#'   # colum_order <- c("patid", "conf_id", "fst_dt", "lst_dt", proc_names_order)
#'   # proc_med_wide <- proc_med_wide[, colum_order]
#'   # 
#'   # 
#'   # # add proc_cd to diagnosis data
#'   # med_conf3 <- merge(x = med_conf2,
#'   #                    y = proc_med_wide,
#'   #                    by = c("patid", "conf_id", "fst_dt", "lst_dt"),
#'   #                    all.x = TRUE)
#'   # 
#'   # colnames(med_conf3)[which(colnames(med_conf3) == "copay_sum")] <- "copay"
#'   
#'   # rm(med_conf2)
#'   
#'   
#'   
#'   # change columns to match those of med_conf
#'   # anticoagulant.split <- anticoagulant[patid %in% patids.split[[i]], ]
#'   # colnames(anticoagulant.split)[which(colnames(anticoagulant.split) == "fill_dt")] <- "fst_dt"
#'   # 
#'   # anticoagulant.split$source <- "anticoagulant.outpatient"
#'   # 
#'   # med_conf_ac <- merge(x = med_conf3,
#'   #                      y = anticoagulant.split,
#'   #                      by = c("patid", "fst_dt", "copay", "source"),
#'   #                      all = TRUE)
#'   # 
#'   # rm(med_conf3)
#'   
#'   
#'   ### Add index VTE dates ------------------------------------------------------
#'   pat_indexDt <- unique(patinfo[patid %in% patids.split[[i]], .(patid, index_dt)])
#'   med_conf_ac <- merge(x = med_conf_ac,
#'                        y = pat_indexDt,
#'                        by = c("patid"), all.x = TRUE)
#'   
#'   
#'   # change the order of columns
#'   column_order <- c("patid", "index_dt", "fst_dt", "lst_dt", "copay", "source", 
#'                     "conf_id", "pos", "description", "category",
#'                     diag1_names, diagX_names, proc_names_order, 
#'                     "gen_name", "days_sup", "quantity", "strength")
#'   med_conf_ac <- med_conf_ac[, (column_order), with = FALSE]
#'   
#'   
#'   # output each batch of data
#'   # out_name <- paste0("combinedData_med_conf_ac", i, ".csv")
#'   # write.csv(med_conf_ac, out_name, 
#'   #           quote = FALSE, row.names=FALSE, na="")
#'   
#'   
#'   # if(i == 1){
#'   #   med_conf_ac2 <- data.table(matrix(NA, nrow = 0,
#'   #                                     ncol = ncol(med_conf_ac)))
#'   #   colnames(med_conf_ac2) <- c(colnames(med_conf_ac))
#'   #   
#'   # }
#'   # 
#'   # 
#'   # reclass.out <- reclass(med_conf_ac, med_conf_ac2)
#'   # med_conf_ac <- reclass.out[[1]]
#'   # med_conf_ac2 <- reclass.out[[2]]
#'   # 
#'   # med_conf_ac2 <- rbind(med_conf_ac2, med_conf_ac)
#'   # 
#'   rm(med_conf_ac)
#' }
#' 
#' # medical[, copay_sum := sum(copay), by = c("patid", "fst_dt", "pos")]












#' 
#' 
#' 
#' 
#' 
#' 
#' # get proc_cd from medical data
#' # this step is moved here so that medical data can be deleted from memory
#' proc_med <- unique(medical2[, .(patid, conf_id, fst_dt, lst_dt, proc_cd)])
#' proc_med <- proc_med[is.na(proc_med$proc_cd) == FALSE, ]
#' 
#' rm(medical, data1)
#' 
#' 
#' 
#' ### Dealing with diagnoses from medical data -------------------------------------
#' 
#' # transform diagnoses
#' # remove duplicate diagnoses in one day, if all are inpatient or all outpatient
#' diag.vars <- c("patid", "conf_id", paste0("diag", 1:25), 
#'                "fst_dt", "lst_dt", "copay_sum", "pos")
#' medical_diag_long <- melt(medical2[, (diag.vars), with = FALSE],
#'                  id.vars = c("patid", "conf_id", "copay_sum", 
#'                              "fst_dt", "lst_dt", "pos"),
#'                  measure.vars = paste0("diag", 1:25),
#'                  value.name = "icd9_raw",
#'                  variable.name = "diag",
#'                  na.rm = TRUE)
#' # remove duplicate diagnoses and empty diagnoses
#' medical_diag_long <- unique(medical_diag_long)
#' 
#' rm(medical2)
#' 
#' 
#' # identify inpatient and outpatient records
#' # medical_diag_long$source <- ifelse(medical_diag_long$conf_id == "",
#' #                                         "medical.outpatient",
#' #                                         "medical.inpatient")
#' # medical_inpatient_diag_long <- medical_diag_long[
#' #   medical_diag_long$conf_id != "",]
#' # medical_outpatient_diag_long <- medical_diag_long[
#' #   medical_diag_long$conf_id == "",]
#' 
#' 
#' ## Deal with outpatient diagnoses from medical data ---------------
#' # inpatient diagnoses will be operated together with confinement data
#' 
#' #' Note that there may be repeated diagX (repeated in X)
#' #' If multiple diag1's exist, then separate each into different
#' #' columns. If multiple diagX (X >= 2) exist, then renumber
#' #' the diagnoses so that one X corresponds to one diagnosis
#' 
#' # deal with diag1
#' medical_diag1_long <- medical_diag_long[diag == "diag1", ]
#' # medical_diag1_long <- medical_diag1_long[order(
#' #   patid, fst_dt, lst_dt, conf_id, pos), ]
#' 
#' # transform from long to wide
#' medical_diag1 <- dcast(setDT(medical_diag1_long),
#'     patid + conf_id + copay_sum + fst_dt + lst_dt + pos ~
#'       rowid(patid, fst_dt, lst_dt, conf_id, pos, copay_sum, prefix = "diag1_"), 
#'           # specify the names of the new variables generated by dcast
#'     value.var = "icd9_raw")
#' 
#' rm(medical_diag1_long)
#' 
#' # deal with remaining diagnoses
#' medical_diagX_long <- medical_diag_long[diag != "diag1", ]
#' 
#' rm(medical_diag_long)
#' 
#' medical_diagX_long$diag <- paste0("diag", 
#'        rowidv(medical_diagX_long, 
#'               cols = c("patid", "fst_dt", "lst_dt", 
#'                        "conf_id", "pos", "copay_sum"))+1)
#' 
#' # transform from long to wide
#' medical_diagX <- dcast(setDT(medical_diagX_long),
#'       patid + conf_id + copay_sum + fst_dt + lst_dt + pos ~ diag, 
#'       value.var = "icd9_raw")
#' 
#' rm(medical_diagX_long)
#' 
#' 
#' # merge all diagnoses
#' medical_all <- merge(x = medical_diag1, 
#'                     y = medical_diagX,
#'                     by = c("patid", "fst_dt", "lst_dt",
#'                            "conf_id", "copay_sum", "pos"),
#'                     all.x = TRUE)
#' 
#' rm(medical_diag1, medical_diagX)
#' 
#' # re-order columns
#' diag_names <- grep("diag", colnames(medical_all), value = TRUE)
#' 
#' diag1_names <- paste0("diag1_", 1:length(grep("diag1_", diag_names)))
#' diagX_names <- paste0("diag", 2:(length(diag_names[! (diag_names %in% diag1_names)])+1))
#' colum_order <- c("patid", "fst_dt", "lst_dt", "conf_id", "copay_sum", "pos",
#'                  diag1_names, diagX_names)
#' medical_all <- medical_all[, colum_order]
#' 
#' # add source variable
#' medical_all$source <- ifelse(medical_all$conf_id == "", 
#'                              "medical.outpatient", "medical.inpatient")
#' 
#' 
#' ## deal with inpatient diagnoses from medical and confinement data  ------------
#' 
#' # read in confinement claims data 
#' 
#' confinement <- fread("../separateData/confinementClaims.txt",
#'                      colClasses = list(character = c("patid")))
#' colnames(confinement) <- tolower(colnames(confinement))
#' 
#' # remove records after ICD-10 became effective
#' confinement <- confinement[confinement$admit_dt < "2015-10-01",]
#' 
#' # keep wanted variables
#' confinement <- confinement[, .(patid, conf_id, admit_dt, disch_dt,
#'                                diag1, diag2, diag3, diag4, diag5,
#'                                copay, los, pos)]
#' 
#' # transpose data into long format with one diagnosis per row
#' # change empty diagnoses to NA
#' for (j in paste0("diag", 1:5)) 
#'   set(confinement, j = j, value = gsub("^$|-|^0{3,5}$", NA, confinement[[j]]))
#' 
#' # confinement_long <- melt(confinement,
#' #        id.vars = c("patid", "conf_id", "copay", "admit_dt", "disch_dt", "pos"),
#' #        measure.vars = paste0("diag", 1:5),
#' #        value.name = "icd9_raw",
#' #        variable.name = "diag",
#' #        na.rm = TRUE)
#' confinement$source <- "confinement.inpatient"
#' 
#' 
#' 
#' # modify columns to match medical data set
#' colnames(confinement)[which(colnames(confinement) == "admit_dt")] <- "fst_dt"
#' colnames(confinement)[which(colnames(confinement) == "disch_dt")] <- "lst_dt"
#' colnames(confinement)[which(colnames(confinement) == "copay")] <- "copay_sum"
#' colnames(confinement)[which(colnames(confinement) == "diag1")] <- "diag1_1"
#' 
#' names_in_med_not_conf <- colnames(medical_all)[
#'   !colnames(medical_all) %in% colnames(confinement)]
#' 
#' confinement <- confinement[, (names_in_med_not_conf) := NA]
#' 
#' # re-order columns
#' confinement <- confinement[, colnames(medical_all), with = FALSE]
#' 
#' med_conf <- rbind(medical_all, confinement)
#' 
#' rm(medical_all)
#' 
#' med_conf <- med_conf[order(patid, fst_dt, lst_dt, source), ]
#' 
#' 
#' # med_conf$pos <- as.character(med_conf$pos)
#' # add pos details
#' pos_detail <- read.xlsx("../separateData/All_codes_ICD9_NDC_HCPCS.xlsx",
#'                         sheetName = "POS", header = TRUE)
#' colnames(pos_detail) <- tolower(colnames(pos_detail))
#' pos_detail$pos <- as.character(pos_detail$pos)
#' 
#' med_conf2 <- merge(x = med_conf,
#'                   y = pos_detail,
#'                   by = "pos", all.x = TRUE)
#' 
#' 
#' rm(med_conf, confinement)
#' 
#' 
#' 
#' ### Deal with proc_cd -------------------------------------------------------
#' 
#' 
#' 
#' # !!! DO NOT KNOW HOW TO ADD PROC_CD FROM FACILITY BECAUSE THEY DO NOT HAVE LST_DT OR POS !!!
#' 
#' # get proc_cd from facility claims data 
#' # facility <- fread("../separateData/facilityClaims.txt",
#' #                   select = c("patid", "fst_dt", "Proc_Cd"),
#' #                   colClasses = list(character=c("patid", "fst_dt", "Proc_Cd")))
#' # colnames(facility) <- tolower(colnames(facility))
#' # facility <- facility[proc_cd != "", ]
#' # 
#' # facility <- unique(facility[order(patid, fst_dt, proc_cd),])
#' # # facility$fst_dt <- as.Date(facility$fst_dt)
#' # facility$source <- "facility.inpatient"
#' 
#' 
#' # transform from long to wide
#' proc_med_wide <- dcast(setDT(proc_med),
#'                        patid + conf_id + fst_dt + lst_dt~
#'                          rowid(patid, conf_id, fst_dt, lst_dt, prefix = "proc_cd"),
#'                        value.var = "proc_cd")
#' 
#' rm(proc_med)
#' 
#' # re-order columns
#' proc_names <- grep("proc_cd", colnames(proc_med_wide), value = TRUE)
#' proc_names_order <- paste0("proc_cd", 1:length(proc_names))
#' colum_order <- c("patid", "conf_id", "fst_dt", "lst_dt", proc_names_order)
#' proc_med_wide <- proc_med_wide[, colum_order]
#' 
#' 
#' # add proc_cd to diagnosis data
#' med_conf3 <- merge(x = med_conf2,
#'                    y = proc_med_wide,
#'                    by = c("patid", "conf_id", "fst_dt", "lst_dt"),
#'                    all.x = TRUE)
#' 
#' colnames(med_conf3)[which(colnames(med_conf3) == "copay_sum")] <- "copay"
#' 
#' rm(med_conf2)
#' 
#' 
#' ## read in anticoagulant claims data --------------------------------------------
#' 
#' patinfo <- fread("../separateData/patient_data.txt")
#' 
#' anticoagulant <- patinfo[, .(patid, copay, fill_dt, gen_name, days_sup,
#'                              quantity, strength, npi)]
#' 
#' # change columns to match those of med_conf
#' colnames(anticoagulant)[which(colnames(anticoagulant) == "fill_dt")] <- "fst_dt"
#' 
#' anticoagulant$source <- "anticoagulant.outpatient"
#' 
#' med_conf_ac <- merge(x = med_conf3,
#'                    y = anticoagulant,
#'                    by = c("patid", "fst_dt", "copay", "source"),
#'                    all = TRUE)
#' 
#' rm(med_conf3)
#' 
#' 
#' ### Add index VTE dates ------------------------------------------------------
#' pat_indexDt <- unique(patinfo[, .(patid, index_dt)])
#' med_conf_ac <- merge(x = med_conf_ac,
#'                      y = pat_indexDt,
#'                      by = c("patid"), all.x = TRUE)
#' 
#' 
#' 
#' 
#' # change the order of columns
#' column_order <- c("patid", "index_dt", "fst_dt", "lst_dt", "copay", "source", 
#'                   "conf_id", "pos", "description", "category",
#'                   diag1_names, diagX_names, proc_names_order, 
#'                   "gen_name", "days_sup", "quantity", "strength")
#' med_conf_ac <- med_conf_ac[, (column_order), with = FALSE]
#' 
#' 
#' write.csv(med_conf_ac, "combinedData_med_conf_ac.csv", 
#'           quote = FALSE, row.names=FALSE, na="")
#' 
#' rm(med_conf_ac)
#' 
#' 
#' 
#' ## baseline covariates ----------------------------------------------------
#' 
#' variables.unwanted <- c(colnames(patinfo)[which(colnames(patinfo) %in% colnames(anticoagulant))],
#'   "clmid", "fill_dt", "category", "brand_name", "copay_sum")
#' # variables_wanted_from_patinfo <- c("index_ac", "index_ac_dt", "ac3mo", "ac3mo_dt",
#' #     "index_cancer_dt", grep("malignancy", colnames(patinfo), value = TRUE),
#' #     "race", "division", "male", "age", "product", "surgery", "smoke", 
#' #     "index_vte_type")
#' # 
#' # 
#' add.variables <- colnames(patinfo)[which(!colnames(patinfo) %in% variables.unwanted)]
#' add.variables <- c("patid", add.variables)
#' baseline <- unique(patinfo[, (add.variables), with = FALSE])
#' 
#' write.csv(baseline, "baseline.csv", 
#'           quote = FALSE, row.names=FALSE, na="")






# mydata <- read.csv("diagData_20181020_freeze.csv", header = TRUE)








