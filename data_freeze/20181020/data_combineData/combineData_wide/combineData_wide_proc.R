##################################################
#' Program name: combineData_wide_proc.R
#' 
#' Description: The program merges proc_cd from medical and facility 
#' data sets. The final data set is in wide format, ordered 
#' chronologically by each patient.
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
##################################################

library(data.table)
library(reshape2)
library(xlsx)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_combineData/combineData_wide")

## read in medical claims data --------------------------------------------------
medical <- fread("../../separateData/medicalClaims.txt",
                 select = c("patid", "Conf_Id", "Copay", paste0("Diag", 1:25),
                            "Fst_Dt", "Lst_Dt", "Proc_Cd", "Pos"),
                 colClasses = list(character=c("patid", "Conf_Id", 
                            paste0("Diag", 1:25), "Fst_Dt", "Lst_Dt", "Proc_Cd", "Pos"),
                            numeric = c("Copay")))
colnames(medical) <- tolower(colnames(medical))

# remove records after ICD-10 became effective
# medical$fst_dt <- as.Date(medical$fst_dt)
# medical <- medical[medical$fst_dt < "2015-10-01",]

# test <- medical[1:10000]

# example of multiplicity in diagnosis codes at deeper tree levels
# example <- test[patid == "802666500107193" & fst_dt == "2015-07-18", ]

# change empty diagnoses to NA
# for (j in paste0("diag", 1:25)) 
#   set(medical, j = j, value = gsub("^$|-|^0{3,5}$", NA, medical[[j]]))
# 
# # change empty or invalid proc_cd to NA
# # range of invalid CPT codes: < 00100
# medical$proc_cd <- with(medical, ifelse(proc_cd < "00100", NA, proc_cd))




## read in pos details -------------------------------------------------
pos_detail <- read.xlsx("../../separateData/All_codes_ICD9_NDC_HCPCS.xlsx",
                        sheetName = "POS", header = TRUE)
colnames(pos_detail) <- tolower(colnames(pos_detail))
pos_detail$pos <- as.character(pos_detail$pos)


# patinfo <- fread("../../separateData/patient_data.txt")
# patinfo <- unique(patinfo[, .(patid, index_dt)])


#' The reclass function unifies two data.tables:
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




### combine the data sets in split data because data is too large for RAM
patids <- unique(medical$patid)
group.size <- 500
x <- seq_along(patids)
patids.split <- split(patids, ceiling(x/group.size))






### deal with proc_cd ------------------------------------------------

# read in facility claims data 
facility <- fread("../../separateData/facilityClaims.txt",
                  select = c("patid", "fst_dt", "Proc_Cd"),
                  colClasses = list(character=c("patid", "fst_dt", "Proc_Cd")))
colnames(facility) <- tolower(colnames(facility))
facility <- facility[proc_cd != "", ]
facility <- unique(facility)


for(i in 1:length(patids.split)){
  med <- subset(medical, patid %in% patids.split[[i]])
  # reduce the size of medical data
  medical <- subset(medical, ! (patid %in% patids.split[[i]]))
  
  med <- med[med$fst_dt < "2015-10-01",]
  
  # change empty diagnoses to NA
  for (j in paste0("diag", 1:25)) 
    set(med, j = j, value = gsub("^$|-|^0{3,5}$", NA, med[[j]]))
  
  # change empty or invalid proc_cd to NA
  # range of invalid CPT codes: < 00100
  med$proc_cd <- with(med, ifelse(proc_cd < "00100", NA, proc_cd))
  
  # add source variable
  med$source0 <- ifelse(med$conf_id == "", "medical.outpatient", "medical.inpatient")
  
  # get proc_cd from medical data
  # this step is moved here so that medical data can be deleted from memory
  proc_med <- unique(med[, .(patid, conf_id, pos, fst_dt, lst_dt, proc_cd, source0)])
  proc_med <- proc_med[is.na(proc_med$proc_cd) == FALSE, ]
  
  rm(med)
  
  
  ## deal with proc_cd from facility claims --------------------------------------
  
  fac <- subset(facility, patid %in% patids.split[[i]])
  facility <- subset(facility, ! (patid %in% patids.split[[i]]))
  
  # add lst_dt to match medical data
  fac$lst_dt <- ""
  fac$conf_id <- ""
  
  # name it as source0 because source cannot go into dcast as a variable
  fac$source0 <- "facility.inpatient" 
  fac$pos <- ""
  
  # change empty or invalid proc_cd to NA
  # range of invalid CPT codes: < 00100
  fac$proc_cd <- with(fac, ifelse(proc_cd < "00100", NA, proc_cd))
  fac <- fac[is.na(proc_cd) == FALSE, ]
  fac <- fac[, colnames(proc_med), with = FALSE]
  
  proc_med_fac <- unique(rbind(proc_med, fac))
  
  rm(fac)
  
  # transform from long to wide
  proc_med_wide <- data.table(dcast(setDT(proc_med_fac),
                         patid + conf_id + pos + fst_dt + lst_dt + source0 ~
                           rowid(patid, conf_id, fst_dt, lst_dt, pos, prefix = "proc_cd"),
                         value.var = "proc_cd"))
  proc_med_wide <- proc_med_wide[order(patid, fst_dt, lst_dt),]
  rm(proc_med)

  # ### Add index VTE dates ------------------------------------------------------
  # pat_indexDt <- unique(patinfo[patid %in% patids.split[[i]], .(patid, index_dt)])
  # proc_med_wide <- merge(x = proc_med_wide,
  #                        y = pat_indexDt,
  #                        by = c("patid"), all.x = TRUE)
  
  ## add pos details -------------------------------------------------
  proc_med_wide <- merge(x = proc_med_wide,
                         y = pos_detail,
                         by = "pos", all.x = TRUE)
  
  
  # merge data iteratively
  if(i == 1){
    final <- data.table(matrix(NA, nrow = 0, ncol = ncol(proc_med_wide)))
    colnames(final) <- c(colnames(proc_med_wide))
  }
  
  reclass.out <- reclass(final, proc_med_wide)
  final <- reclass.out[[1]]
  proc_med_wide <- reclass.out[[2]]
  
  rm(reclass.out)
  
  final <- rbind(final, proc_med_wide)
  
  rm(proc_med_wide)
  
}

# re-order columns
colnames(final)[which(colnames(final) == "source0")] <- "source"
proc_names <- grep("proc_cd", colnames(final), value = TRUE)
proc_names_order <- paste0("proc_cd", 1:length(proc_names))
colum_order <- c("patid", "source", "conf_id", "pos", "description",
                 "category", "fst_dt", "lst_dt", proc_names_order)
final <- final[, colum_order, with = FALSE]

write.csv(final, "procData_20181020_freeze.csv",
          quote = FALSE, row.names=FALSE, na="")


