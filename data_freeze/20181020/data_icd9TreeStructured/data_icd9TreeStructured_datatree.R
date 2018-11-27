##################################################
#' Program name: data_icd9TreeStructured.R
#' 
#' Description: The program summarizes the tree-structured
#' ICD-9 codes for individual patients.
#' 
#' Author: Mengbing Li
#' 
#' Created: 11/24/2018
#' 
#' Revisions:
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_icd9TreeStructured")

library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(data.tree)

## read in medical claims data --------------------------------------------------
medical <- fread("../diagData_20181020_freeze.csv",
                 colClasses = list(character = 1:73))

# keep diagnoses after index VTE
medical <- medical[fst_dt >= index_dt &
                     fst_dt < as.Date("2015-10-01") &
                     source %in% c("medical.outpatient", "medical.inpatient"), ]
medical$source <- NULL


## create a subset data
medical <- medical[1:10000,]

# only want patient ID and diagnoses
# ignore diagnosis date and pos
# transform diagnoses into long format
diag_names <- grep("diag", colnames(medical), value = TRUE)
diag.vars <- c("patid", diag_names)
medical <- medical[, diag.vars, with=FALSE]

#' replace trivial values with NA
#' Trivial values include: "" (blank), only having 0's in codes
for (j in diag_names) 
  set(medical, j = j, value = gsub("^$|-|^0{3,5}$", NA, medical[[j]]))

# transform into long format
medical_long <- melt(medical,
                      id.vars = c("patid"),
                      measure.vars = diag_names,
                      value.name = "icd9_raw",
                      variable.name = "diag",
                      na.rm = TRUE)
rm(medical)

# count the number of occurrences of each diagnosis in each patient
medical_long[, diag := NULL]
medical_long[, N_diag := .N, by = c("patid", "icd9_raw")]
medical_long <- unique(medical_long)
medical_long <- medical_long[order(patid, icd9_raw), ]



#' Build a tree structure of ICD-9 codes
#' The basic icd-9 code consists of 3 digits, while some are more specific
#' with 4 or five digits subdivision.
#' E codes and V codes represent external causes of injury and
#'  supplemental classification
#' V codes format: VXX(.XX)
#' E codes format: EXXX(.X)
medical_long$icd9_3digits <- with(medical_long,
    ifelse(substr(icd9_raw, 1, 1)=="E",
           substr(icd9_raw, 1, 4),
           substr(icd9_raw, 1, 3))) 
medical_long$icd9_4digits <- with(medical_long,
   ifelse(substr(icd9_raw, 1, 1)=="E",
          ifelse(nchar(icd9_raw) < 5, NA, substr(icd9_raw, 1, 5)),
          ifelse(nchar(icd9_raw) < 4, NA, substr(icd9_raw, 1, 4)))) 
medical_long$icd9_5digits <- with(medical_long,
   ifelse(substr(icd9_raw, 1, 1)=="E",
          NA,
          ifelse(nchar(icd9_raw) < 5, NA, substr(icd9_raw, 1, 5)))) 

# count the number of ICD-9 codes at each level
medical_long[, count_3digits := ifelse(icd9_3digits=="", 0L, sum(N_diag)),
             by = c("patid", "icd9_3digits")]
medical_long[, count_4digits := ifelse(is.na(icd9_4digits), 0L, sum(N_diag)),
             by = c("patid", "icd9_4digits")]
medical_long[, count_5digits := ifelse(is.na(icd9_5digits), 0L, sum(N_diag)),
             by = c("patid", "icd9_5digits")]





# convert data frame into a data.tree
# define the pathString, which describes the
# hierarchy by defining a path from the root to each leaf.

# paste3 removes NA when pasting multiple columns together
paste3 <- function(..., sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret == ""
  ret
}
medical_long$pathString <- paste3("icd9", 
                                 medical_long$patid,
                                 medical_long$icd9_3digits, 
                                 medical_long$icd9_4digits,
                                 medical_long$icd9_5digits,
                                 sep = "/")

# create count of the paths
medical_long$counts <- with(medical_long,
    ifelse(count_5digits != 0, count_5digits,
           ifelse(count_4digits != 0, count_4digits, count_3digits)))

medicalToTree <- medical_long[, .(pathString, counts)]
medicalDiag <- as.Node(medicalToTree)
print(medicalDiag, "counts", limit = 40)

# aggregate counts
medicalDiag$Do(function(x) {
  x$counts <- Aggregate(node = x,
                            attribute = "counts",
                            aggFun = sum)
  }, 
traversal = "post-order")


medicalDiag.DataFrameTree <- ToDataFrameTree(medicalDiag,
                                             "pathString", "counts")

options(max.print = 1000000)
sink("output_icd9TreeStructured.txt", append = FALSE, split = FALSE,
     type = c("output", "message"))

cat("Tree structured ICD-9 codes from medical claims data of 
    individual patients. 
    Each patid has a tree of ICD-9 codes. Each leaf is a code
    that appeared at least once between 2007-01-01 and 2015-09-30,
    regardless of the exact service date or place of service.
    The last column 'counts' is the number of times the code
    appears in the medical claims data of the patient.")
cat("\n")
print(medicalDiag.DataFrameTable[c("levelName", "counts")])

sink(NULL)





