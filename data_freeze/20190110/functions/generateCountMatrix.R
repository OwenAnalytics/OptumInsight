##################################################
#' Program name: generateCountMatrix.R
#' 
#' Description: Produce a counting matrix of ICD-9 codes per patient.
#' The output is a matrix where each cell is a non-negative integer.
#' 
#' Author: Mengbing Li
#' 
#' Created: 01/22/2019
#' 
#' Revisions: 
##################################################

#' Input: 
#' data = input data table
#' x = variable of the column
#' startDate = the date on which we start conting the occurrences of ICD-9 codes
#' stopDate = the date by which we stop counting the occurrences of ICD-9 codes
generateCountMatrix <- function(data, x,
                    startDate = "index_dt", 
                    stopDate = as.Date("2015-09-30")){
  if(startDate != "index_dt"){
    data <- data[fst_dt >= startDate, ]
  }
  if(stopDate != as.Date("2015-09-30")) data <- data[fst_dt <= stopDate, ]
  # count the number of times each ICD-9 code appears in each patient
  # and store the counts in a sparse matrix
  CountTable <- table(data[, c("patid", x), with=FALSE])
  dimensions <- dim(CountTable)
  matrix(CountTable, nrow = dimensions[1], ncol = dimensions[2])
}

# check: diagInpatientCount <- generateCountMatrix(diagInpatient, "icd9_3digits")