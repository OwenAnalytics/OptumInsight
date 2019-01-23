##################################################
#' Program name: data_icd9Summary.R
#' 
#' Description: The program summarizes individual-level
#' ICD-9 patterns. We ignore the temporal effect.
#' 
#' Author: Mengbing Li
#' 
#' Created: 12/05/2018
#' 
#' Revisions:
##################################################

setwd('C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/data_icd9Summary')

library(dplyr)
library(tidyr)
library(data.table)
library(Matrix)

# extract 3-digit ICD-9 codes
source("../functions/icd9_hierarchy.R")
diagInpatient <- fread("../diagInpatient_long.csv")
diagInpatient$icd9_3digits <- icd9_3digits(diagInpatient, icd9_raw)

# count the number of times each ICD-9 code appears in each patient
# and store the counts in a sparse matrix
diagInpatientCount <- table(diagInpatient[, c("patid", "icd9_3digits")])
diagInpatientCount <- Matrix(diagInpatientCount, sparse = TRUE)

# count the number of unique ICD-9 codes each patient has
NUniqueCodeByPatient <- apply(diagInpatientCount, 1, function(x) sum(x>0))

hist(NUniqueCodeByPatient, 
     main = "Number of unique 3-digit ICD-9 codes in each patient after index VTE",
     xlab = "Number of codes")







## read in medical claims data --------------------------------------------------
medical0 <- fread("../diagData_20181020_freeze.csv",
                  colClasses = list(character = 1:73))

# keep diagnoses after index VTE
medical <- medical0[fst_dt >= index_dt &
                      fst_dt < as.Date("2015-10-01") &
                      source %in% c("medical.outpatient", "medical.inpatient"), ]
medical$source <- NULL

confinement <- medical0[fst_dt >= index_dt &
                          fst_dt < as.Date("2015-10-01") &
                          source == "confinement.inpatient" &
                          (! patid %in% unique(medical$patid)), ]
confinement$source <- NULL

rm(medical0)


## create a subset data
# medical <- medical[patid %in% patientIndx,]

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

## repeat the same procedures for confinement data
diag_names <- grep("diag", colnames(confinement), value = TRUE)
diag.vars <- c("patid", diag_names)
confinement <- confinement[, diag.vars, with=FALSE]

#' replace trivial values with NA
#' Trivial values include: "" (blank), only having 0's in codes
for (j in diag_names) 
  set(confinement, j = j, value = gsub("^$|-|^0{3,5}$", NA, confinement[[j]]))

# transform into long format
confinement_long <- melt(confinement,
                         id.vars = c("patid"),
                         measure.vars = diag_names,
                         value.name = "icd9_raw",
                         variable.name = "diag",
                         na.rm = TRUE)
rm(confinement)

medical_long <- rbind(medical_long, confinement_long)

# count the number of occurrences of each diagnosis in each patient
medical_long[, diag := NULL]
medical_long[, N_diag := .N, by = c("patid", "icd9_raw")]

# count the total number of diagnosis codes of each patient across records
medical_long[, N_diag_total := .N, by = c("patid")]

N_totalDiagnosis <- unique(medical_long[, .(patid, N_diag_total)])

medical_long <- unique(medical_long)
medical_long <- medical_long[order(patid, icd9_raw), ]


# only want ICD-9 codes
medical_long <- medical_long[substr(icd9_raw, 1, 1) %in% c(0:9, "E", "V"),]

# create roots
medical_long$icd9_3digits <- with(medical_long,
                                  ifelse(substr(icd9_raw, 1, 1)=="E",
                                         substr(icd9_raw, 1, 4),
                                         substr(icd9_raw, 1, 3))) 


### Find patterns -----------------------------------------------------------

#' patterns:
#' 3-digit  4-digit  5-digit  pattern_code
#'       1        0        0             3
#'       1        1        0             4
#'       1        1        1             5
#'       0        0        0             0
#' output data:
#' columns: roots of the ICD-9 trees
#' rows: individual patients
#' cells: pattern_code


load("diagnosisInformation_list.RData")

# initialize sparse matrix
roots <- unique(diagnosisInformation$roots)
patients <- unique(diagnosisInformation$patients)
patternInformation <- Matrix(0, ncol = length(roots), nrow = length(patients))
rownames(patternInformation) <- patients
colnames(patternInformation) <- roots

## find the level of each code
#' if multiple levels of the same tree occur, then take the highest level
# medical_long$level <- nchar(medical_long$icd9_raw)

# medical_long <- medical_long[1:1000,]
medical_long <- medical_long[,level := max(nchar(icd9_raw)), by = .(patid, icd9_3digits)]
medical_long <- medical_long[!duplicated(medical_long[,c("patid", "icd9_3digits", "level")]),]

for(patient in patients){
  patientData <- medical_long[patid == patient, ]
  patternInformation[patient, patientData$icd9_3digits] <- patientData$level
}

# save(patternInformation, file = "patternInformation.RData")





### Summarize individual-specific patterns ----------------------------------
# patternInformation <- patternInformation[1:20,]
# patients <- patients[1:20]

individualTableList <- apply(patternInformation, 1, table)

completeTable <- function(x){
  tableNames <- names(x)
  columnsWanted <- as.character(c(0, 3:5))
  columnsNotInTable <- setdiff(columnsWanted, tableNames)
  x[columnsNotInTable] <- 0
  x <- x[columnsWanted]
  x
}

individualTableList <- lapply(individualTableList, completeTable)

individualTables <- data.table(do.call("rbind", individualTableList))
individualTables$patid <- patients
individualTables <- individualTables[, c(5, 1:4)]

## calculate the proportions of levels of ICD-9 codes within each patient
individualTables <- data.frame(individualTables)
individualTables$N_diagTotal <- rowSums(individualTables[, 3:5])
individualTables[, 7:9] <- lapply(individualTables[, 3:5], 
    function(x) x/individualTables$N_diagTotal)
colnames(individualTables)[7:9] <- paste("prop_individual", 3:5, sep="")




### Summarize code-specific patterns ----------------------------------------
codeTableList <- apply(patternInformation, 2, table)
codeTableList <- lapply(codeTableList, completeTable)

codeTables <- data.table(do.call("cbind", codeTableList))
codeTables$pattern <- c(0, 3:5)
codeTables <- codeTables[, c(ncol(codeTables), 1:(ncol(codeTables)-1)), with = FALSE]

## calculate the proportions of levels of ICD-9 codes within each patient
codeTables <- data.frame(t(codeTables))
colnames(codeTables) <- as.character(codeTables[1,])
codeTables <- codeTables[-1,]
codeTables$pattern <- rownames(codeTables)
rownames(codeTables) <- 1:nrow(codeTables)
codeTables$N_diagTotal <- rowSums(codeTables[, 2:4])
codeTables[, 6:8] <- lapply(codeTables[, 2:4], 
                            function(x) x/codeTables$N_diagTotal)
colnames(codeTables)[6:8] <- paste("prop_code", 3:5, sep="")



rm(list=setdiff(ls(), c("patternInformation", "individualTables", "codeTables")))
save.image("diagnosisPatterns.RData")




### Look at results ---------------------------------------------------------
# library(Matrix)
# load("diagnosisPatterns.RData")

individualPatternMeans <- colMeans(individualTables[,2:5])
codePatternMeans <- rowMeans(codeTables[,2:ncol(codeTables)])

# these two are the same
log(individualPatternMeans / sum(individualPatternMeans))
log(codePatternMeans / sum(codePatternMeans))

print(patternInformation[1:10, 1:10])



## plots of individuals -------------------------------------
## counts
individualTables <- data.table(individualTables)
colnames(individualTables)[2:5] <- paste("length", c(0,3:5), sep = "")
individualTables <- individualTables[order(-length4, -length5, -length3),]

col_set <- c("green", "red", "blue")
pdf("results_individuals_counts.pdf", width=100, height=5)
matplot((individualTables[, 3:5]), type="l", lty = "solid", lwd = 0.5, xaxt='n',
        col = col_set, xlab = "Patients", ylab = "Counts",
        main = "Counts of diagnosis codes for individuals (ordered by levels 4, 5, and 3)")
axis(1, at=1:(nrow(individualTables)),
     labels = rownames(individualTables), cex.axis=0.5, srt=45)
legend("topleft", colnames(individualTables)[3:5], col = col_set,
       cex = 0.8, fill = col_set)
dev.off()


## proportions
individualTables <- data.table(individualTables)
individualTables <- individualTables[
  order(-prop_individual4, -prop_individual5, -prop_individual3),]

col_set <- c("green", "red", "blue")
pdf("results_individuals_proportions.pdf", width=100, height=5)
matplot((individualTables[, 7:9]), type="l", lty = "solid", lwd = 0.5, xaxt='n',
        col = col_set, xlab = "Patients", ylab = "Counts",
        main = "Proportioins of diagnosis codes for individuals (ordered by levels 4, 5, and 3)")
axis(1, at=1:(nrow(individualTables)),
     labels = rownames(individualTables), cex.axis=0.5, srt=45)
legend("topleft", colnames(individualTables)[7:9], col = col_set,
       cex = 0.8, fill = col_set)
dev.off()




### plots of codes -------------------------------------
## counts
codeTables <- data.table(codeTables)
colnames(codeTables)[1:4] <- paste("length", c(0,3:5), sep = "")
codeTables <- codeTables[order(-length4, -length5, -length3),]

col_set <- c("green", "red", "blue")
pdf("results_codes_counts.pdf", width=100, height=5)
matplot(codeTables[, 2:4], type="l", lty = "solid", lwd = 0.5, xaxt='n',
        col = col_set, xlab = "Codes", ylab = "Counts",
        main = "Counts of diagnosis codes at different levels (ordered by levels 4, 5, and 3)")
axis(1, at=1:(nrow(codeTables)),
     labels = codeTables$pattern, cex.axis=0.5, srt=45)
legend("topleft", colnames(codeTables)[2:4], col = col_set,
       cex = 0.8, fill = col_set)
dev.off()


## proportions
codeTables <- data.table(codeTables)
codeTables <- codeTables[order(-prop_code4, -prop_code5, -prop_code3),]

col_set <- c("green", "red", "blue")
pdf("results_codes_proportions.pdf", width=100, height=5)
matplot(codeTables[, 6:8], type="l", lty = "solid", lwd = 0.5, xaxt='n',
        col = col_set, xlab = "Patients", ylab = "Counts",
        main = "Proportioins of diagnosis codes at different levels (ordered by levels 4, 5, and 3)")
axis(1, at=1:(nrow(codeTables)),
     labels = rownames(codeTables), cex.axis=0.5, srt=45)
legend("topleft", colnames(codeTables)[6:8], col = col_set,
       cex = 0.8, fill = col_set)
dev.off()
