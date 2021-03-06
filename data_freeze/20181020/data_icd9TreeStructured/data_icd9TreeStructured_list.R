##################################################
#' Program name: data_icd9TreeStructured_list.R
#' 
#' Description: The program summarizes the tree-structured
#' ICD-9 codes for individual patients.
#' The output is a list containing medical diagnoses of each
#' patient, including code structures and counts.
#' 
#' Author: Mengbing Li
#' 
#' Created: 11/24/2018
#' 
#' Revisions: 
#' 12/06/2018 - 1. Add names of the ICD-9 codes;
#'  2. Find out diagnosis codes that are not available in the ICD-9 dictionary
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_icd9TreeStructured")

library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(data.tree)
library(Matrix)

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
medical_long <- unique(medical_long)
medical_long <- medical_long[order(patid, icd9_raw), ]



#' Build a tree structure of ICD-9 codes
#' The basic icd-9 code consists of 3 digits, while some are more specific
#' with 4 or five digits subdivision.
#' E codes and V codes represent external causes of injury and
#'  supplemental classification
#' V codes format: VXX(.XX)
#' E codes format: EXXX(.X)
# only want ICD-9 codes
medical_long <- medical_long[substr(icd9_raw, 1, 1) %in% c(0:9, "E", "V")]

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




### Create a list containing medical diagnoses of individual patients -----------------
#' diagnosisInformation = list
#' patids = patient id
#' roots = root of the ICD-9 tree
#' trees = sparse matrices that represent the tree-structured ICD-9 codes
#' nodes = character vectors containing nodes of each tree in trees
#' counts = numeric vectors containing the number of times each node appears
#'   in the patient's medical record

## initialize containers to store diagnosis codes
nodes <- list()
diagTrees <- list()

## first obtain all diagnoses that appear in medical claims data
allDiag <- unique(medical_long[, .(icd9_3digits, icd9_4digits, icd9_5digits)])

# create tree structure of the ICD-9 codes
diagRoots <- unique(allDiag$icd9_3digits)

# iterate over each root to create the corresponding tree
for(currentDiagRoots in diagRoots){
  subDiagData <- allDiag[icd9_3digits == currentDiagRoots,]
  
  # convert path representation to network representation
  currentDiagNetwork <- subDiagData[, 1:2]
  colnames(currentDiagNetwork) <- c("parent", "child")
  if (!all(is.na(currentDiagNetwork$child))) { # if a level-4 code is present
    # obtain edges between level 4 and level 5
    diagNetwork45 <- subDiagData[is.na(icd9_5digits) == FALSE, 2:3] 
    if (nrow(diagNetwork45) != 0) { # if edges are present
      colnames(diagNetwork45) <- c("parent", "child")
      currentDiagNetwork <- rbind(currentDiagNetwork, diagNetwork45)
    }
  }
  currentDiagNetwork <- currentDiagNetwork[order(parent, child)]
  
  # convert network representation to adjacency matrix representation
  currentDiagNodes <- unique(na.omit(unlist(currentDiagNetwork, use.names=F)))
  currentDiagTree <- table(currentDiagNetwork[, lapply(.SD, factor, currentDiagNodes)])
  currentDiagTree <- Matrix((matrix(currentDiagTree, nrow = nrow(currentDiagTree)) > 0)*1)
  
  nodes[[paste(currentDiagRoots)]] <- currentDiagNodes
  diagTrees[[paste(currentDiagRoots)]] <- currentDiagTree
}



## second obtain diagnosis counts of individual patients
patids <- unique(medical_long$patid)
patient <- c()
roots <- c()
counts <- list()

# iterate over individual patients
for(currentPatid in patids){
  currentPatidSubdata <- medical_long[patid == currentPatid,]
  currentPatidSubdata <- currentPatidSubdata[order(icd9_3digits, icd9_4digits, icd9_5digits),]
  currentPatidRoots <- unique(currentPatidSubdata$icd9_3digits)
  
  # iterate over each root of diagnosis trees
  for(currentPatidRoot in currentPatidRoots){
    # get the counts of present nodes
    currentPatidcurrentDiagLevel3 <- unique(currentPatidSubdata[
      icd9_3digits == currentPatidRoot, .(icd9_3digits, count_3digits)])
    colnames(currentPatidcurrentDiagLevel3) <- c("node", "count")
    currentPatidcurrentDiagLevel4 <- na.omit(unique(currentPatidSubdata[
      icd9_3digits == currentPatidRoot, .(icd9_4digits, count_4digits)]))
    colnames(currentPatidcurrentDiagLevel4) <- c("node", "count")
    currentPatidcurrentDiagLevel5 <- na.omit(unique(currentPatidSubdata[
      icd9_3digits == currentPatidRoot, .(icd9_5digits, count_5digits)]))
    colnames(currentPatidcurrentDiagLevel5) <- c("node", "count")
    
    currentPatidcurrentDiag <- rbind(currentPatidcurrentDiagLevel3,
      currentPatidcurrentDiagLevel4, currentPatidcurrentDiagLevel5)
    
    # fill the counts of absent nodes in the patient with zeros
    nodesInCurrentTree <- nodes[[currentPatidRoot]]
    nodesToAdd <- setdiff(nodesInCurrentTree, currentPatidcurrentDiag$node)
    dataToAdd <- list('node' = nodesToAdd, 'count' = rep(0, length(nodesToAdd)))
    currentPatidcurrentDiag <- rbindlist(list(currentPatidcurrentDiag, dataToAdd))
    currentPatidcurrentDiag <- currentPatidcurrentDiag[order(node),]
    
    patient <- c(patient, currentPatid)
    roots <- c(roots, currentPatidRoot)
    
    counts[[paste(currentPatid, ":", currentPatidRoot, sep = "")]] <- 
      currentPatidcurrentDiag$count
  }
}


diagnosisInformation <- list(
  "patients" = patient,
  "roots" = roots,
  "trees" = diagTrees,
  "nodes" = nodes,
  "counts" = counts
)

rm(list=setdiff(ls(), "diagnosisInformation"))


# save(diagnosisInformation, file="diagnosisInformation.RData")





### 12/06/2018 revision: add names of the ICD-9 codes to the list ------------------
#' Using ICD-9 Version 32 Full and Abbreviated Code Titles 
#'   Effective October 1, 2014
#' There are no new ICD-9-CM code updates effective October 1, 2014.
#' Reference: https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/addendum.html

# setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_icd9TreeStructured")
# 
# library(Matrix)
# library(xlsx)
# 
# load("diagnosisInformation_list.RData")

codeNames <- data.table(readLines("ICD-9-CM-v32-master-descriptions/CMS32_DESC_LONG_DX.txt"))

## separate icd-9 codes and names
#' ^: matches the start of the string
#' (...): grouping in regular expressions. 
#'   Each group can than be refer using \\N, with N being the No. of (...) used
#' \w: word characters, equivalent to [[:alnum:]_] or [A-z0-9_]
#' \s: space, ` `
#' .: matches any single character
#' +: matches at least 1 times
codeNames$code <- sub('(^\\w+)\\s.+','\\1', codeNames$V1)
codeNames$name <- sub('(^\\w+)(\\s.+)','\\2', codeNames$V1)
codeNames$V1 <- NULL

# add names
for(node in names(diagnosisInformation$nodes)){
  nodesInTree <- diagnosisInformation$nodes[[node]]
  names(nodesInTree) <- codeNames$name[match(nodesInTree, codeNames[, code])]
  diagnosisInformation$nodes[[node]] <- nodesInTree
}

save(diagnosisInformation, file="diagnosisInformation_list.RData")







### Find out diagnosis codes that are not available in the ICD-9 dictionary --------
allDiagICD9 <- allDiag[substr(icd9_3digits, 1, 1) %in% c(0:9, "E", "V")]
allDiagICD9$is.found3 <- allDiagICD9$icd9_3digits %in% codeNames$code
allDiagICD9$is.found4 <- allDiagICD9$icd9_4digits %in% codeNames$code
allDiagICD9$is.found5 <- allDiagICD9$icd9_5digits %in% codeNames$code

allDiagICD9$is.found <- apply(allDiagICD9[, 4:6], 1, any)

# total number of unique diagnosis codes, regardless of levels
nrow(allDiagICD9)

# number of diagnosis codes that are not found in ICD-9 dictionary
sum(!allDiagICD9$is.found)














