##################################################
#' Program name: getICD9_CodeList.R
#' 
#' Description: The function processes raw ICD-9 code file
#'  and convert it into an .rds file.
#' 
#' Author: Mengbing Li
#' 
#' Created: 01/31/2019
#' 
#' Revisions:
##################################################

library(dplyr)
library(tidyr)
library(data.table)
library(igraph)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110")

codeList <- fread("../20181020/ICD-9-CM-codes/CMS32_DESC_LONG_DX.txt", sep = '\t',
                  header = FALSE)
colnames(codeList) <- "codeList"

## separate icd-9 codes and names
#' ^: matches the start of the string
#' (...): grouping in regular expressions.
#'   Each group can than be refer using \\N, with N being the No. of (...) used
#' \w: word characters, equivalent to [[:alnum:]_] or [A-z0-9_]
#' \s: space, ` `
#' .: matches any single character
#' +: matches at least 1 times
codeList$code <- sub('(^\\w+)\\s.+','\\1', codeList$codeList)
codeList$name <- sub('(^\\w+)(\\s.+)','\\2', codeList$codeList)
codeList$codeList <- NULL

codeList$code <- trimws(codeList$code, which = "right")
codeList$name <- trimws(codeList$name)


saveRDS(codeList, file = "icd9_CompleteCodeList.rds")
