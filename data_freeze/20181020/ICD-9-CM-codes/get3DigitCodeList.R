##################################################
#' Program name: get3DigitCodeList.R
#' 
#' Description: The program cleans icd9_3digitCodeList.RTF and creates
#' a txt file of three-digit ICD-9 codes and associated names.
#' 
#' Author: Mengbing Li
#' 
#' Created: 12/17/2018
#' 
#' Revisions: 
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/ICD-9-CM-codes")

library(dplyr)
library(tidyr)
library(data.table)

## read in lines
#' codeList <- readLines("icd9_3digitCodeList_raw.txt")
#' codeList <- as.data.frame(codeList)
#' codeList$codeList <- as.character(codeList$codeList)
#' 
#' ## remove lines not starting with an ICD-9 code
#' codeList$codeList[grep('^\\d{3}', codeList$codeList, inv = T)] <- NA
#' codeList <- na.omit(codeList)
#' 
#' ## separate icd-9 codes and names
#' #' ^: matches the start of the string
#' #' (...): grouping in regular expressions. 
#' #'   Each group can than be refer using \\N, with N being the No. of (...) used
#' #' \w: word characters, equivalent to [[:alnum:]_] or [A-z0-9_]
#' #' \s: space, ` `
#' #' .: matches any single character
#' #' +: matches at least 1 times
#' codeList$code <- sub('(^\\w+)\\s.+','\\1', codeList$codeList)
#' codeList$name <- sub('(^\\w+)(\\s.+)','\\2', codeList$codeList)
#' codeList$codeList <- NULL
#' 
#' codeList$code <- trimws(codeList$code, which = "right")
#' codeList$name <- trimws(codeList$name)
#' 
#' write.table(codeList, file = "icd9_3digitCodeList.txt",
#'             quote = FALSE, sep = "\t", 
#'             row.names = FALSE)




load("icd9ChaptersMajor.RData")
# codes
icd9Major <- unname(unlist(icd9ChaptersMajor))
#disease names
icd9MajorNames <- names(icd9ChaptersMajor)
codeList <- data.frame(code = icd9Major,
                       name = icd9MajorNames)
write.table(codeList, file = "icd9_3digitCodeList2.txt",
            quote = FALSE, sep = "\t", 
            row.names = FALSE)







