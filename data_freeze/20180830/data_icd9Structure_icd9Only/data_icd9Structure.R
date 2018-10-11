##################################################
#' Program name: data_icd9Structure.R
#' 
#' Description: The program identifies the 3-level tree structure in ICD-9 codes
#' from medical claims and confinement claims data.
#' 
#' Author: Mengbing Li
#' 
#' Created: 09/03/2018
#' 
#' Revisions:
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20180830/data_icd9Structure")

library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(grid)

#' get the distribution of ICD-9 codes in medical and confinement data
#' Idea: 
#' 1) remove duplicate rows from medical and confinement data
#' 2) transform diagnosis1-5 or 1-25 from wide to long
#' 3) remove duplicate ICD-9 codes, matching on patid and admit_dt or
#'   fst_dt
#' 4) merge medical and confinement diagnosis and remove duplicates
#' (steps 3 and 4 are not reversed because the medical data is too large.
#' So we reduce data size and then proceed)
#' 5) extract the first three letters as level 1 ICD-9 code, and first
#' four letters as level 2 ICD-9 codes

# get icd-9 codes from medical data
medical <- fread("../medicalClaims.txt",
                 select = c("patid", "Conf_Id", paste0("Diag", 1:25), "Fst_Dt"),
                 colClasses = list(character=c("patid", "Conf_Id",
                              paste0("Diag", 1:25), "Fst_Dt")))
colnames(medical) <- tolower(colnames(medical))
medical$fst_dt <- as.Date(medical$fst_dt)
# since we are only interested in ICD-9 codes, records before 2015-10-01 are discarded
medical <- medical[medical$fst_dt < as.Date("2015-10-01"),]

medical.distinct <- distinct(medical)
# rm(medical)

#' replace trivial values with NA
#' Trivial values include: "" (blank), only having 0's in codes
medical.distinct <- data.frame(apply(medical.distinct, 2,
                              function(x) gsub("^$|-|0{4,5}|^0{3}$", NA, x)))
medical.distinct[, which(colnames(medical.distinct)!="fst_dt")] <- lapply(medical.distinct[,
              which(colnames(medical.distinct)!="fst_dt")], as.character)
medical.distinct$fst_dt <- as.Date(medical.distinct$fst_dt)
# convert from wide to long and keep distinct values
medical.distinct.long <- melt(medical.distinct,
          id.vars=c("patid", "conf_id", "fst_dt"),
          value.name = "icd9",
          variable.name = "diagnosis",
          na.rm=TRUE)
medical.distinct.long <- distinct(medical.distinct.long)
medical.distinct.long <- medical.distinct.long[order(medical.distinct.long$patid,
                                                      medical.distinct.long$fst_dt),]


# get icd-9 codes from confinement data
confinement <- readRDS("../confinement.rds")
confinement2 <- confinement[, c("patid", "conf_id", "admit_dt", paste0("diag", 1:5))]
colnames(confinement2)[which(colnames(confinement2)=="admit_dt")] <- "fst_dt"
confinement2$fst_dt <- as.Date(confinement2$fst_dt)
confinement2 <- confinement2[confinement2$fst_dt < as.Date("2015-10-01"),]

# note: rows are already distinct in confinement2
# replace blank values or trivial values with NA
confinement2 <- data.frame(apply(confinement2, 2,
                                 function(x) gsub("^$|-|0{4,5}|^0{3}$", NA, x)))
confinement2[, which(colnames(confinement2)!="fst_dt")] <- lapply(confinement2[,
              which(colnames(confinement2)!="fst_dt")], as.character)
# convert from wide to long and keep distinct values
confinement2.long <- melt(confinement2,
                          id.vars=c("patid", "conf_id", "fst_dt"),
                          value.name = "icd9",
                          variable.name = "diagnosis",
                          na.rm=TRUE)
confinement2.long <- distinct(confinement2.long)
confinement2.long <- medical.distinct.long[order(confinement2.long$patid,
                                                 confinement2.long$fst_dt),]

# merge medical records and confinement records
medical_confinement <- rbind(medical.distinct.long, confinement2.long)
# remove dot "." in icd-9 code
medical_confinement$icd9 <- gsub("\\.", "", medical_confinement$icd9)
# remove additional icd-10 codes
icd10_index <- which(!substr(medical_confinement$icd9, 1, 1) %in% c("V", "E", as.character(0:9)))
medical_confinement <- medical_confinement[-icd10_index,]

#' Build a tree structure of ICD-9 codes
#' The basic icd-9 code consists of 3 digits, while some are more specific
#' with 4 or five digits subdivision.
#' E codes and V codes represent external causes of injury and
#'  supplemental classification
#' V codes format: VXX(.XX)
#' E codes format: EXXX(.X)
data_icd9 <- data.frame(medical_confinement[, "icd9"])
colnames(data_icd9) <- "raw"
data_icd9$raw <- as.character(data_icd9$raw)
data_icd9$icd9_3digits <- with(data_icd9, ifelse(substr(raw, 1, 1)=="E",
                                                 substr(raw, 1, 4),
                                                 substr(raw, 1, 3))) 
data_icd9$icd9_4digits <- with(data_icd9,
        ifelse(substr(raw, 1, 1)=="E",
               ifelse(nchar(raw) < 5, "", substr(raw, 1, 5)),
               ifelse(nchar(raw) < 4, "", substr(raw, 1, 4)))) 

data_icd9$icd9_5digits <- with(data_icd9,
        ifelse(substr(raw, 1, 1)=="E",
               "",
               ifelse(nchar(raw) < 5, "", substr(raw, 1, 5)))) 
data_icd9 <- data_icd9[order(data_icd9$icd9_3digits,
                             data_icd9$icd9_4digits,
                             data_icd9$icd9_5digits),]


# count the number of roots at each level
data_icd9 <- data.table(data_icd9)
data_icd9[, count_3digits := ifelse(icd9_3digits=="", -1L, .N), by = as.character(icd9_3digits)]
data_icd9[, count_4digits := ifelse(icd9_4digits=="", -1L, .N), by = icd9_4digits]
data_icd9[, count_5digits := ifelse(icd9_5digits=="", -1L, .N), by = icd9_5digits]

data_icd9.counts <- unique(data_icd9)

## find the number of roots at each level
sink("output_icd9Structure.txt", append = FALSE, split = FALSE)
print("The number of 3-digit ICD-9 codes is "); length(unique(data_icd9.counts$icd9_3digits)) #1190
print("The number of 4-digit ICD-9 codes is "); length(unique(data_icd9.counts$icd9_4digits)) #5836
print("The number of 5-digit ICD-9 codes is "); length(unique(data_icd9.counts$icd9_5digits)) #6106
sink(NULL)


## histogram of icd-9 code counts at each level
plot.data_3digits <- unique(data_icd9.counts[, "icd9_3digits", "count_3digits"])
colnames(plot.data_3digits) <- c("count", "icd9_level")
plot.data_4digits <- unique(data_icd9.counts[data_icd9.counts$count_4digits>=0, "icd9_4digits", "count_4digits"])
colnames(plot.data_4digits) <- c("count", "icd9_level")
plot.data_5digits <- unique(data_icd9.counts[, "icd9_5digits", "count_5digits"])
colnames(plot.data_5digits) <- c("count", "icd9_level")
plot.data <- rbind(plot.data_3digits, plot.data_4digits, plot.data_5digits)


ggplot(plot.data, aes(x=count)) + 
  geom_histogram(binwidth=15, colour="salmon", fill="white") + 
  facet_grid(icd9_level ~ .) +
  scale_x_continuous(limits = c(0, 2000)) +
  labs(title="Histograms of counts of ICD-9 codes at each level, with <= 2000 counts",
       x="Counts of ICD-9 codes",
       y="Frequency") 
ggsave("histogram_countICD9.pdf")


sink("output_icd9Structure.txt", append = TRUE, split = FALSE)

cat("\n")

print("Summary of counts of 3-digit ICD-9 codes:")
summary(data_icd9.counts$count_3digits)

print("Summary of counts of 4-digit ICD-9 codes:")
summary(data_icd9.counts$count_4digits)

print("Summary of counts of 5-digit ICD-9 codes:")
summary(data_icd9.counts$count_5digits)
sink(NULL)


