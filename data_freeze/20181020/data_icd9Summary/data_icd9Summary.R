##################################################
#' Program name: data_icd9Summary.R
#' 
#' Description: The program summarizes patient-level
#' ICD-9 codes after index VTE date.
#' 
#' Author: Mengbing Li
#' 
#' Created: 11/10/2018
#' 
#' Revisions:
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_icd9Summary")

library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(grid)

## read in medical claims data --------------------------------------------------
medical <- fread("../diagData_20181020_freeze.csv",
                 colClasses = list(character = 1:73))

# keep diagnoses after index VTE
medical <- medical[fst_dt >= index_dt &
                     fst_dt < as.Date("2015-10-01") &
                     source %in% c("medical.outpatient", "medical.inpatient"), ]
medical$source <- NULL


# transform diagnoses into long format
diag_names <- grep("diag", colnames(medical), value = TRUE)
diag.vars <- c("patid", "index_dt", diag_names, "fst_dt")
medical_long <- melt(medical[, (diag.vars), with = FALSE],
                     id.vars = c("patid", "index_dt", 
                                 "fst_dt"),
                     measure.vars = diag_names,
                     value.name = "icd9_raw",
                     variable.name = "diag",
                     na.rm = TRUE)
rm(medical)

## create a test data set
# medical_long <- medical_long[1:10000, ]


# only need the diagnosis codes from medical data
medical_long <- data.table(medical_long[, icd9_raw])
colnames(medical_long) <- "icd9_raw"

#' replace trivial values with NA
#' Trivial values include: "" (blank), only having 0's in codes
medical_long$icd9_raw <- gsub("^$|-|0{4,5}|^0{3}$", NA, medical_long$icd9_raw)
medical_long <- medical_long[is.na(medical_long$icd9_raw) == FALSE, ]


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
          ifelse(nchar(icd9_raw) < 5, "", substr(icd9_raw, 1, 5)),
          ifelse(nchar(icd9_raw) < 4, "", substr(icd9_raw, 1, 4)))) 
medical_long$icd9_5digits <- with(medical_long,
   ifelse(substr(icd9_raw, 1, 1)=="E",
          "",
          ifelse(nchar(icd9_raw) < 5, "", substr(icd9_raw, 1, 5)))) 

medical_long <- medical_long[order(medical_long$icd9_3digits,
                             medical_long$icd9_4digits,
                             medical_long$icd9_5digits),]


# count the number of roots at each level
medical_long[, count_3digits := ifelse(icd9_3digits=="", 0L, .N), by = as.character(icd9_3digits)]
medical_long[, count_4digits := ifelse(icd9_4digits=="", 0L, .N), by = icd9_4digits]
medical_long[, count_5digits := ifelse(icd9_5digits=="", 0L, .N), by = icd9_5digits]

medical_long.counts <- unique(medical_long)

## find the number of roots at each level
sink("output_icd9PatientSummary.txt", append = FALSE, split = FALSE)
print("The number of 3-digit ICD-9 codes is "); length(unique(medical_long.counts$icd9_3digits)) #1190
print("The number of 4-digit ICD-9 codes is "); length(unique(medical_long.counts$icd9_4digits)) #5836
print("The number of 5-digit ICD-9 codes is "); length(unique(medical_long.counts$icd9_5digits)) #6106
sink(NULL)


## histogram of icd-9 code counts at each level
plot.data_3digits <- unique(
  medical_long.counts[medical_long.counts$count_3digits>0,
                      .(icd9_3digits, count_3digits)])
plot.data_3digits$icd9_level <- "icd9_3digits"
colnames(plot.data_3digits) <- c("icd9_code", "count", "icd9_level")

plot.data_4digits <- unique(
  medical_long.counts[medical_long.counts$count_4digits>0,
                      .(icd9_4digits, count_4digits)])
plot.data_4digits$icd9_level <- "icd9_4digits"
colnames(plot.data_4digits) <- c("icd9_code", "count", "icd9_level")

plot.data_5digits <- unique(
  medical_long.counts[medical_long.counts$count_5digits>0,
                      .(icd9_5digits, count_5digits)])
plot.data_5digits$icd9_level <- "icd9_5digits"
colnames(plot.data_5digits) <- c("icd9_code", "count", "icd9_level")

plot.data <- rbind(plot.data_3digits, plot.data_4digits, plot.data_5digits)


ggplot(plot.data, aes(x=count)) + 
  geom_histogram(binwidth=15, colour="salmon", fill="white") + 
  facet_grid(icd9_level ~ .) +
  scale_x_continuous(limits = c(0, 2000)) +
  scale_y_continuous(limits = c(0, 800)) +
  labs(title="Frequency of counts of ICD-9 codes at each level, with <= 2000 counts",
       x="Counts of ICD-9 codes",
       y="Frequency") 
ggsave("histogram_countICD9.pdf",
       width = 8, height = 10)



# output the frequency table of ICD-9 codes
write.csv(medical_long.counts, "icd9_frequency_table.csv", 
          quote = FALSE, row.names=FALSE, na="")

sink("output_icd9PatientSummary.txt", append = TRUE, split = FALSE)

print("frequency of ICD-9 codes with 3 digits")
summary(medical_long.counts$count_3digits[medical_long.counts$icd9_3digits != ""])
print("10 most frequent 3-digit ICD-9 codes")
sort(table(medical_long$icd9_3digits[medical_long$icd9_3digits != ""]),
     decreasing = TRUE)[1:10]

print("frequency of ICD-9 codes with 4 digits")
summary(medical_long.counts$count_4digits[medical_long.counts$icd9_4digits != ""])
print("10 most frequent 4-digit ICD-9 codes")
sort(table(medical_long$icd9_4digits[medical_long$icd9_4digits != ""]),
     decreasing = TRUE)[1:10]

print("frequency of ICD-9 codes with 5 digits")
summary(medical_long.counts$count_5digits[medical_long.counts$icd9_5digits != ""])
print("10 most frequent 5-digit ICD-9 codes")
sort(table(medical_long$icd9_5digits[medical_long$icd9_5digits != ""]),
     decreasing = TRUE)[1:10]

sink(NULL)

