##################################################
#' Program name: data_icd9Summary.R
#' 
#' Description: The program summarizes patient-level
#' ICD-9 codes after index VTE date.
#' 
#' Author: Mengbing Li
#' 
#' Created: 10/08/2018
#' 
#' Revisions:
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20180830/data_icd9Summary")

library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(ggplot2)
library(grid)

patinfo <- fread("../combineData.csv",
                 select = c("patid", "source", "fst_dt", "diag", "icd9_raw", 
                            "icd9_3digits", "icd9_4digits", "icd9_5digits", "index_dt"),
                 colClasses = list(character=1:9))

# test <- patinfo[1:10000, ]
patinfo <- unique(patinfo)
patinfo$fst_dt <- as.Date(patinfo$fst_dt)
patinfo$index_dt <- as.Date(patinfo$index_dt)
# keep records after index VTE date, and before start date of ICD-10
patinfo <- patinfo[fst_dt >= index_dt & fst_dt < as.Date("2015-10-01"), ]

## summary of occurrences of icd9 codes over all patients
patinfo[, count_3digits := .N, by = icd9_3digits]
patinfo[, count_4digits := .N, by = icd9_4digits]
patinfo[, count_5digits := .N, by = icd9_5digits]

patinfo_icd9only <- unique(patinfo[, -c("patid", "fst_dt", "diag", "icd9_raw")])
patinfo_icd9only <- patinfo_icd9only[order(icd9_3digits, icd9_4digits, icd9_5digits),][
  icd9_3digits != "",
]

write.csv(patinfo_icd9only, "icd9_frequency_table.csv", 
          quote = FALSE, row.names=FALSE, na="")

sink("output_icd9PatientSummary.txt", append = FALSE, split = FALSE)

print("frequency of ICD-9 codes with 3 digits")
summary(patinfo_icd9only$count_3digits[patinfo_icd9only$icd9_3digits != ""])
print("10 most frequent 3-digit ICD-9 codes")
sort(table(patinfo$icd9_3digits[patinfo$icd9_3digits != ""]),
     decreasing = TRUE)[1:10]

print("frequency of ICD-9 codes with 4 digits")
summary(patinfo_icd9only$count_4digits[patinfo_icd9only$icd9_4digits != ""])
print("10 most frequent 4-digit ICD-9 codes")
sort(table(patinfo$icd9_4digits[patinfo$icd9_4digits != ""]),
     decreasing = TRUE)[1:10]

print("frequency of ICD-9 codes with 5 digits")
summary(patinfo_icd9only$count_5digits[patinfo_icd9only$icd9_5digits != ""])
print("10 most frequent 5-digit ICD-9 codes")
sort(table(patinfo$icd9_5digits[patinfo$icd9_5digits != ""]),
     decreasing = TRUE)[1:10]

sink(NULL)


## histogram of icd-9 code counts at each level
plot.data_3digits <- unique(patinfo_icd9only[patinfo_icd9only$icd9_3digits != "", "icd9_3digits", "count_3digits"])
colnames(plot.data_3digits) <- c("count", "icd9_level")
plot.data_4digits <- unique(patinfo_icd9only[patinfo_icd9only$icd9_4digits != "", "icd9_4digits", "count_4digits"])
colnames(plot.data_4digits) <- c("count", "icd9_level")
plot.data_5digits <- unique(patinfo_icd9only[patinfo_icd9only$icd9_5digits != "", "icd9_5digits", "count_5digits"])
colnames(plot.data_5digits) <- c("count", "icd9_level")
plot.data <- rbind(plot.data_3digits, plot.data_4digits, plot.data_5digits)


ggplot(plot.data, aes(x=count)) + 
  geom_histogram(binwidth=15, colour="salmon", fill="white") + 
  facet_grid(icd9_level ~ .) +
  scale_x_continuous(limits = c(0, 2000)) +
  labs(title="Histograms of counts of ICD-9 codes at each level, with <= 2000 counts",
       x="Counts of ICD-9 codes",
       y="Frequency") 
# ggsave("histogram_countICD9.pdf")




## look at recurrent diagnoses of a particular patient
patient1 <- patinfo[patid=="802666500107193" & icd9_3digits != "",]
patient1 <- unique(patient1[, .(patid, source, fst_dt, index_dt, icd9_3digits)])

# store the cumulative occurrences of icd-9 codes with 3 digits
icd9.3 <- patient1[, .(source, fst_dt, index_dt, icd9_3digits)]
icd9.3 <- icd9.3[order(icd9_3digits, fst_dt),][,
                 `:=`(ones = 1, days = as.numeric(fst_dt - index_dt))]
icd9.3$in_out <- with(icd9.3, ifelse(source == "medical.outpatient",
                                     "outpatient", "inpatient"))
icd9.3[, icd9_cum_counts := cumsum(ones), by = c("in_out", "icd9_3digits")]
icd9.3[, total_counts := max(icd9_cum_counts), by = icd9_3digits]


ggplot(data = icd9.3, aes(x = days, y = icd9_cum_counts,
                          color = icd9_3digits)) +
  geom_step() +
  geom_point() +
  geom_jitter(width = 0.8, height = 0.5) +
  facet_grid(in_out ~ .) +
  xlab("Days after index VTE date") +
  ylab("Cumulative counts of diagnoses") + 
  ggtitle("Cumulative counts of diagnoses after index VTE date. Patient 802666500107193
          \n All diagnoses") 
# +
  # scale_x_continuous(limits = c(0, max(icd9.3$days[icd9.3$total_counts>1])))
# ggsave("recurrentICD9_example_allDiagnoses.pdf")


ggplot(data = icd9.3[total_counts > 1, ], aes(x = days, y = icd9_cum_counts,
                          color = icd9_3digits, label = icd9_3digits)) +
  geom_step() +
  geom_point() +
  geom_jitter(width = 0.8, height = 0.5) +
  facet_grid(in_out ~ .) +
  geom_text(check_overlap = T, vjust = 0, size = 3.5, nudge_x = 0.25, nudge_y = 0.25) +
  xlab("Days after index VTE date") +
  ylab("Cumulative counts of diagnoses") + 
  ggtitle("Cumulative counts of diagnoses after index VTE date. Patient 802666500107193
          \n Diagnoses that occurred only once are omitted") 
# ggsave("recurrentICD9_example_recurrentDiagnoses.pdf")
ggsave("Rplot.pdf")


example_data <- icd9.3[, c("in_out", "fst_dt", "icd9_3digits", "days",
                       "icd9_cum_counts", "index_dt")]
colnames(example_data)[which(colnames(example_data) == "days")] <- "days_after_vte"
example_data <- example_data[order(in_out, icd9_3digits),]
write.csv(example_data, "data_802666500107193.csv",
          quote = FALSE, row.names=FALSE, na="")

