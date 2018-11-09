##################################################
#' Program name: data_insuranceGaps.R
#' 
#' Description: This program examines the interval gaps
#' between insurance coverage periods, from 2007-01-01
#' to 2016-12-31.
#' 
#' Author: Mengbing Li
#' 
#' Created: 09/20/2018
#' 
#' Revisions: 11/08/2018 - use 20181020 data
##################################################

library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(ggplot2)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_insuranceGaps")

# read in patient data
patinfo <- fread("../separateData/patient_data.txt",
                 select = c("patid", "index_dt"), 
                 colClasses = list(character = 1:2))

patinfo <- unique(patinfo)
patinfo$index_dt <- as.Date(patinfo$index_dt)

# read in insurance coverage information from member_details
member <- fread("../separateData/member.txt", 
                select = c("Patid", "Eligeff", "Eligend", "ENROLL_LENGTH"),
                colClasses=list(character=1, Date=2:3, integer=4))
colnames(member) <- tolower(colnames(member))
# keep patients in the patinfo data
member <- member[patid %in% patinfo$patid, ]
member$eligeff <- as.Date(member$eligeff)
member$eligend <- as.Date(member$eligend)
member <- member[order(patid, eligeff, eligend),][
  eligend >= as.Date("2008-01-01"), # because we defined continuous
  # enrollment starting in 2008
]

# add index VTE date to member
member <- merge(x = member,
                y = patinfo,
                by = "patid",
                all.x = TRUE)

# since all enrollments are combined, the number of rows in member data
# equals the number of gaps between insurance coverage

# count the number of lapses at any time in each individual
member[, n_gaps := .N, by = patid]
member[, n_gaps := n_gaps - 1]

# count the number of lapses after index VTE in each individual
member[, gap_seq := rowid(patid)]
member[, gapAfterVTE := (gap_seq > 1 & eligeff > index_dt)]
member[, n_gapsAfterVTE := sum(gapAfterVTE), by = patid]


### 1.  The number of lapses in each individual -----------------------------

# lapses at any time
n_gaps_individual <- unique(member[, .(patid, n_gaps)])

sink("output_insuranceGaps.txt", append = FALSE)
print("The number of enrollment gaps at any time:")
table(n_gaps_individual$n_gaps)
sink(NULL)

ggplot(data = n_gaps_individual, aes(x = n_gaps, fill = "salmon")) +
  geom_histogram(alpha=0.3, position="identity", binwidth = 1) +
  labs(x = "Number of insurance coverage gaps",
       y = "Frequency",
       title = "Histogram of number of insurance coverage gaps
       at any time") +
  theme(legend.position="none")
ggsave("histogram_nCoverageGaps.pdf")


# lapses after index VTE
n_gapsAfterVTE_individual <- unique(member[, .(patid, n_gapsAfterVTE)])

sink("output_insuranceGaps.txt", append = TRUE)
print("The number of enrollment gaps after index VTE:")
table(n_gapsAfterVTE_individual$n_gapsAfterVTE)
sink(NULL)

ggplot(data = n_gapsAfterVTE_individual, 
       aes(x = n_gapsAfterVTE, fill = "salmon")) +
  geom_histogram(alpha=0.3, position="identity", binwidth = 1) +
  labs(x = "Number of insurance coverage gaps",
       y = "Frequency",
       title = "Histogram of number of insurance coverage gaps
       after index VTE") +
  theme(legend.position="none")
ggsave("histogram_nCoverageGapsAfterVTE.pdf")





### 2. Lengths of lapses ----------------------------------------------------

# calculate lengths of the lapses
member_withGaps <- member[n_gaps >= 1, ]
member_withGaps[, 
  gapLength := as.numeric(eligeff-lag(eligend))/30, #gapLength in month
  by = patid]

ggplot(data = member_withGaps[!is.na(gapLength),],
       aes(x = gapLength, fill = "salmon")) +
  geom_histogram(alpha=0.3, position="identity", binwidth = 1) +
  facet_grid(gapAfterVTE ~ ., labeller = label_both) +
  labs(x = "Months",
       y = "Frequency",
       title = "Histogram of length of insurance coverage gaps") +
  theme(legend.position="none")
ggsave("histogram_lengthCoverageGaps.pdf")


modes <- function(x, na.remove = TRUE) {
  x <- na.omit(x)
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
gaps <- na.omit(member_withGaps$gap_length)

sink("output_insuranceGaps.txt", append = TRUE)
print("Summary of gaps:"); summary(gaps)
print("Mode of gaps:"); modes(gaps)
sink(NULL)
