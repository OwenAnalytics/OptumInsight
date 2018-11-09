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
ggsave("histogram_nGaps.pdf")


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
ggsave("histogram_nGapsAfterVTE.pdf")





### 2. Lengths of lapses ----------------------------------------------------

# calculate lengths of the lapses
member_withGaps <- member[n_gaps >= 1, ]
member_withGaps[, 
  gapLength := as.numeric(eligeff-lag(eligend))/30, #gapLength in month
  by = patid]

member_gapsHappen <- member_withGaps[!is.na(gapLength),]

ggplot(data = member_gapsHappen,
       aes(x = gapLength, fill = "salmon")) +
  geom_histogram(alpha=0.3, position="identity", binwidth = 1) +
  facet_grid(gapAfterVTE ~ ., labeller = label_both) +
  labs(x = "Months",
       y = "Frequency",
       title = "Histogram of length of insurance coverage gaps") +
  theme(legend.position="none")
ggsave("histogram_lengthGaps.pdf")


modes <- function(x, na.remove = TRUE) {
  x <- na.omit(x)
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

sink("output_insuranceGaps.txt", append = TRUE)
print("Summary of gap lengths in months:")
member_gapsHappen %>%
  group_by(gapAfterVTE) %>%
  summarise("Min" = min(gapLength),
            "1st Qu" = quantile(gapLength, 0.25),
            "Median" = median(gapLength),
            "3rd Qu." = quantile(gapLength, 0.75),
            "Max" = max(gapLength),
            "Mean" = mean(gapLength),
            "SD" = sd(gapLength)) %>%
  print(n = Inf)
sink(NULL)



### 3. The number of days after index VTE when the first lapse happens ------------

# gaps in months
member_withGaps[, gapsHappenNDaysAfterVTE := as.numeric(eligeff - index_dt) / 30]

# consider gaps after index VTE
gapsAfterVTE <- member_withGaps[gapsHappenNDaysAfterVTE > 0, ]

# only consider the first gap
firstGapAfterVTE <- gapsAfterVTE[
  gapsAfterVTE[ , .I[which.min(gapsHappenNDaysAfterVTE)],
                by = patid]$V1]

ggplot(data = firstGapAfterVTE,
      aes(x = gapsHappenNDaysAfterVTE, 
          y = gapLength,
          fill = "salmon")) +
  geom_point() +
  labs(x = "Months after index VTE when the first gap starts",
      y = "Length of gap in months",
      title = "The number of months after index VTE 
      when the first insurance coverage gap happens VS length of gaps") +
  theme(legend.position="none")
ggsave("histogram_firstGapsAfterVTEvsLength.pdf")

ggplot(data = firstGapAfterVTE,
       aes(x = gapsHappenNDaysAfterVTE, fill = "salmon")) +
  geom_histogram(alpha=0.3, position="identity", binwidth = 1) +
  labs(x = "Months after index VTE when the first gap starts",
       y = "Frequency",
       title = "Histogram of the number of months after index VTE 
       when the first insurance coverage gap happens") +
  theme(legend.position="none")
ggsave("histogram_firstGapsAfterVTE.pdf")


sink("output_insuranceGaps.txt", append = TRUE)
print("Summary of the first gap after index VTE in months:")
firstGapAfterVTE %>%
  summarise("Min" = min(gapsHappenNDaysAfterVTE),
            "1st Qu" = quantile(gapsHappenNDaysAfterVTE, 0.25),
            "Median" = median(gapsHappenNDaysAfterVTE),
            "3rd Qu." = quantile(gapsHappenNDaysAfterVTE, 0.75),
            "Max" = max(gapsHappenNDaysAfterVTE),
            "Mean" = mean(gapsHappenNDaysAfterVTE),
            "SD" = sd(gapsHappenNDaysAfterVTE)) 
sink(NULL)