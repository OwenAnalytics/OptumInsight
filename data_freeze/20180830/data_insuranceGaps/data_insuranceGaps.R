##################################################
#' Program name: data_insuranceGaps.R
#' 
#' Description: This program examines the interval gaps
#' between insurance coverage periods, from 2006-01-01
#' to 2016-12-31.
#' 
#' Author: Mengbing Li
#' 
#' Created: 09/20/2018
#' 
#' Revisions:
##################################################

library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(ggplot2)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20180830/data_insuranceGaps")

# read in patient data
patinfo <- fread("../patient_data.txt",
                 select = "patid", colClasses = "character")
patinfo <- unique(patinfo)

# read in insurance coverage information from member_details
member <- fread("../member.txt", 
                select = c("Patid", "Eligeff", "Eligend", "ENROLL_LENGTH"),
                colClasses=list(character=1, Date=2:3, integer=4))
colnames(member) <- tolower(colnames(member))
# keep patients in the patinfo data
member <- member[patid %in% patinfo$patid, ]
member$eligeff <- as.Date(member$eligeff)
member$eligend <- as.Date(member$eligend)
member <- member[order(patid, eligeff, eligend),][
  eligeff >= as.Date("2006-01-01"),
]

# since all enrollments are combined, the number of rows in member data
# equals the number of gaps between insurance coverage
member[, n_gaps := .N, by = patid]

# examine the gaps
member_gaps <- member[n_gaps > 1, ]
member_gaps[, gap_length := as.numeric(lead(eligeff) - eligend - 1, units="days"), by = patid]

ggplot(data=member_gaps, aes(x=gap_length, fill = "salmon")) +
  geom_histogram(alpha=0.3, position="identity", binwidth = 30) +
  labs(x = "Length of insurance coverage gaps",
       y = "Frequency",
       title = "Histogram of length of insurance coverage gaps") +
  theme(legend.position="none")
ggsave("histogram_coverageGaps.pdf")


modes <- function(x, na.remove = TRUE) {
  x <- na.omit(x)
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
gaps <- na.omit(member_gaps$gap_length)

sink("output_insuranceGaps.txt", append = FALSE)
print("Summary of gaps:"); summary(gaps)
print("Mode of gaps:"); modes(gaps)
sink(NULL)
