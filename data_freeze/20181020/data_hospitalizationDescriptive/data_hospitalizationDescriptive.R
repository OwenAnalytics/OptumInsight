##################################################
#' Program name: data_hospitalizationDescriptive.R
#' 
#' Description: This program summarizes characteristics
#' of hospital admissions, including event times
#' and lengths of stay.
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
library(sqldf)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_hospitalizationDescriptive")



### 1. Length of stay among VTE admissions and non-VTE admissions -----------
# An admission is considered VTE related if one of the diagnoses is a VTE

diagData <- fread("../diagData_20181020_freeze.csv",
                  colClasses = list(character = 1:73))

# obtain inpatient records from medical data
medicalInpatient <- diagData[source == "medical.inpatient",]

# obtain admission dates and lengths of stay
confinement <- diagData[source == "confinement.inpatient",
                        .(patid, index_dt, conf_id, fst_dt, lst_dt, 
                          charge, copay, pos, description, category)]
rm(diagData)

# find out diagnoses on admission
# medicalInpatient_confinement <- merge(x = medicalInpatient,
#       y = confinement,
#       by = c("patid", "index_dt", "conf_id",
#              "fst_dt", "pos", "description", "category"),
#       all.x = TRUE)
# 
# confinement_allDiag <- subset(medicalInpatient_confinement,
#                               is.na(lst_dt.y) == FALSE)


# calculate length of stay
confinement$index_dt <- as.Date(confinement$index_dt)
confinement$fst_dt <- as.Date(confinement$fst_dt)
confinement$lst_dt <- as.Date(confinement$lst_dt)
confinement$los <- as.numeric(confinement$lst_dt - confinement$fst_dt) + 1

confinementAfterVTE <- confinement[fst_dt >= index_dt,]

ggplot(data = confinementAfterVTE[los <= 60,],
       aes(x = los, fill = "salmon")) +
  geom_histogram(alpha=0.3, position="identity", binwidth = 1) +
  labs(x = "Length of stay in days",
       y = "Frequency",
       title = "Histogram of lengths of stay after index VTE
       <= 60") +
  theme(legend.position="none")
ggsave("histogram_losAfterVTE.pdf")




# 2. Summarize hospital admissions by place of service --------------------
sink("output_hospitalizationDescriptive.txt", append = FALSE)
print("The number of hospitalizations after index VTE
      by place of service:")
(nAdmissionsByPOS <- sort(colSums(
  table(confinementAfterVTE$los, confinementAfterVTE$description)),
  decreasing = TRUE))
sink(NULL)

confinement_manyAdmissions <- confinementAfterVTE[
  description %in% names(nAdmissionsByPOS)[1:3],]
ggplot(data = confinement_manyAdmissions[los <= 60,],
       aes(x = los, fill = "salmon")) +
  geom_histogram(alpha=0.3, position="identity", binwidth = 1) +
  facet_grid(description ~., scales="free_y") + 
  labs(x = "Length of stay in days",
       y = "Frequency",
       title = "Histogram of lengths of stay after index VTE in
       top three places of service") +
  theme(legend.position="none")
ggsave("histogram_losAfterVTEByPOS.pdf")




# plot individual hospital admission trajectories
confinementAfterVTE <- confinementAfterVTE[
  order(patid, fst_dt, lst_dt)]
confinementAfterVTE[, seq_admissions := rowid(patid)]
confinementAfterVTE[, 
  admissionDaysAfterVTE := as.numeric(fst_dt - index_dt)]


patids <- unique(confinement$patid)

ggplot(data = confinementAfterVTE[patid %in% patids[5:10]],
       aes(x = admissionDaysAfterVTE,
           y = seq_admissions,
           color = patid)) +
  geom_step() +
  geom_point() +
  geom_jitter(width = 0.05, height = 0.1) +
  xlab("Days after index VTE date") +
  ylab("Cumulative counts of hospital admission") + 
  ggtitle("Cumulative counts of admissions after index VTE date") 
ggsave("histogram_cumCountsAdmissionsAfterVTE.pdf")


