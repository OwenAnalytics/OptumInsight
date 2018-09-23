##################################################
#' Program name: data_gapsInAC.R
#' 
#' Description: This program checks whether the gaps between
#' anticoagulant trajectories are due to cease of VTE or
#' lack of insurance coverage
#' 
#' Author: Mengbing Li
#' 
#' Created: 09/13/2018
#' 
#' Revisions:
##################################################

library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(ggplot2)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20180830/data_gapsInAC")

## read in patient data
patinfo <- readRDS("../patient_data.rds")

patinfo2 <- patinfo[, .(patid, clmid, index_dt, fill_dt, category, days_sup)]

pdf("histogram_daysOfSupply.pdf")
hist(patinfo2$days_sup, main = "Histogram of days of supply",
     xlab = "Days of supply")
dev.off()

sink("output.txt", append = FALSE)
print("Summary of days of supply")
summary(patinfo2$days_sup)
sink(NULL)

#' Logic: 
#' 1. Remove AC prescription periods covered entirely in another AC
#' period: fill date >= previous fill date & end_dt <= previous end_dt. 
#' 2. Check gaps due to lack of insurance coverage:
#'  fill_dt <= eligend & lead(fill_dt) >= lead(eligeff)
#' 3. Check gaps due to hospitalization
#'  fill_dt <=disch_dtligend & lead(fill_dt) >= lead(admit_dt)


## 1. Find covered AC periods --------------------------------------------

#' The entire AC period is covered by the previous AC period, which.
#' in particular means fill date >= previous fill date & 
#' end_dt <= previous end_dt. 
#' 1) First order the data by ascendingly patid, fill_dt, and end_dt. 
#' 2) Then remove records satisfying the constraint above. 

# identify AC periods
patinfo2[, acEndDt := fill_dt + days_sup]

## Step 2: determine continuity of AC trajectory -------------------
patinfo2.ordered <- patinfo2[order(patid, fill_dt, -acEndDt),]


## check whether each AC period is covered entirely by any other AC periods

# subset patients who had only one fill. Such AC periods are not covered
patinfo3 <- patinfo2.ordered[!(duplicated(patinfo2.ordered$patid)|duplicated(patinfo2.ordered$patid, fromLast=TRUE)),]
patinfo3[, covered:=FALSE]

# check patients with more than one fill. covered = FALSE if the AC
# period is not covered
for(pat in unique(patinfo2.ordered[!patid %in% patinfo3$patid,patid])){
  tmp <- patinfo2.ordered[patid==pat,][,covered:=FALSE]
  n <- nrow(tmp)
  for(j in n:2){
    for(k in j-1:1){
      test <- (tmp$fill_dt[j]>=tmp$fill_dt[k] & tmp$acEndDt[j]<=tmp$acEndDt[k])
      if (test) {break}
    }
    tmp$covered[j] <- test
  }
  patinfo3 <- rbind(patinfo3, tmp)
}


# identify which AC periods are covering smaller periods
patinfo3 <- patinfo3[order(patid, fill_dt, -acEndDt)]
patinfo3$coveringAC <- with(patinfo3, 
      ifelse(covered==TRUE, lead(category), ""))

# identify covered AC periods
acCovered <- patinfo3[covered==TRUE,]

sink("output_gapsInAC.txt", append = TRUE)
cat("\n")
print("The number of covered AC periods is")
sum(table(acCovered$category, acCovered$coveringAC)) 

cat("\n")
print("Table of covered AC periods (rows) and covering AC periods (columns)")
table(acCovered$category, acCovered$coveringAC)
sink(NULL)

ggplot(acCovered, aes(x=days_sup)) +
  geom_histogram(binwidth=5, colour="salmon", fill="white") + 
  facet_grid(category ~ .) +
  labs(title="Histograms of days of supply of covered anticoagulant periods",
       x="Days of supply",
       y="Frequency") 
ggsave("histogram_coveredAC_dayOfSupply.pdf")

#remove covered AC periods
patinfo.noCoveredAC <- patinfo3[patinfo3$covered==FALSE,]
patinfo.noCoveredAC$coveringAC <- NULL



### 2. Identify gaps in AC periods due to lack of insurance coverage -------

#' We care about the cease of an AC trajectory of two types:
#' (1) There is a gap greater than n days (say 30) between 
#' two AC trajectories. Prescriptions of at least two ACs is 
#' necessary for such a gap.
#' (2) The end of the most recent AC trajectory. Check whether 
#' the insurance end date is very close to the end of the trajectory.

# get patients having at least two rows
patinfo_ge2AC <- patinfo.noCoveredAC[patinfo.noCoveredAC[, .I[.N > 1], by = patid]$V1,]
patinfo_ge2AC <- patinfo_ge2AC[order(patid, fill_dt)]
gapLength <- 30
patinfo_ge2AC[, gap := lead(fill_dt) - acEndDt > gapLength, by = patid]
patinfo_ge2AC[, `:=`(acDtBeforeGap = if_else(gap==TRUE, fill_dt, as.Date("1970-01-01")),
                     acDtAfterGap = if_else(gap==TRUE, lead(fill_dt), as.Date("1970-01-01")))]
patinfo_haveGap <- patinfo_ge2AC[gap==TRUE, ]

# read in insurance coverage information from member_details
member <- fread("../member_details.txt", 
                select = c("patid", "Pat_PlanId", "Eligeff", "Eligend"),
                colClasses=list(character=1:2, Date=3:4))
colnames(member) <- tolower(colnames(member))
member$eligeff <- as.Date(member$eligeff)
member$eligend <- as.Date(member$eligend)
member <- member[order(patid, eligeff, eligend),]


gaps_elig <- merge(x = patinfo_haveGap, y = member, by = "patid")






