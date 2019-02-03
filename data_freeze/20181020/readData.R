# Codes for viewing the first 1000 rows of data sets quickly

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020")

library(data.table)

baseline <- fread("baseline_20181020_freeze.csv", nrows = 1000)

diagnosis <- fread("diagData_20181020_freeze.csv",
                   colClasses = list(character=1:73),
                   nrows = 1000)

# get confinement claims
confinement <- diagnosis[source == "confinement.inpatient",]


procedure <- fread("procData_20181020_freeze.csv",
                   colClasses = list(character=1:112),
                   nrows = 1000)


# check whether any patient has proc_cd from both medical and facility data
for(j in unique(procedure$patid)){
  subdata <- procedure[patid == j, ]
  if ("facility.inpatient" %in% subdata$source & 
      "medical.inpatient" %in% subdata$source) 
    break 
}
