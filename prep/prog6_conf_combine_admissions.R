#######################################################################################
#### THIS PROGRAM USES CONFINEMENT DATA FOR ELIGIBLE PATIENTS AND COMBINES         ####
#### CONTINUOUS ADMISSION DATA OF MULTIPLE ROWS INTO ONE ROW. CONTINUOUS ADMISSION ####
#### IS DEFINED AS THE DISCHARGE DATE IS THE SAME AS THE NEXT ADMISSION DATE       ####
#######################################################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/prep")

# sink(file("../prep/prog6_conf_combine_admissions.log", open="wt"), type = "message")

library(sas7bdat)
library(dplyr)
library(tidyr)
library(data.table)


# READ IN CONFINEMENT DATE SET
confinfo <- read_excel("../data/prog2_conf.xlsx")

# COMBINE CONTINUOUS HOSPITALIZATION PERIODS: MERGE INTO ONE ROW IF
# THE NEXT ADMIT_DT IS THE SAME AS THE PREVIOUS DISCH_DT
confinfo2 <- confinfo %>%
  group_by(patid) %>%
  mutate(disch_dt_combined = disch_dt,
         next_admit_dt = lead(admit_dt),
         flag = 0)

# IDENTIFY CONTINUOUS PERIOD AND FLAG THE ROW
for(i in 1:nrow(confinfo2)) {
  if ( !is.na(confinfo2$next_admit_dt[i]) ) {
    if ( confinfo2$admit_dt[i+1]==confinfo2$disch_dt[i] ){
      confinfo2$disch_dt_combined[i] <- confinfo2$disch_dt[i+1]
      confinfo2$flag[i+1] <- 1
    }
  }
}

# REMOVE REDUNDANT ROWS
confinfo3 <- confinfo2 %>%
  filter(flag == 0) %>%
  select(-c(next_admit_dt, flag))
confinfo3 <- confinfo3[,c(1,9,2,10,4:8)]

saveRDS(confinfo3,"../data/prog6_conf_combine_admissions.rds")

print(paste("The number of patients who have a confinement record is ", nrow(confinfo3))) # 59741
print(paste("The number of rows is ", nlevels(as.factor(confinfo3$patid)))) # 13288

# sink()

