########################################################################
#' Program name: subset_3_removeNoACPeriod_multipleCancers.R
#' 
#' Description: The program completes the remaining inclusion/exclusion
#' criteria: exclude patients who do not have an outpatient 
#' anticoagulant within 30 days after index VTE date.
#' The program also merges multiple index cancer types into single 
#' categories.
#' 
#' Author: Mengbing Li
#' 
#' Created: Wednesday 04/11/2018
#' 
#' Revisions: 09/11/2018 - revised header information
########################################################################


#!/usr/bin/Rscript

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/preprocessing/subsetting")


options(java.parameters = "-Xmx8000m")

library(sas7bdat)
library(dplyr)
library(tidyr)
library(xlsx)
library(readxl)


### READ IN COMPLETED SUBSET DATA SET
patinfo <- read_excel("../data/subset_2_inclusionExclusion_withDuplicates.xlsx", sheet = "subset_2_withDuplicates")
patinfo[,c("fill_dt","index_dt","index_cancer_dt")] <- 
  lapply(patinfo[,c("fill_dt","index_dt","index_cancer_dt")], as.Date)


### 1. EXCLUDE PATIENTS WHO DO NOT GET AN OUTPATIENT AC WITHIN 30 DAYS --------

checkAC <- function(end_dt){
  # KEEP ROWS WITH AC WITHIN end_dt DAYS AFTER INDEX VTE
  patid_end <- patinfo %>%
    filter(fill_dt <= index_dt+end_dt & fill_dt >= index_dt) %>%
    select(patid) %>%
    distinct(patid)
  
  # KEEP ONLY PATIENTS WHO GOT AC WITHIN end_dt DAYS AFTER INDEX VTE
  patinfo2 <- patinfo %>%
    filter(patid %in% patid_end$patid) %>%
    mutate(patid = factor(patid))
  
  return(patinfo2)
}


patinfo2 <- checkAC(end_dt = 30)
print(paste("The number of eligible patients who got an AC within 30 days after index VTE date is",
      nlevels(as.factor(patinfo2$patid)) ) ) # 14945
print(paste("The number of rows in the data set is", nrow(patinfo2))) # 171050








### 2. CONVERT MULIPLE CANCERS INTO SINGLE CATEGORIES -----------------------

#' stomach + any other cancer --> stomach,                                       
#' pancreas + any other cancer --> pancreas,                                     
#' stomach + pancreas --> pancreas,                              
#' lung/lymphoma/gynecologic/bladder/testicular cancer + any other non-stomach   
#'   or pancreas cancer --> the named cancer (either lung, lymphoma, gynecologic,
#'   bladder, testicular (essentially ignore the other non stomach/pancreas      
#'   cancer),                                                                    
#' any combination not listed above --> multiple cancers.    

# CONVERT WIDE FORMAT WITH MULTILE CANCERS INTO LONG FORMAT WITH ONE CANCER PER ROW
patinfo_long <- patinfo2 %>%
  mutate(cancer_type = strsplit(as.character(cancer_type), ", ")) %>% 
  unnest(cancer_type) %>%
  select(patid, cancer_type) %>% 
  distinct() %>%
  arrange(patid)

# EXTRACT SINGLE CANCERS
patinfo_singlecancer <- patinfo_long %>%
  group_by(patid) %>%
  filter(n()==1) %>%
  mutate(cancer_type_combined = cancer_type)%>%
  select(-cancer_type)

# ONLY CONSIDER MULTIPLE CANCERS
patinfo_multicancer <- patinfo_long %>%
  group_by(patid) %>%
  filter(n()>1)

# SCORE EACH CANCER TYPE
patinfo_multicancer <- patinfo_multicancer %>%
  mutate(score = case_when(
    cancer_type == "Pancreas" ~ 20,
    cancer_type == "Stomach" ~ 10,
    cancer_type %in% c("Lung", "Lymphoma", "Gynecologic","Bladder", "Testicular") ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(patid) %>%
  mutate(score_sum = sum(score))

# IMPLEMENT THE MULTIPLE CANCER ALGORITHM
patinfo_cancers <- patinfo_multicancer %>%
  mutate(cancer_type_combined = case_when(
    score_sum >= 20 ~ "Pancreas",
    (score_sum >=10) & (score_sum <20) ~ "Stomach",
    (score_sum == 1) & (score == 0) ~ "",
    (score_sum == 1) & (score == 1) ~ cancer_type,
    TRUE ~ "Multiple Cancers"
  )) 

# KEEP GOOD ROWS
patinfo_multicancer2 <- patinfo_cancers %>%
  filter(cancer_type_combined != "") %>% 
  distinct(patid, cancer_type_combined) 
cancer_info <- rbind(patinfo_singlecancer,patinfo_multicancer2)
patinfo3 <- merge(x=patinfo2, y=cancer_info, by="patid", all.x = TRUE)
names(patinfo3) <- tolower(names(patinfo3))

patinfo3 <- patinfo3[,c(1:8,ncol(patinfo3),9:(ncol(patinfo3)-1))]
patinfo3$patid <- as.character(patinfo3$patid)
patinfo3 <- patinfo3 %>%
  mutate(fill_dt = as.Date(fill_dt, origin="1960-01-01"),
         index_dt = as.Date(index_dt, origin="1960-01-01"),
         index_cancer_dt = as.Date(index_cancer_dt, origin="1960-01-01"),
         cancer_type = as.character(cancer_type))








# WRITE TABLE OUTPUT
saveRDS(patinfo3, "../data/subset_3_removeNoACPeriod_multipleCancers.rds")

wb <- loadWorkbook("../data/subset_3_removeNoACPeriod_multipleCancers.xlsx")

removeSheet(wb, sheetName="subset_3_removeNoACPeriod") # replace with new data frame
sheet <- createSheet(wb, sheetName="subset_3_removeNoACPeriod")
addDataFrame(patinfo3, sheet, row.names = FALSE)
saveWorkbook(wb, "../data/subset_3_removeNoACPeriod_multipleCancers.xlsx")



