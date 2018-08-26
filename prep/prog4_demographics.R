
#######################################################################################
####                             DEMOGRAPHICS                                      ####
#### THIS PROGRAM                                                                  ####
#### 1. CHECKS INCONSISTENCIES IN PATIENT INFORMATION FROM THE MEMBER (member)     ####
#### DATA SET, PARTICULARLY ON GENDER, YEAR OF BORN, RACE, AND DIVISION.           ####
#### 2. KEEPS DEMOGRAPHICS FROM THE ROW WHERE ELIGIBILITY PERIOD (mem_details)     ####
#### COVERS INDEX VTE DATE.                                                        ####
#### 3. ADD THESE VARIABLE TO ANALYSIS DATA SET FOR FUTURE USE.                    ####
#######################################################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/prep")

# sink("prog4_demographics.log")

library(dplyr)
library(tidyr)
library(data.table)


### 1. CHECKS INCONSISTENCIES IN PATIENT INFORMATION FROM MEMBER ------------
patinfo <- readRDS("../data/subset_ac_days_multi_cancers.rds")
member <- fread("../data/member.txt",
                select=c("Patid","index_dt", "Eligeff", "Eligend", "Gdr_Cd",
                         "Yrdob","RACE_CD","Division", "Product"),
                colClasses = c("Patid" = "character", "index_dt" = "Date",
                               "Eligeff" = "Date", "Eligend" = "Date",
                               "Gdr_Cd" = "character", "Yrdob" = "integer",
                               "RACE_CD" = "character", "Division" = "character",
                               "Product" = "character"))
names(member) <- c("patid", "index_dt", "eligeff", "eligend", "gender", "yrdob", "race", "division", "product")


# KEEP INFORMATION FOR ELIGIBLE PATIENTS
member1 <- member %>%
  filter(patid %in% patinfo$patid)
member1$index_dt <- as.Date(strptime(member1$index_dt, "%m/%d/%Y"))
member1$eligeff <- as.Date(member1$eligeff)
member1$eligend <- as.Date(member1$eligend)


# CHECK FOR DUPLICATED ROWS WITH SAME PATID
member_dup <- member1 %>% group_by(patid) %>% filter(n() > 1) # 8776 rows with duplicated patid

# CHECK INCONSISTENCIES IN EACH DEMOGRAPHIC VARIABLE
member_check <- function(x){
  member1 %>% select(patid, x) %>% distinct() %>% group_by(patid) %>% filter(n() > 1) 
}
# CHECK FOR INCONSISTENCY IN INDEX_DT
member_index_dt <- member_check("index_dt") # 0 obs
# CHECK FOR INCONSISTENCY IN GENDER
member_gdr <- member_check("gender") # 0 obs
# CHECK FOR INCONSISTENCY IN YRDOB
member_yrdob <- member_check("yrdob") # 2 obs
# CHECK FOR INCONSISTENCY IN RACE_CD
member_race <- member_check("race") # 0 obs
# CHECK FOR INCONSISTENCY IN DIVISION
member_division <- member_check("division") # 536 obs
member_prod <- member_check("product") # 536 obs

# EXTRACT GENDER, YRDOB, AND RACE FROM MEMBER
demo1 <- member1 %>% 
  select(patid, gender, race) %>%
  distinct()




# 2. KEEPS DIVISION FROM THE ROW WHERE ELIGIBILITY PERIOD (mem_details) --------
member_details <- fread("../data/member_details.txt",
                select=c("Patid", "Eligeff", "Eligend", "Yrdob", "Division", "Product"),
                colClasses = c("Patid" = "character", "Eligeff" = "Date", 
                               "Eligend" = "Date", "Yrdob" = "integer",
                               "Division" = "character", "Product" = "character"))
names(member_details) <- tolower(names(member_details))

# KEEP INFORMATION FOR ELIGIBLE PATIENTS
member_details1 <- member_details %>%
  filter(patid %in% patinfo$patid)
member_details1 <- merge(x=member_details1, y=unique(patinfo[,c("patid","index_dt")]),
                         by="patid", all.x = TRUE)
member_details1$eligeff <- as.Date(member_details1$eligeff)
member_details1$eligend <- as.Date(member_details1$eligend)


# FOR PATIENTS WITH INCONSISTENT INFORMATION ON THESE VARIABLES, USE THE ONE WITH ELIGEFF <= INDEX_DT <= ELIGEND
member_details2 <- member_details1 %>%
  filter(eligeff <= index_dt & eligend >= index_dt) %>%
  distinct(patid, .keep_all=TRUE) %>%
  select(-c(eligeff, eligend, index_dt)) # exactly one row for each patient



# 3. ADD THESE VARIABLE TO ANALYSIS DATA SET FOR FUTURE USE   -------------
demo2 <- merge(x=demo1, y=member_details2, by="patid", all.x = TRUE)



# OUTPUT THE DEMOGRAPHICS TABLE
saveRDS(demo2, file = "../data/prog4_demographics.rds")

# sink()
