#######################################################################################
#' THIS PROGRAM MANIPUTALTES THE SUBSET DATA SET subset_ac_days_multi_cancers TO CREATE
#'  A WORKING DATA SET FOR USE IN MODELING LATER
#' 1. SUM UP COPAYS FOR THE SAME AC CATEGORY ON THE SAME DATE
#' 2. KEEP ONLY AC PRESCRIBED ON OR AFTER INDEX VTE DATE
#' 3. MERGE ALL COVARIATES INTO ONE FINAL ANALYSIS DATA SET:  
#'   1) DEMOGRAPHICS
#' 4. OBTAIN PRIMARY OUTCOME: THE MOST RECENT ANTICOALULANT FILL BEFORE 90 DAYS AFTER
#'  INDEX VTE DATE
#' 5. ADD INDICATOR FOR HAVING SURGERY WITHIN 28 DAYS PRIOR TO INDEX VTE
#' 6. ADD INDICATOR FOR HOSPITALIZATION WITHIN 28 DAYS PRIOR TO INDEX VTE 
#' 7. ADD INDICATOR FOR SMOKING WITHIN 28 DAYS PRIOR TO INDEX VTE
#' 8. ADD THE MOST RECENT PLATELETS LAB VALUE AT 30 DAYS PRIOR TO INDEX VTE
#' 9. ADD THE MOST RECENT HEMOGLOBIN LAB VALUE AT 30 DAYS PRIOR TO INDEX VTE
#' 10. ADD THE MOST RECENT GFR LAB VALUE AT 30 DAYS PRIOR TO INDEX VTE
#' 11. ADD INDEX VTE TYPE
#' 12. ADD INDICATOR FOR HAVING VTE HISTORY  
#' 13. ADD INDICATOR FOR HAVING ANTIPLATELETS WITHIN 12 MONTHS PRIOR TO INDEX VTE
#' 14. ADD COMORBIDITY WITHIN 12 MONTHS PRIOR TO INDEX VTE, AND CALCULATE CHARLSON
#'   COMORBIDITY SCORE
#' 15. ADD BMI: THE MOST RECENT RECORD BEFORE INDEX VTE DATE
#' 
#' ADD TABLE 2 VARIABLES
#' 1. EDUCATION LEVEL
#' 2. FEDERAL POVERTY STATUS
#' 3. HOME OWNERSHIP
#' 4. HOUSEHOLD INCOME RANGE
#' 5. NETWORTH RANGE
#' 6. OCCUPATION TYPE
#' 7. RACE
#' 8. REGION
#' 9. INSURANCE
#' 10. AVERAGE RX PER PROVIDER (TBD)
#' 11. AVERAGE UNIQUE RX PER PROVIDER (TBD)
#' 12. PROVIDER SPECPECIATY (TBD)
#' 13. COPAY 
#' 14. POS CODES (TBD)
#######################################################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/prep")

# sink("prog100_create_analysis_data.log")

# options(java.parameters = "-Xmx8000m")

library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(xlsx)
library(sas7bdat)

add_wb <- function(sheetnm, data, putrow = FALSE){
  removeSheet(wb, sheetName=sheetnm) # replace with new data frame
  sheet <- createSheet(wb, sheetName=sheetnm)
  addDataFrame(data, sheet, row.names = putrow) 
}


### READ IN COMPLETED SUBSET DATA SET
patinfo <- readRDS("../data/subset_ac_days_multi_cancers.rds")

# GET A LIST OF PATIDS
patids <- unique(patinfo$patid)


### 0. Combine malignancies
cancer_info <- patinfo %>% 
  select(patid, cancer_type) %>%
  distinct()

# split cancers into multiple rows
cancer_info2 <- cancer_info %>%
  mutate(cancer_type_single = strsplit(as.character(cancer_type), ", ")) %>% 
  unnest(cancer_type_single)

# merge cancer categories
cancer_info2 <- cancer_info2 %>% mutate(cancer_type_single2 = case_when(
  cancer_type_single %in% c("Esophageal","Stomach","Colon",
                              "Rectum","Pancreas") ~ "Gastrointestinal",
  cancer_type_single %in% c("Other abdominal", "Heart", "Other thoracic",
                            "Other genitourinary", "Other primary malignancy",
                            "Metastatic Cancer","Unspecified Site", 
                            "Multiple cancers", "Endocrine", "Gynecololgic",
                            "Head & Neck", "Neuroendocrine", "Other Gynecololgic",
                            "Sarcoma/Soft tissue") ~ "Other",
  cancer_type_single %in% c("Gynecololgic", "Ovary") ~ "Gynecololgic",
  cancer_type_single %in% c("Prostate", "Testicular", "Bladder",
                              "Kidney") ~ "Genitourinary",
  cancer_type_single %in% c("Non-Hodgkin Lymphoma", "Hodgkin Lymphoma",
                              "Other Lymphoid Neoplasms and Histiocytic Disorders", "Myeloma",
                              "Leukemia") ~ "Hematologic",
  TRUE ~ cancer_type_single
))


# CONVERT EACH CANCER INTO BINARY INDICATOR
cancers <- data.frame(cancer_info2$cancer_type_single2)
cancer_type_binary <- data.frame(model.matrix(~ . + 0,
        data = cancers,
        contrasts.arg = lapply(cancers, contrasts, contrasts=FALSE)))
cancer_type_binary <- cbind(data.frame(cancer_info2$patid), cancer_type_binary)
colnames(cancer_type_binary) <- gsub("cancer_info2.", "", colnames(cancer_type_binary))
colnames(cancer_type_binary) <- gsub("cancer_type_single2", "malignancy_", colnames(cancer_type_binary))
colnames(cancer_type_binary) <- gsub("\\.\\.\\.", "\\.", colnames(cancer_type_binary))
cancer_type_binary2 <- aggregate(.~patid, cancer_type_binary, sum)
patinfo <- merge(x=patinfo, y=cancer_type_binary2, by="patid")



### 1. SUM UP COPAYS FOR THE SAME AC CATEGORY ON THE SAME DATE  -------------

#' IF TWO ROW AGREE ON PATID, CLMID, AC FILL, FILL DATE, QUANTITY, AND STRENGTH,
#' THEN THEY DIFFER ON PLANID.
#' CALCULATE THE SUM OF COPAYS, AND REMOVE DUPLICATE ROWS
patinfo2 <- patinfo %>%
  group_by(patid, clmid, fill_dt, brand_name, days_sup, quantity, strength) %>%
  mutate(copay_sum = sum(copay)) %>%
  distinct(patid, clmid, fill_dt, brand_name, days_sup, quantity, strength, .keep_all=TRUE) %>%
  ungroup() %>%
  arrange(patid, fill_dt, brand_name)




### 2. KEEP ONLY AC PRESCRIBED ON OR AFTER INDEX VTE DATE -------------------
patinfo3 <- patinfo2 %>% filter(fill_dt >= index_dt) 

print(paste("The number of eligible patients with a fill date after index VTE is ",
            nlevels(as.factor(patinfo3$patid)) ) ) #14945
print(paste("After summing copays on the same AC, the number of rows in the data set is ",
            nrow(patinfo3))) # 134340




### 3. MERGE ALL COVARIATES INTO ONE FINAL ANALYSIS DATA SET  ---------------
# data1 <- readRDS("../data/prog100_data1_copay_sum_aftervte.rds")
data2 <- readRDS("../data/prog4_demographics.rds")
analysis_data1 <- merge(x=patinfo3,y=data2, by="patid", all.x = TRUE)

# CREATE ANTI-COAGULANT CATEGORIES ACCORDING TO TABLE 1
analysis_data2 <- analysis_data1 %>%
  mutate(brand_name = as.character(brand_name), category = as.character(category)) %>%
  mutate(male = as.numeric(gender=="M"),
         gen_name = case_when(
           brand_name %in% c("Fragmin", "Lovenox") ~ "LMWH",
           brand_name == "Warfarin" ~ "Warfarin",
           brand_name == "Eliquis" ~ "Apixaban",
           brand_name == "Pradaxa" ~ "Dabigatran",
           brand_name == "Savaysa" ~ "Edoxaban",
           brand_name == "Xarelto" ~ "Rivaroxaban",
           ((category == "DOACS") && (!brand_name %in% c("Eliquis", "Pradaxa", "Savaysa", "Xarelto"))) ~ "Other DOACS",
           TRUE ~ "Other"
         ),
         age = year(index_dt) - yrdob)  %>%
  select(-gender) 

analysis_data2 <- analysis_data2 %>% 
  mutate(age_cat = case_when(
    18 <= age & age <= 34 ~ "18_34",
    35 <= age & age <= 54 ~ "35_54",
    55 <= age & age <= 64 ~ "55_64",
    65 <= age & age <= 74 ~ "65_74",
    75 <= age & age <= 84 ~ "75_84",
    85 <= age ~ "85_"
  ))






### 4. OBTAIN INDEX AC AND PRIMARY OUTCOME ---------------------------------------

# 4.1 OBTAIN INDEX AC ----------------------------------------------------------

# OBTAIN THE FIRST AC FILL AFTER INDEX VTE DATE
mindate <- data.table(analysis_data2 %>%
  select(patid, fill_dt, gen_name, days_sup) %>%
  group_by(patid) %>%
  filter(fill_dt == min(fill_dt)) %>%
  mutate(index_ac_dt = fill_dt))

mindate2 <- mindate[, category := gen_name]

lmwh_names <- c("Lovenox","Fragmin")
doacs_names <- c("Eliquis", "Pradaxa", "Savaysa", "Xarelto")

# combine multiple AC on the ac3mo_dt
combineMultipleAC1 <- function(data){
  data_out <- unique(data[, .(patid, index_ac_dt)][, index_ac := ""])
  patids <- unique(data_out$patid)
  
  for(j in 1:length(patids)){
    tmp <- data[patid==patids[j]]
    categories <- unique(tmp$category)
    n <- length(categories)
    if(n==1) {
      out <- tmp$category[1]
    } else if (n>2) {
      out <- "Unknown/Multiple"
    } else{
      if (all(c("Warfarin", "LMWH") %in% categories)){
        ind <- which(tmp$category=="LMWH")
        max_days_sup <- max(tmp$days_sup[ind])
        if (max_days_sup <= 10) { 
          out <- "Warfarin"
        } else out <- "Unknown/Multiple"
      } else out <- "Unknown/Multiple"
    }
    data_out$index_ac[data_out$patid==patids[j]] <- out
  }
  return(data_out)
}

indexAC <- combineMultipleAC1(mindate2)



### 4.2 OBTAIN AC3Mo ----------------------------------------------
#' Logic: 
#' 1. merge fill dates and inr dates together. Basically treat INR as
#' an AC with a grace period of 42 days.
#' 2. determine continuity of AC trajectory: continuous if end_dt := 
#' fill date + days_sup + grace period is greater than the next fill date. However,
#' we need to consider the case when the entire AC period is covered
#' by the previous AC period, which in particular means fill date >=
#' previous fill date & end_dt <= previous end_dt. 
#' 1) First order the data by ascendingly patid, fill_dt, and end_dt. 
#' 2) Then remove records satisfying the constraint above. 
#' 3) Next by patid, for all but the last row, determine whether end_dt 
#' >= next fill date. For the last row, determine whether end_dt >=
#' indexVTE+90.
#' 3. determine AC3Mo:
#' 1) for patiens who were continuously on AC, select the last fill 
#' prior to indexVTE+90 as the AC3Mo date, and the AC on that day as
#' AC3Mo. The AC3Mo date is the last fill date.
#' 2) for patients who stopped, find the first row with end_dt < next
#' fill date. The fill_dt in that row is the stopping date. AC3Mo is
#' labeled as "Not captured". The stopping date is the end_dt in that row.


## Step 1: merge AC fills and INR tests
# ADD INR TEST DATES
lab_all <- data.table(fread("../data/prog1_lab_all.txt",
                            colClasses = c("character", "Date", "character", "Date",
                                           "numeric", "numeric", "numeric", "character")))
colnames(lab_all) <- tolower(colnames(lab_all))
lab_all$patid <- as.character(lab_all$patid)
lab_all$index_dt <- as.Date(lab_all$index_dt)
lab_all$lab_dt <- as.Date(lab_all$lab_dt)

# get only inr test
inr_names <- c("*INTR.*NORM.*RATIO*|*INR.*|*I\\.*N\\.*R\\.*|*NORMALIZED.*|*PROTHROMBIN*|*PROTIME*")
inr_info <- lab_all[grep(inr_names, toupper(lab_all$tst_desc), value=FALSE),]
inr_names_not <- c("DILUTE PROTHROMBIN TIME(DPT)", "PROTHROMBIN FRAGMENT 1+2 AG",
                   "PROTHROMBIN FRAGMENT 1+2 MOAB", "PROTHROMBIN G20210A",
                   "VANCOMYCINRANDOM", "PROTHROMBIN ANTIBODIES, IGM", 
                   "PROTHROMBIN ANTIBODIES, IGG", "PROTHROMBIN GENE MUTATION",
                   "PROTHROMBIN GENE MUTATION INTE", "ANTIPROTHROMBIN ANTIBODY, IGM",
                   "ANTIPROTHROMBIN ANTIBODY, IGG", "PROTHROMBIN AB.IGG",
                   "PROTHROMBIN AB.IGM", "GENTAMICINRANDOM", "PROTHROMBIN 20210A MUT.ANAL.F2",
                   "DILUTE PROTIME SCREEN", "SODIUM,URINE,NORMALIZED", 
                   "PROTHROMBIN GENE ANALYSIS", "PROTHROMBIN G20210A GENE MUTAT",
                   "FibroMeter Prothrombin Index", "Protein, Urine, Normalized", 
                   "INR,NON-MEDICATED RANGE", "ANTIPROTHROMBIN AB IGG/IGM",
                   "FibroMeter Prothrombin Index")
inr_info <- inr_info[! trimws(toupper(tst_desc)) %in% toupper(inr_names_not),] 
inr_info <- unique(inr_info, by=c("patid", "lab_dt"))

## ADD INR TEST DATES
inr_info2 <- inr_info %>% mutate(dates = as.Date(lab_dt),
                                 gen_name = "INR",
                                 days_sup = 0,
                                 grace = 42) %>%
  select(patid, index_dt, dates, gen_name, days_sup, grace)

## ADD AC FILLS
dates_ac <- data.table(analysis_data2 %>%
  select(patid, index_dt, fill_dt, gen_name, days_sup)  %>%
  mutate(grace = case_when(
    gen_name == "Warfarin" ~ 60,
    TRUE ~ 30
  )))
colnames(dates_ac)[which(colnames(dates_ac)=="fill_dt")] <- "dates"

# add up days of supply if one AC was prescribed multiple times in one day
# dates_ac2 <- unique(dates_ac[,days_sup := sum(days_sup), by=.(patid, dates, brand_name)])


#' get_outcome obtains the anticoagulant at index_dt + days
#' days of 3 months = 90, days of 4 months = 120
#' outcome_name = ac3mo or ac4mo
get_outcome <- function(days, outcome_name){
  # combine AC fills and INR
  dates_all <- data.table(rbind(dates_ac, inr_info2) %>%
    mutate(end_dt = dates + days_sup + grace,
           index_dt_days = index_dt + days))
  
  ## Step 2: determine continuity of AC trajectory
  dates_all <- dates_all[order(patid, dates, -end_dt),][dates <= index_dt_days]

  # check whether each AC period is covered entirely by any other AC periods

  # subset patients who had only one fill
  patients1 <- dates_all[!(duplicated(dates_all$patid)|duplicated(dates_all$patid, fromLast=TRUE)),]
  patients1[, covered:=FALSE]
  
  # check patients with more than one fill
  for(pat in unique(dates_all[!patid %in% patients1$patid,patid])){
    tmp <- dates_all[patid==pat,][,covered:=FALSE]
    n <- nrow(tmp)
    for(j in n:2){
      for(k in j-1:1){
        test <- (tmp$dates[j]>=tmp$dates[k] & tmp$end_dt[j]<=tmp$end_dt[k])
        if (test) {break}
      }
      tmp$covered[j] <- test
    }
    patients1 <- rbind(patients1, tmp)
  }

  # remove covered AC periods
  dates_all2 <- patients1[patients1$covered==FALSE,]

  # check if end_dt is after the next fill date
  dates_all2 <- dates_all2[order(patid, dates, -end_dt),]
  dates_all2[, remove := end_dt >= lead(dates), by=patid]
  dates_all2[, remove2 := ifelse(is.na(remove), end_dt>=index_dt_days, remove)]

  
  ## Step 3: determine AC3Mo

  # first identify the AC3Mo for patients who did not stop
  nostop0 <- dates_all2[, .SD[all(remove2)], by=patid]
  nostop1 <- patients1[patients1$patid %in% nostop0$patid]
  # the last AC fill (prior to indexVTE+90) is the AC3Mo
  nostop2 <- nostop1[gen_name != "INR"][
    , .SD[dates==max(dates)], by=patid]
  nostop2 <- unique(nostop2[, "outcome_dt" := dates][, .(patid, outcome_dt)])
  nostop3 <- merge(x=dates_ac, y=nostop2, by="patid", all.x=TRUE)
  nostop4 <- nostop3[, .SD[dates==outcome_dt]][,category:=gen_name]

  # combine multiple AC on the outcome_dt
  nostop <- unique(nostop4[, .(patid, outcome_dt)][, outcome := ""])
  patids <- unique(nostop$patid)
  
  for(j in 1:length(patids)){
    tmp <- nostop4[patid==patids[j]]
    categories <- unique(tmp$category)
    n <- length(categories)
    if(n==1) {
      out <- tmp$category[1]
    } else if (n>2) {
      out <- "Unknown/Multiple"
    } else{
      if (all(c("Warfarin", "LMWH") %in% categories)){
        ind <- which(tmp$category=="LMWH")
        max_days_sup <- max(tmp$days_sup[ind])
        if (max_days_sup <= 10) {
          out <- "Warfarin"
        } else out <- "Unknown/Multiple"
      } else out <- "Unknown/Multiple"
    }
    nostop$outcome[nostop$patid==patids[j]] <- out
  }
  
  # second identify stopping date for those who stopped
  # identify patients who stopped
  stopped0 <- dates_all2[, .SD[!all(remove2)], by=patid]
  stopped1 <- stopped0[, .SD[remove2==min(remove2)], by=patid]
  stopped2 <- stopped1[, .SD[end_dt==min(end_dt)], by=patid]
  stopped2[, c("outcome", "outcome_dt") := .("Not captured", end_dt)]
  stopped3 <- unique(stopped2, by=c("patid", "outcome", "outcome_dt"))
  stopped <- stopped3[,.(patid, outcome, outcome_dt)]

  # combine patients who did and did not stop
  ac_days <- rbind(nostop, stopped)
  colnames(ac_days)[2:3] <- paste0(outcome_name, c("_dt", ""))
  
  return(ac_days)
}

ac3mo_result <- get_outcome(90, "ac3mo")
ac4mo_result <- get_outcome(120, "ac4mo")


# add indexAC and aC3Mo to data
index_3mo <- merge(x=indexAC, y=ac3mo_result, by="patid")
index_34mo <- merge(x=index_3mo, y=ac4mo_result, by="patid")

analysis_data3 <- merge(x=analysis_data2, y=index_34mo, by="patid", all.x = TRUE) %>% 
  arrange(patid, fill_dt)


# REORDER THE COLUMNS
malignancy_names <- colnames(analysis_data3)[grep("malignancy_", colnames(analysis_data3))]
analysis_data3 <- analysis_data3[,c("patid", "clmid", "fill_dt", "category",
            "brand_name", "gen_name", "copay", "copay_sum", "index_ac", "index_ac_dt",
            "ac3mo", "ac3mo_dt", "ac4mo", "ac4mo_dt", "index_dt", "index_cancer_dt",
            "cancer_type", "cancer_type_combined",malignancy_names, "days_sup", "quantity",
            "strength","npi", "race", "yrdob", "division", "male", "age", "age_cat", "product")]


rm(analysis_data1, analysis_data2,dates_ac, index_3mo, inr_info, inr_info2,
   lab_all, mindate, mindate2)






### 5. ADD INDICATOR FOR SURGERY --------------------------------------------

# READ IN PROCCD INFORMATION
procinfo <- fread("../data/check_2_proccd.txt", header = TRUE,
                  select = c("patid", "index_dt", "fst_dt", "Proc_Cd", "procedure"),
                  colClasses=c("character","Date","Date","integer","character",
                               "character","character","character"))
procinfo[,c("index_dt", "fst_dt")] <- lapply(procinfo[,c("index_dt", "fst_dt")], as.Date)
colnames(procinfo) <- tolower(colnames(procinfo))

# KEEP ONLY SURGERY INFORMATION
surgeryinfo <- procinfo %>% 
  filter(procedure == "Surgery") %>%
  filter(fst_dt > index_dt - 28) %>%
  filter(patid %in% patids) %>%
  mutate(proc_cd = as.numeric(proc_cd))

analysis_data3 <- analysis_data3 %>% 
  mutate(surgery = patid %in% surgeryinfo$patid)




### 6. CHECK FOR HOSPITALIZATION WITHIN 28 DAYS PRIOR TO INDEX VTE ----------

# OBTAIN HOSPITALIZATION INFORMATION FROM CONFINEMENT DATA 
confinfo <- read_excel("../data/prog2_conf.xlsx")
confinfo$admit_dt <- as.Date(confinfo$admit_dt)
confinfo$disch_dt <- as.Date(confinfo$disch_dt)
confinfo$index_dt <- as.Date(confinfo$index_dt)

hospitalization <- confinfo %>%
  filter(disch_dt < index_dt & disch_dt >= index_dt - 28) %>%
  select(patid) %>%
  distinct()

analysis_data3 <- analysis_data3 %>%
  mutate(hospitalized = patid %in% hospitalization$patid)




### 7. ADD INDICATOR FOR SMOKING WITHIN 28 DAYS PRIOR TO INDEX VTE ----------

# KEEP ONLY SMOKING INFORMATION
smokeinfo <- procinfo %>%
  filter(procedure == "smoking" & fst_dt > index_dt - 28) %>%
  filter(patid %in% patids)

analysis_data3 <- analysis_data3 %>% 
  mutate(smoke = patid %in% smokeinfo$patid)

rm(procinfo, data2, surgeryinfo, smokeinfo)




### 8. ADD THE MOST RECENT HEMOGLOBIN LAB VALUE AT 30 DAYS PRIOR TO INDEX VTE --------
labinfo <- read_excel("../data/prog3_labs.xlsx", sheet = "all_labs")
names(labinfo) <- tolower(names(labinfo))
labinfo[,c("index_dt","lab_dt")] <- lapply(labinfo[,c("index_dt","lab_dt")], as.Date)

# ADD THE WANTED LAB NAMES
hemo_names <- toupper(c("hemoglobin", "HGB", "Hemoglobin (Hgb)", "HEMOGLOBIN-O"))

# GET THE WANTED LABS WITH NONZERO VALUES
hemo_info <- as.data.frame(labinfo %>%
    filter(trimws(toupper(tst_desc), "both") %in% c(hemo_names)) %>%
    filter(rslt_nbr != 0))

# FIND EXTREME VALUES
hemo_extreme <- as.data.frame(hemo_info %>% filter(rslt_nbr <3 | rslt_nbr > 25))

# FIND THE MOST RECENT TEST DATE PRIOR TO INDEX VTE
hemo_info2 <- hemo_info %>%
  group_by(patid) %>%
  filter(lab_dt == max(lab_dt)) %>%
  filter(rslt_nbr == max(rslt_nbr)) %>%
  distinct(rslt_nbr, .keep_all = TRUE) 

# GROUP LAB VALUES INTO RANGES
hemo_info2 <- hemo_info2 %>%
  mutate(hemoglobin = case_when(
    rslt_nbr < 7 ~ "<7 g/dL",
    rslt_nbr >= 7 & rslt_nbr <= 10 ~ "[7, 10] g/dL",
    rslt_nbr > 10 ~ ">10 g/dL"
  )) %>%
  select(patid, hemoglobin)

analysis_data4 <- merge(x=analysis_data3, y=hemo_info2, by="patid", all.x = TRUE)




### 9. ADD THE MOST RECENT PLATELETS LAB VALUE AT 30 DAYS PRIOR TO INDEX VTE --------

# ADD THE WANTED LAB NAMES
pla_names <- toupper(c("PLATELETS", "Platelets", "Platelet CT", "Platelet Count", "Platelet",
                       "Platelet count (sodium citrate", "Platelet count&platelet count",
                       "Platelet count", "automated", "platelet ct", "platelet ct (poly)",
                       "PLT", "PLT COUNT", "CITRATED BLD", "PLT CT", "PLT-O"))

# GET THE WANTED LABS WITH NONZERO VALUES
pla_info <-labinfo %>%
  filter(trimws(toupper(tst_desc), "both") %in% c(pla_names)) %>%
  filter(rslt_nbr != 0)

# CONVERT NUMBERS INTO CONSISTENT UNITS
# DIVIDE VALUES ENDING WITH 000 BY 1000
pla_info <-  as.data.frame(pla_info %>%
  mutate(rslt_nbr2 = ((rslt_nbr %% 1000)!= 0)*rslt_nbr + ((rslt_nbr %% 1000)==0)*round(rslt_nbr/1000)))

# FIND EXTREME VALUES
pla_extreme <- as.data.frame(pla_info %>% filter(rslt_nbr2 < 0 | rslt_nbr2 > 2000))

# FIND THE MOST RECENT TEST DATE PRIOR TO INDEX VTE
pla_info2 <- pla_info %>%
  group_by(patid) %>%
  filter(lab_dt == max(lab_dt)) %>%
  filter(rslt_nbr2 == max(rslt_nbr2)) %>%
  distinct(rslt_nbr2, .keep_all = TRUE) 

# GROUP LAB VALUES INTO RANGES
pla_info2 <- pla_info2 %>%
  mutate(platelet = case_when(
    rslt_nbr < 30 ~ "<30K",
    rslt_nbr >= 30 & rslt_nbr < 50 ~ "[30, 50)K",
    rslt_nbr >= 50 & rslt_nbr <= 100 ~ "[50, 100]K",
    rslt_nbr > 100 ~ ">100K"
  )) %>%
  select(patid, platelet)

analysis_data4 <- merge(x=analysis_data4, y=pla_info2, by="patid", all.x = TRUE)





### 10. ADD THE MOST RECENT GFR LAB VALUE AT 30 DAYS PRIOR TO INDEX VTE -------

# ADD THE WANTED LAB NAMES
gfr_names <- toupper(c("EGFR if AFRICN AM", "EGFR IF NONAFRICN AM", "Glomerular Filtration Rate/1.7", "e-GFR", "e-GFR",
   "African American", "eGFR (calculation) for African", "eGFR (calculation)", "eGFR AFRICAN AMERICAN",
   "eGFR AFRICAN AMER.", "eGFR AA", "eGFR (Afr. Amer.)", "EGFR", "e-GFR", "eGFR Non-AFR. American",
   "eGFR NON-AFRICAN AMER.", "EGFR(BLACK) (POLY)", "EGFRAA", "eGFR-O", "eGLOM FILT RATE AFRICAN AMER",
   "eGLOMERULAR FILTRATION RATE", "GFR", "GFR - AFRICAN AMERICAN", "GFR - NON-AFRICAN AMERICAN",
   "GFR (CKD-EPI) AA", "GFR (CKD-EPI) NON-AA", "GFR (EST) ALL OTHER RACES", "gfr african american",
   "-GFR African American", "GFR Caucasian", "GFR Est. African American", "GFR Estimate",
   "GFR ESTIMATE (CALCULATED)", "GFR ESTIMATED", "GFR Estimated (African American)", 
   "GFR ESTIMATED (CKD-EPI)", "GFR Estimated (Non-African Ame", "GFR MDRD Af Amer", "GFR MDRD Non Af Amer",
   "GFR NON AFRICAN AMERICAN", "GFR NON-AFRIC AMER", "-GFR Non-African American", "GFR NON-AFRICAN AMERICAN",
   "GFR(AFRICAN AMERICAN)", "GFR(EST) AFRICAN AMERICAN", "GFR(OTHERS)", "GFR, Estim African American",
   "GFR/A", "GFR/Black", "GFR/N", "GFR/White", "GFR-AA", "GFR-Af Amer.", "GFR-CA", "GFR-Non Af Amer.",
   "Glom Filt Rate, Estimated", "GLOMERULAR FILT. RATE", "GLOMERULAR FILTRATE ESTIMATE",
   "Glomerular Filtration Rate"))

# GET THE WANTED LABS WITH NONZERO VALUES
gfr_info <- labinfo %>% 
  filter(trimws(toupper(tst_desc), "both") %in% c(gfr_names)) %>%
  filter(rslt_nbr != 0)

# CONVERT NUMBERS INTO CONSISTENT UNITS
# DIVIDE VALUES ENDING WITH 000 BY 1000
gfr_info <-  as.data.frame(gfr_info %>%
  mutate(rslt_nbr2 = ((rslt_nbr %% 1000)!= 0)*rslt_nbr + ((rslt_nbr %% 1000)==0)*round(rslt_nbr/1000)))

# FIND EXTREME VALUES
gfr_extreme <- as.data.frame(gfr_info %>% filter(rslt_nbr2 < 0 | rslt_nbr2 > 200))

# FIND THE MOST RECENT TEST DATE PRIOR TO INDEX VTE
gfr_info2 <- gfr_info %>%
  group_by(patid) %>%
  filter(lab_dt == max(lab_dt)) %>%
  filter(rslt_nbr2 == max(rslt_nbr2)) %>%
  distinct(rslt_nbr2, .keep_all = TRUE) 

# GROUP LAB VALUES INTO RANGES
gfr_info2 <- gfr_info2 %>%
  mutate(gfr = case_when(
    rslt_nbr < 15 ~ "<15",
    rslt_nbr >= 15 & rslt_nbr < 30 ~ "[15, 30)",
    rslt_nbr >= 30 & rslt_nbr <= 60 ~ "[30, 60]",
    rslt_nbr > 60 ~ ">60"
  )) %>%
  select(patid, gfr)

analysis_data4 <- merge(x=analysis_data4, y=gfr_info2, by="patid", all.x = TRUE)



# OUTPUT THE EXTREME LAB TESTS
# wb <- loadWorkbook("../data/prog3_labs.xlsx")

# add_wb(sheetnm = "hemoglobin", data = hemo_info)
# add_wb(sheetnm = "platelets", data = pla_info)
# add_wb(sheetnm = "gfr", data = gfr_info)
# # createSheet(wb,sheetName = "hemoglobin_extremes")
# # createSheet(wb,sheetName = "platelets_extremes")
# # createSheet(wb,sheetName = "gfr_extremes")
# # 
# add_wb(sheetnm = "hemoglobin_extremes", data = hemo_extreme)
# add_wb(sheetnm = "platelets_extremes", data = pla_extreme)
# add_wb(sheetnm = "gfr_extremes", data = gfr_extreme)

# saveWorkbook(wb, "../data/prog3_labs.xlsx")


rm(labinfo,hemo_info,hemo_info2, hemo_extreme,pla_info,pla_info2,pla_extreme,
   gfr_info,gfr_info2,gfr_extreme)






### 11. & 12. ADD INDEX VTE TYPE & HISTORY OF VTE -----------------------------

vte_info <- read_excel("../data/prog8_vte.xlsx", sheet = "prog8_vte")
analysis_data4 <- merge(x=analysis_data4, y=vte_info, by="patid", all.x = TRUE)




### 13. ADD INDICATOR FOR HAVING ANTIPLATELETS WITHIN 12 MONTHS PRIOR --------
antipla_info <- read_excel("../data/prog9_antiplatelets.xlsx", sheet = "prog9_antiplatelets")
names(antipla_info) <- tolower(names(antipla_info))
names(antipla_info)[10] <- "antiplatelet"
antipla_info$fill_dt <- as.Date(antipla_info$fill_dt)

nlevels(as.factor(antipla_info$patid)) # 738


#' CHECK WHETHER THERE ARE PATIENTS WHO GOT MULTIPLE ANTIPLATELETS OF DIFFERENT
#' CATEGORIES ON THE SAME DATE
# antipla_info_check <- as.data.frame(antipla_info %>%
#   select(patid, fill_dt, antiplatelet) %>%
#   distinct(patid, antiplatelet, .keep_all=TRUE) %>% 
#   group_by(patid) %>% filter(n()>1))


# # WRITE THE OUTPUT TO EXISTING EXCEL
# wb <- loadWorkbook("../data/prog9_antiplatelets.xlsx")
# 
# removeSheet(wb, sheetName="prog_9_antiplatelets_multiple") # replace with new data frame
# sheet <- createSheet(wb, sheetName="prog_9_antiplatelets_multiple")
# addDataFrame(antipla_info_check, sheet, row.names = FALSE)
# 
# saveWorkbook(wb, "../data/prog9_antiplatelets.xlsx")


#' OBTAIN ANTIPLATELETS FOR EACH PATIENT
antipla_info2 <- antipla_info %>%
  select(patid, antiplatelet) %>%
  distinct()

# CONVERT INTO BINARY VALUES
bin <- model.matrix(~antiplatelet+0, data=antipla_info2)
bin2 <- as.data.frame(cbind(antipla_info2[,1], bin))
bin3 <- aggregate(. ~ patid, bin2, sum)

analysis_data4 <- merge(x=analysis_data4, y=bin3, by="patid", all.x = TRUE)

rm(analysis_data3,confinfo)





### 14. ADD COMORBIDITY WITHIN 12 MONTHS PRIOR TO INDEX VTE ------------------
comorb_info <- read_excel("../data/prog10_comorbidity.xlsx", sheet = "prog10_comorbidity")
colnames(comorb_info)[2] <- "comorbidity"

# COMBINE MULTIPLE ROWS OF COMORBIDITIES INTO ONE ROW
comorb_info <- comorb_info %>% 
  group_by(patid) %>%
  mutate(comorbidities  = paste(comorbidity, collapse =", "))

# CONVERT EACH COMORBIDITY INTO BINARY INDICATOR
comorb <- as.data.frame(comorb_info$comorbidity)
bin <- data.frame(model.matrix(~ . + 0, data=comorb,
             contrasts.arg = lapply(comorb, contrasts, contrasts=FALSE)))
colnames(bin) <- gsub("X.comorb_info.", "", colnames(bin))
comorb2 <- cbind(data.frame(comorb_info[,1]), bin)
comorb3 <- aggregate(. ~ patid, comorb2, sum)
comorb.patid <- unique(comorb_info[,c("patid", "comorbidities")])
comorb4 <- merge(x=comorb.patid, y=comorb3, by="patid")

# fill in 0s for patients with no comorbidities
analysis_data4_tmp <- data.table(analysis_data4)
patients_no_comorb <- unique(analysis_data4_tmp[! analysis_data4_tmp$patid %in% comorb4$patid, .(patid)])
patients_no_comorb[, "comorbidities" := ""]
patients_no_comorb[, (colnames(comorb4)[-1:-2]) := 0]
comorb4 <- rbind(comorb4, patients_no_comorb)
comorb4 <- data.frame(comorb4[order(patid),])

# calculate Charlson comorbidity score
charlson_comorb_names_1point_raw <- c("Myocardial infarction", "Congestive heart failure",
  "Peripheral vascular disease", "Cerebrovascular disease", "Dementia",
  "Chronic pulmonary disease", "Rheumatic disease", "Peptic ulcer disease",
  "Mild liver disease", "Diabetes without chronic complication")
charlson_comorb_names_2point_raw <- c("Diabetes with chronic complication",
                                  "Hemiplegia or paraplegia", "Renal disease")
charlson_comorb_names_3point_raw <- c("Moderate or severe liver disease")
charlson_comorb_names_6point_raw <- "Aids/HIV"
get_charlson_comorb_cols <- function(x){
  y <- gsub(" |\\/", ".", x)
  z <- paste("comorbidity.", y, sep="")
  # w <- which(charlson_comorb_names_1point %in% colnames(comorb4))
  return(z)
}
charlson_comorb_cols_1point <- get_charlson_comorb_cols(charlson_comorb_names_1point_raw)
charlson_comorb_cols_2point <- get_charlson_comorb_cols(charlson_comorb_names_2point_raw)
charlson_comorb_cols_3point <- get_charlson_comorb_cols(charlson_comorb_names_3point_raw)
charlson_comorb_cols_6point <- get_charlson_comorb_cols(charlson_comorb_names_6point_raw)
comorb4$charlson_comorb_score <- rowSums(comorb4[,charlson_comorb_cols_1point]) +
  rowSums(2*comorb4[, charlson_comorb_cols_2point]) + 
  3*comorb4[, charlson_comorb_cols_3point] +
  6*comorb4[, charlson_comorb_cols_6point]
  

analysis_data5 <- merge(x=analysis_data4, y=comorb4, by="patid", all.x=TRUE)

rm(comorb, comorb.patid, comorb_info, comorb2, comorb3, comorb4,
   data3, data4, analysis_data4, antipla_info, antipla_info2)




###### PART 2. ADD TABLE 2 VARIABLES -------------------------------------------

# analysis_data <- readRDS("../data/prog100_analysis_data.rds")
ses_member <- fread("../data/prog11_ses.txt", header = TRUE,
                    colClasses = c("character","character","character",
                                   "integer", "integer","integer",
                                   "character", "character", "integer"))
names(ses_member)[which(names(ses_member)=="race")] <- "race_ses"


# CHECK FOR MULTIPLE OBSERVATIONS IN SES
check <- ses_member %>%
  group_by(patid) %>%
  filter(n()>1) # 0 rows

analysis_data6 <- merge(x=analysis_data5, y=ses_member[,-"race_ses"], by="patid", all.x = TRUE)




### 15. ADD COPAY RANGE -----------------------------------------------------
data7 <- data.table(analysis_data6)
  
# SUM UP COPAYS OF INDEX AC AND ACMO3
acpay1 <- data7[index_ac_dt==fill_dt, # index_ac==gen_name & 
            .("index_ac_copay"=sum(copay_sum)), by=patid]
# acpay1 <- data7[index_ac==gen_name & index_ac_dt==fill_dt,
#              .("index_ac_copay"=sum(copay_sum), days_sup, quantity, strength), by=patid][
#                ,"index_ac_strength_num":=gsub("[M[:punct:]].*$", "", strength)
#              ]
# acpay2 <- acpay1[,"index_ac_strength_num":=gsub("[M[:punct:]].*$", "", strength)]
data8 <- merge(x=data7, y=acpay1, by="patid", all.x = TRUE)

acpay_3mo <- data7[ac3mo_dt==fill_dt, #ac3mo==gen_name & 
                .("ac3mo_copay"=sum(copay_sum)), by=patid]
data8 <- merge(x=data8, y=acpay_3mo, by="patid", all.x = TRUE)

acpay_4mo <- data7[ac4mo_dt==fill_dt, #ac4mo==gen_name & 
                   .("ac4mo_copay"=sum(copay_sum)), by=patid]
data8 <- merge(x=data8, y=acpay_4mo, by="patid", all.x = TRUE)

# CREATE RANGE
data9 <- data8 %>%
    mutate(index_ac_copay_rng = case_when(
      index_ac_copay < 0 ~ "<0",
      index_ac_copay < 10 & index_ac_copay >=0 ~ "[0,10)",
      index_ac_copay >= 10 & index_ac_copay < 30 ~ "[10,30)",
      index_ac_copay >= 30 & index_ac_copay < 50 ~ "[30,50)",
      index_ac_copay >= 50 & index_ac_copay < 100 ~ "[50,100)",
      index_ac_copay >= 100 & index_ac_copay < 250 ~ "[100,250)",
      index_ac_copay >= 250 & index_ac_copay < 500 ~ "[250,500)",
      index_ac_copay >= 500 ~ "\u2265 500",
      TRUE ~ "Undefined"
    ))
  
data9 <- data9 %>%
  mutate(ac3mo_copay_rng = case_when(
    ac3mo_copay < 0 ~ "<0",
    ac3mo_copay < 10 & ac3mo_copay >=0 ~ "[0,10)",
    ac3mo_copay >= 10 & ac3mo_copay < 30 ~ "[10,30)",
    ac3mo_copay >= 30 & ac3mo_copay < 50 ~ "[30,50)",
    ac3mo_copay >= 50 & ac3mo_copay < 100 ~ "[50,100)",
    ac3mo_copay >= 100 & ac3mo_copay < 250 ~ "[100,250)",
    ac3mo_copay >= 250 & ac3mo_copay < 500 ~ "[250,500)",
    ac3mo_copay >= 500 ~ "\u2265 500",
    TRUE ~ "Undefined"
  ))

data9 <- data9 %>%
  mutate(ac4mo_copay_rng = case_when(
    ac4mo_copay < 0 ~ "<0",
    ac4mo_copay < 10 & ac4mo_copay >=0 ~ "[0,10)",
    ac4mo_copay >= 10 & ac4mo_copay < 30 ~ "[10,30)",
    ac4mo_copay >= 30 & ac4mo_copay < 50 ~ "[30,50)",
    ac4mo_copay >= 50 & ac4mo_copay < 100 ~ "[50,100)",
    ac4mo_copay >= 100 & ac4mo_copay < 250 ~ "[100,250)",
    ac4mo_copay >= 250 & ac4mo_copay < 500 ~ "[250,500)",
    ac4mo_copay >= 500 ~ "\u2265 500",
    TRUE ~ "Undefined"
  ))


data9 <- data.table(data9)
data9 <- data9[order(patid, fill_dt)]




### 16. add BMI records -----------------------------------------------------
bmi_info <- fread("../data/prog13_bmi.txt",
                  select = c("patid", "bmi"),
                  colClasses = c("character", "Date", "numeric", "Date"))

data10 <- merge(x=data9, y=bmi_info, by="patid", all.x=TRUE)




# SAVE TEMPORARILY
# wb <- createWorkbook(type = "xlsx")
saveRDS(data10, "../data/analysis_data.rds")
write.table(data10, "../data/analysis_data.txt", sep="\t")
 
# wb <- loadWorkbook("../data/prog100_analysis_data.xlsx")
# add_wb(sheetnm = "prog100_analysis_data", data = data10)
# saveWorkbook(wb, "../data/prog100_analysis_data.xlsx")










