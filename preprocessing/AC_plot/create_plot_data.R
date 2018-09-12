#####################################################################################
####                  CREATE WORKING DATA SET FOR THE SHINY APP                  ####
#### 1. CREATE NUMERIC SCALE FOR ANTICOAGULANTS ON THE Y AXIS                    ####
#### 2. CREATE NUMERIC SCALE FOR INR LAB ON THE Y AXIS                           ####
#### 3. CREATE NUMERIC SCALE FOR CONFINEMENT ON THE Y AXIS                       ####
#####################################################################################


library(dplyr)
library(tidyr)
library(data.table)
library(readxl)

setwd("C:/Users/Mengbing Li/Box Sync/OptumInsight_DataManagement/AC_plot")


### READ IN COMPLETED SUBSET DATA SET
patinfo <- readRDS("../data/subset_ac_days_multi_cancers.rds")

### 1. CREATE NUMERIC SCALE FOR ANTICOAGULANTS ON THE Y AXIS  ---------------
patinfo2 <- patinfo %>%
  group_by(patid, category) %>%
  mutate(id = (category=="DOACS")*seq(from=45, by=-0.25, length.out=n()) +
           (category=="LMWH")*seq(from=30, by=-0.25, length.out=n()) +
           (category=="Warfarin")*seq(from=15, by=-0.25, length.out=n()),
         category_num = 36*(category=="DOACS") + 24*(category=="LMWH") + 8*(category=="Warfarin"),
         fill_sup = fill_dt+days_sup) %>%
  ungroup(patid, category)
# SAVE THE DATA SET
saveRDS(patinfo2, "../AC_plot/working_data_patinfo.rds")



### 2. CREATE NUMERIC SCALE FOR INR LAB ON THE Y AXIS -----------------------

# ADD INR TEST DATES
lab_all <- data.table(fread("../data/prog1_lab_all.txt",
                            colClasses = c("character", "Date", "character", "Date",
                                           "numeric", "numeric", "numeric", "character")))
colnames(lab_all) <- tolower(colnames(lab_all))

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
inr_info$index_dt <- as.Date(inr_info$index_dt)
inr_info$lab_dt <- as.Date(inr_info$lab_dt)

inr_info2 <- unique(inr_info, by=c("patid", "lab_dt"))
inr_info2$inr_dt <- inr_info2$lab_dt

# DETERMINE VALUES OF POINTS OF LAB INR ON THE Y AXIS IN THE PLOT
inr_info2$patid <- as.factor(inr_info2$patid)
patid_levels <- levels(inr_info2$patid)     
inr_info2$inr_num <- rep(0,nrow(inr_info2))
inr_info2$category <- rep("0",nrow(inr_info2))

# INITIALIZE A VECTOR TO STORE THE VALUE FOR INR ON THE Y AIS
for( patids in patid_levels ){
  sub_inr <- inr_info2[inr_info2$patid==patids,]
  sub_patinfo <- patinfo2[patinfo2$patid==patids,]
  fill_dt_c <- sub_patinfo$fill_dt
  fill_sup_dt_c <- sub_patinfo$fill_sup
  id_c <- sub_patinfo$id
  category_c <- as.character(sub_patinfo$category)
  inr_dt_c <- sub_inr$inr_dt
  n_fill <- length(fill_dt_c)
  n_inr <- length(inr_dt_c)
  sub_inr_num <- rep(0,n_inr)
  sub_inr_category <- rep("0", n_inr)
  
  for(i in 1:n_inr){
    inr_dt_i <- inr_dt_c[i]
    max_dt <- max(fill_sup_dt_c)
    
    if (inr_dt_i <= max_dt) {
      tmp <- (inr_dt_i >= fill_dt_c & inr_dt_i <= fill_sup_dt_c)
      
      # INR HAPPENS BEFORE THE LAST END OF SUPPLY DATE
      # CASE 1: INR HAPPENS BETWEEN FILL DATE AND END OF SUPPLY DATE
      if (any(tmp)) {
        # FIND THE INDEX OF THE FIRST OVERLAPPING OCCURRENCE
        min_tmp <- which.max(tmp)
        sub_inr_num[i] <- id_c[min_tmp]
        sub_inr_category[i] <- category_c[min_tmp]
      }
      
      # CASE 2: INR DOES NOT HAPPEN BETWEEN THE OVERLAPPING PERIOD OF 
      # ANY FILL DATE AND END OF SUPPLY DATE
      else if (!any(tmp)) {
        tmp2 <- (inr_dt_i > fill_sup_dt_c & inr_dt_i > fill_dt_c)
        min_tmp <- which.min(tmp2)
        sub_inr_num[i] <- id_c[min_tmp]
        sub_inr_category[i] <- category_c[min_tmp]
      }
    }
    
    # CASE 3: INR HAPPENS AFTER THE LAST END OF SUPPLY DATE
    else {
      min_tmp <- which.max(fill_sup_dt_c)
      sub_inr_num[i] <- id_c[min_tmp]
      sub_inr_category[i] <- category_c[min_tmp]
    }
    
  }
  inr_info2$inr_num[inr_info2$patid==patids] <- sub_inr_num
  inr_info2$category[inr_info2$patid==patids] <- sub_inr_category
}

inr_info2$category <- as.factor(inr_info2$category)

# SAVE THE DATA SET
saveRDS(inr_info2, "working_data_inr.rds")




### 3. CREATE NUMERIC SCALE FOR CONFINEMENT ON THE Y AXIS  ------------------

confinfo <- readRDS("../data/prog6_conf_combine_admissions.rds")

# REMOVE DIAGNOSIS INFORMATION
confinfo2 <- confinfo[,-5:-9]
confinfo2 <- confinfo2 %>%
  filter(disch_dt_combined >= index_dt) %>%
  group_by(patid) %>%
  arrange(patid, admit_dt, disch_dt_combined) %>%
  mutate(id = sequence(n()),
         index_dt = as.Date(index_dt),
         admit_dt = as.Date(admit_dt),
         disch_dt_combined = as.Date(disch_dt_combined))
saveRDS(confinfo2, "working_data_conf.rds")




### 4. ADD INSURANCE ENROLLMENT TO THE PLOT ---------------------------------
## determine length of record
member <- fread("../data/member.txt", 
                select = c("Patid", "index_dt", "Eligeff", "Eligend"),
                colClasses=list(character=1, Date=2:4, numeric=5))
colnames(member) <- tolower(colnames(member))
member2 <- member[patid %in% patinfo2$patid, ]
member2$index_dt <- as.Date(member2$index_dt, format = "%m/%d/%Y")
member2[, 3:4] <- lapply(member2[, 3:4], as.Date)

# keep enrollment periods that cover the index VTE date or start after the index VTE date
member2 <- member2[index_dt <= eligend, ]
saveRDS(member2, "working_data_member.rds")


