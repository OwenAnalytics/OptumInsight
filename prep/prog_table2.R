#######################################################################################
# THIS PROGRAM CALCULATE TABLE 2 STATISTICS.                                          #
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
#' 13. COPAY (TBD)
#' 14. POS CODES (TBD)
#######################################################################################

setwd("C:/Users/Mengbing Li/Box Sync/OptumInsight_DataManagement/prep")

library(dplyr)
library(tidyr)
library(xlsx)
library(readxl)
library(data.table)

add_wb <- function(sheetnm, data, rownm = FALSE){
  removeSheet(wb, sheetName=sheetnm) # replace with new data frame
  sheet <- createSheet(wb, sheetName=sheetnm)
  addDataFrame(data, sheet, row.names = rownm) 
}

analysis_data <- data.table(readRDS("../data/prog100_analysis_data.rds"))
# SELECT DISTINCT ROWS
analysis_data2 <- analysis_data %>% distinct(patid, .keep_all=TRUE)

# UNKNOWN VALUE IN THESE VARIABLES ARE CODED AS "U"
ses1 <- c("education", "fed_poverty",  "occupation")
analysis_data2[, (ses1) := lapply(.SD, function(x) ifelse(is.na(x), "U", x)), .SDcols=ses1]

# UNKNOWN VALUE IN THESE VARIABLES ARE CODED AS "0"
ses2 <- c("home_ownership", "networth_range", "income_range")
analysis_data2[, (ses2) := lapply(.SD, function(x) ifelse(is.na(x), "0", x)), .SDcols=ses2]

### CALCULATE TABLE 1 STATISTICS -------------------------------------------

# OBTAIN ONLY TIME INVARIANT COVARIATES
# analysis_data2 <- analysis_data %>%
#   distinct(patid, outcome, .keep_all = TRUE) %>%
#   mutate(race = ifelse(race=="", "U", race))

# OUTPUT PATIENTS WHO GOT MULTIPLE AC ON THE FIRST FILL DATE AFTER INDEX VTE
# multi_ac <- as.data.frame(analysis_data2 %>% group_by(patid) %>% filter(n()>1))
# 
# add_wb(sheetnm = "multiple_index_ac", data = multi_ac)
# saveWorkbook(wb, "../data/prog101_summary_stats.xlsx")



# FUNCTION TO INSERT A NEW ROW
insertRow <- function(existingDF, newrow, newtitle, r) {
  if (r == 1) {
    newnm <- c(newtitle, rownames(existingDF))
  } else {
    newnm <- c(rownames(existingDF)[1:r-1], newtitle, rownames(existingDF)[seq(r,nrow(existingDF))])
  }
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  rownames(existingDF) <- newnm
  existingDF
}



# TABULATE COUNTS BY AC CATEGORY:
#' data: input data set 
#' rowvar: variable whose categories go to the row. NULL if using original variable values as row name
#' titleRow: title on the separating row
table2_tab <- function(data, rowvar, colvar, rownm, titleRow){
  table1_part <- as.data.frame.matrix(table(unlist(data[[rowvar]]), data[[colvar]]))
  ac <- levels(factor(data[[colvar]]))
  # c("LMWH","Warfarin","Apixaban","Dabigatran", "Edoxaban","Rivaroxaban",
  # "Not captured", "Other", "Unknown/Multiple")
  # CHECK WHICH CATEGORIES DO NOT APPEAR IN THE COLUMN AND ASSIGN 0 TO THE COUNTS
  x <- which(! ac %in% colnames(table1_part))
  if (length(x) != 0) table1_part[,ac[x]] <- 0 
  table1_part$`DOACS Total` <- rowSums(table1_part[,c("Apixaban","Dabigatran",
                                                      "Edoxaban","Rivaroxaban")])
  # SPECIFY THE ORDER OF COLUMNS
  if (colvar=="index_ac") {nm_order <- c("LMWH","Warfarin","Apixaban","Dabigatran",
                                         "Edoxaban","Rivaroxaban","DOACS Total",
                                         "Other","Unknown/Multiple") } else {
                                           nm_order <- c("LMWH","Warfarin","Apixaban","Dabigatran",
                                                         "Edoxaban","Rivaroxaban","DOACS Total",
                                                         "Not captured", "Other","Unknown/Multiple")}
  table1_part <- table1_part[,nm_order]
  if (!is.null(rownm)) rownames(table1_part) <- rownm
  # TO INSERT INDENT: sapply(rownm, function(x) c(paste0("=CONCATENATE(\"    \", \"", x, "\")")))
  
  table1_part$Total <- rowSums(table1_part[,-which(colnames(table1_part)=="DOACS Total")])
  
  # ADD COLUMN PERCENT FOR EACH COUNT
  table1_part <- as.matrix(table1_part)
  colperc <- as.matrix(round(prop.table(data.matrix(table1_part),2)*100,2))
  table1_part2 <- as.data.frame(matrix(paste0(table1_part, " (", colperc, "%)"), 
                                       nrow=nrow(table1_part), dimnames=dimnames(table1_part) ))
  
  table1_part2 <- insertRow(table1_part2, rep(NA,ncol(table1_part2)), titleRow, 1)
  table1_part2
}





table_calculate <- function(colvar){
   # OUTPUT PATIENTS WHO GOT MULTIPLE AC ON THE FIRST FILL DATE AFTER INDEX VTE
  # multi_ac <- as.data.frame(analysis_data2 %>% group_by(patid) %>% filter(n()>1))
  # 
  # add_wb(sheetnm = "multiple_index_ac", data = multi_ac)
  # saveWorkbook(wb, "../data/prog101_summary_stats.xlsx")
  
  # REMOVE NA VALUES TO AVOID ERROR IN TABULATION
  rm_na <- function(variable, colvar){
    as.data.frame(analysis_data2 %>% 
                    select(patid, colvar, variable) %>%
                    filter(!is.na(variable)))
  }
  
  # TABULATE BY EACH TABLE 2 COVARIATE

  table2_edu <- table2_tab(analysis_data2, "education", colvar,
                           c("Less than 12th Grade", "High School Diploma",
                             "Less than Bachelor Degree","Bachelor Degree Plus", "Unknown"),
                           "Education Level")
  
  table2_pov <- table2_tab(analysis_data2, "fed_poverty", colvar, 
                           c("Above 400% FPL", "Below 400% FPL", "Unknown/Missing"),
                           "Federal Poverty Level")

  table2_home <- table2_tab(analysis_data2, "home_ownership", colvar, 
                           c("Unknown ", "Probable Homeowner"), "Home Ownership")
  
  table2_income <- table2_tab(analysis_data2, "income_range", colvar, 
                            c("Unknown  ", "<$40K", "$40K-$49K", "$50K-$59K",
                              "$60K-$74K", "$75K-$99K", "$100K+"), "Household Income Range")
  
  table2_networth <- table2_tab(analysis_data2, "networth_range", colvar, 
                              c("Unknown   ", "<$25K", "$25K-$149K", "$150K-$249K",
                                "$250K-$499K", "$500K+"), "Networth Range")
  
  table2_occup <- table2_tab(analysis_data2, "occupation", colvar,
                             c("Manager/Owner/Professional", "White Collar/Health/Civil Service/Military",
                               "Blue Collar", "Homemaker/Retired", "Missing/Unknown"),
                               "Occupation")
  
  table2_race <- table2_tab(analysis_data2, "race", colvar,
                             c("","Asian", "Black", "Hispanic", "Unknown    ", "White"),
                             "Race")
  # WEIRD THAT THERE IS A "36" CATEGORY IN RACE WHEN RUNNING THE FUNCTION
  table2_race <- table2_race[-2,]

  table2_region <- table2_tab(analysis_data2, "division", colvar, NULL, "Region")
  
  table2_insurance <- table2_tab(analysis_data2, "product", colvar, NULL, "Insruance")

  table2_copay <- table2_tab(analysis_data2, "outcome_copay_rng", colvar,
                            c("<$10", "$10-30", "$100-250", "$250-500", 
                              "$30.01-50", "$50.01-100", 
                              "$>500", "Undefined"), "Copay")
  table2_copay <- table2_copay[c("Copay", "<$10", "$10-30", "$30.01-50", "$50.01-100", 
                                 "$100-250", "$250-500", "$>500", "Undefined"),]
  
  ### COMBINE ALL TABLE 1 ROWS ------------------------------------------------
  table2 <- rbind(table2_edu, table2_pov, table2_home, table2_income, 
                      table2_networth, table2_occup, table2_race, table2_region,
                      table2_insurance, table2_copay)
  table2[,1:9] <- lapply(table2[,1:9], as.character)
  return(table2)
}


# CALCULATE COUNTS BY INDEX AC AND OUTCOME RESPECTIVELY
table2_indexac <- table_calculate("index_ac")

table2_outcome <- table_calculate("outcome")

# COMBINE THE COUNTS
gap_col <- as.matrix(rep(NA, nrow(table2_indexac)), ncol=1)
colnames(gap_col)[1] <- "Left: Index AC; \n Right: Outcome"
table2 <- cbind(table2_indexac, gap_col, table2_outcome)








# SAVE TO EXCEL
wb <- loadWorkbook("../data/summary_stats.xlsx")

# createSheet(wb, sheetName = "table2")
removeSheet(wb, sheetName="table2") # replace with new data frame
sheet <- createSheet(wb, sheetName="table2")
addDataFrame(table2, sheet, row.names = TRUE) 

saveWorkbook(wb, "../data/summary_stats.xlsx")



## Add summary statistics of copay to table 2
analysis_data2$outcome2 <- with(analysis_data2,
  ifelse(outcome %in% c("Apixaban", "Dabigatran", "Edoxaban", "Rivaroxaban"),
         "DOAC", outcome))
summary_copay <- data.frame(analysis_data2 %>%
  group_by(outcome2) %>%
  summarise("Mean "=mean(outcome_copay),
            "Median"=median(outcome_copay),
            "Standard Deviation"=sd(outcome_copay),
            "Min"=min(outcome_copay),
            "Max"=max(outcome_copay)))


# SAVE TO EXCEL
wb <- loadWorkbook("../data/summary_stats.xlsx")

# createSheet(wb, sheetName = "table2")
removeSheet(wb, sheetName="summary_copay") # replace with new data frame
sheet <- createSheet(wb, sheetName="summary_copay")
addDataFrame(summary_copay, sheet, row.names = TRUE) 

saveWorkbook(wb, "../data/summary_stats.xlsx")

