#######################################################################################
# THIS PROGRAM CALCULATE SUMMARY STATISTICS.                                          #
# 0. OVERALL SUMMARY STATISTICS                                                       #
# SPECIFICALLY FOR TABLE 1, FREQUENCY COUNTS AND SD BY AC CATEGORY OF THESE VARIABLES #
# 1. AGE                                                                              #
# 2. SEX                                                                              #                    
# 3. SURGERY <= 4 WEEKS BEFORE INDEX VTE                                        
# 4. HOSPITALIZATION <= FOUR WEEKS BEFORE INDEX VTE                                   #
# 5. SMOKING <= 4 WEEKS BEFORE INDEX VTE                                                      
# 6. LABS:                                                                    
#   1) HEMOGLOBIN                                                                     #
#   2) PLATELETS                                                                      #
#   3) GFR                                                                            #
# 7. HISTORY OF VTE                                                                   #
# 8. ANTIPLATELETS                                                              
# 9. MALIGNANCY: INDEX CANCER TYPE                                                    #
# 10. VTE CATEGORY                                                                    #
# 11. CHARLSON COMORBIDITY INDEX                                                      #
#######################################################################################

setwd("C:/Users/Mengbing Li/Box Sync/OptumInsight_DataManagement/prep")


library(dplyr)
library(tidyr)
library(xlsx)
library(readxl)
library(reshape2)
library(data.table)

add_wb <- function(sheetnm, data, rownm = FALSE){
  removeSheet(wb, sheetName=sheetnm) # replace with new data frame
  sheet <- createSheet(wb, sheetName=sheetnm)
  addDataFrame(data, sheet, row.names = rownm) 
}

analysis_data <- data.table(readRDS("../data/prog100_analysis_data.rds"))
# SELECT DISTINCT ROWS
analysis_data2 <- analysis_data %>% distinct(patid, .keep_all=TRUE)

# add antiplatelet information
antiplainfo <- read_excel("../data/prog9_antiplatelets.xlsx", sheet = "prog9_antiplatelets")
names(antiplainfo) <- tolower(names(antiplainfo))
names(antiplainfo)[10] <- "antiplatelet"
antiplainfo3 <- merge(x=unique(analysis_data[,c("patid", "index_ac", "outcome")]), 
                     y=unique(antiplainfo[,c("patid", "antiplatelet")]),
                     by="patid", all.x = TRUE)

# add comorbidity information
comorbinfo <- data.table(read_excel("../data/prog10_comorbidity.xlsx"))
colnames(comorbinfo)[2] <- "comorbidities"
comorbinfo2 <- merge(x=unique(analysis_data[,c("patid", "index_ac", "outcome")]), 
                y=comorbinfo, by="patid", all.x = TRUE)

### 0. CALCULATE OVERALL DATA SUMMARY ---------------------------------------

# (1) GENDER
# all_n_gender <- analysis_data %>% distinct(patid, .keep_all=TRUE) %>%
#   summarise(total = n(),
#             n_male = sum(male), perc_male = sum(male)/n(),
#             n_female = n() - sum(male), perc_female = (n() - sum(male))/n() )
# total n_male perc_male n_female perc_female
# 14945   6563 0.4547848     7868   0.5452152


# (2) NUMBER OF OBSERVATIONS FOR EACH INDIVIDUAL
# ind_obs <- as.data.frame(analysis_data %>%
#   group_by(patid) %>%
#   summarise(n_ind_obs = n()) %>% 
#   group_by(n_ind_obs) %>%
#   summarise(freq_ind_obs = n()) %>%
#   arrange(n_ind_obs))

# WRITE THE OUTPUT TO EXISTING EXCEL
# wb <- loadWorkbook("../data/prog101_summary_stats.xlsx")

# add_wb(sheetnm = "overall_n_by_gender", data = all_n_gender)
# add_wb(sheetnm = "n_individual_observations", data = ind_obs)

# saveWorkbook(wb, "../data/prog101_summary_stats.xlsx")




### CALCULATE TABLE 1 STATISTICS -------------------------------------------

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
table1_tab <- function(data, rowvar, colvar, rownm, titleRow){
  table1_part <- as.data.frame.matrix(table(unlist(data[[rowvar]]), data[[colvar]]))
  ac <- levels(factor(data[[colvar]]))

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

  ### 1ST ROW: OVERALL COUNT
  overall <- data.matrix(table(analysis_data2[[colvar]]))
  # ADD ROW PERCENT TO OVERALL COUNT
  rowperc <- as.matrix(round(prop.table(overall,2)*100,2))
  overall2 <- as.data.frame(matrix(paste0(overall, " (", rowperc, "%)"), 
                                       nrow=nrow(overall), dimnames=dimnames(overall) ))
  overall2[,1] <- as.character(overall2[,1])
  doacs_total <- sum(overall[c("Apixaban","Dabigatran","Edoxaban","Rivaroxaban"),])
  overall2["DOACS Total",1] <- paste0(doacs_total, " (",
                                      round(doacs_total/colSums(overall)*100,2), "%)")
  overall2["Total",1] <- as.character(colSums(overall))
  # TRANSPOSE THE SUMMARY TABLE TO HAVE AC AS COLUMNS
  overall_nm <- rownames(overall2)
  overall_wide <- as.data.frame(t(overall2))
  colnames(overall_wide) <- overall_nm
  
  # SPECIFY THE ORDER OF COLUMNS
  if (colvar=="index_ac") {nm_order <- c("LMWH","Warfarin","Apixaban","Dabigatran",
                                         "Edoxaban","Rivaroxaban","DOACS Total",
                                         "Other","Unknown/Multiple", "Total") } else {
                           nm_order <- c("LMWH","Warfarin","Apixaban","Dabigatran",
                                         "Edoxaban","Rivaroxaban","DOACS Total",
                                         "Not captured", "Other","Unknown/Multiple", "Total")}
  overall_wide <- overall_wide[,nm_order]
  rownames(overall_wide) <- "Overall Counts (Row %)"
  
  
  ### 2ND ROW: MEAN AGE AND SD
  table1_all_age_general <- analysis_data2 %>%
    group_by_(colvar) %>%
    summarise(age = paste(round(mean(age),2), "±", round(sd(age),2)))
  
  table1_age_quantiles <- analysis_data2 %>%
    group_by_(colvar) %>%
    summarise(`0%`= as.character(quantile(age, probs=0)),
              `25%`=quantile(age, probs=0.25),
              `50%`=quantile(age, probs=0.5),
              `75%`=quantile(age, probs=0.75),
              `100%`=quantile(age, probs=1)) 
  table1_age_quantiles[,colvar] <- NULL
  table1_all_age_general2 <- cbind(table1_all_age_general, table1_age_quantiles)

  # CALCULATE COUNTS IN DOACS AND SUM UP AS DOACS TOTAL
  table1_all_age_doacs <- analysis_data2 %>%
    filter(category == "DOACS") %>%
    group_by(category) %>%
    summarise(age = paste(round(mean(age),2), "±", round(sd(age),2)))
  table1_age_doacs_quantiles <- analysis_data2 %>%
    filter(category == "DOACS") %>%
    group_by(category) %>%
    summarise(`0%`= as.character(quantile(age, probs=0)),
              `25%`=quantile(age, probs=0.25),
              `50%`=quantile(age, probs=0.5),
              `75%`=quantile(age, probs=0.75),
              `100%`=quantile(age, probs=1)) %>%
    select(-category)
  table1_age_doacs2 <- cbind(table1_all_age_doacs, table1_age_doacs_quantiles)
  
  colnames(table1_age_doacs2)[1] <- colvar
  table1_age_doacs2[,colvar][1] <- "DOACS Total"
  
  # COMBINE ALL AC CATEGORIES
  table1_all_age <- rbind(table1_all_age_general2, table1_age_doacs2)
  
  # TRANSPOSE THE SUMMARY TABLE TO HAVE AC AS COLUMNS
  table1_all_age_nm <- table1_all_age[[colvar]]
  table1_all_age_wide <- as.data.frame(t(table1_all_age[,-1]))
  colnames(table1_all_age_wide) <- table1_all_age_nm
  
  # CALCULATE TOTAL COUNTS AND AVERAGE AGE
  analysis_data_dist <- analysis_data2 %>%
    distinct(patid, .keep_all=TRUE)
  table1_all_age_wide$Total <- c(paste0(round(mean(analysis_data_dist$age),2), "±", round(sd(analysis_data_dist$age),2)),
                      quantile(analysis_data_dist$age))
  
  # REORDER COLUMNS
  table1_all_age_wide <- table1_all_age_wide[,nm_order]
  table1_all_age_wide[,1:ncol(table1_all_age_wide)] <- lapply(table1_all_age_wide[,1:ncol(table1_all_age_wide)], as.character)
  
  age_wide <- insertRow(table1_all_age_wide, rep(NA,ncol(table1_all_age_wide)),
                              paste("Age"), 1)
  
  rownames(age_wide) <- c("Age", "Mean ± SD",
                          "Quantiles \n 0%", "25%", "50%", "75%", "100%")
  
  
  
  ## COMBINE OVERALL COUNTS AND AGE
  overall_age <- rbind(overall_wide, age_wide)
  
  
  
  ### CALCULATE COUNTS ON CATEGORICAL VARIABLES -----------------------------------
  table1_age <-   table1_tab(analysis_data2, "age_cat", colvar,
                             c("18-34", "35-54", "55-64", "65-74", "75-84", "85-"),
                             "Age category")
  table1_age <- insertRow(table1_age, rep(NA,ncol(table1_age)),
                          paste("Counts (Column percent)"), 1)
  table1_sex <- table1_tab(analysis_data2, "male", colvar, c("Female", "Male"), "Sex")
  # table1_sex <- insertRow(table1_sex, rep(NA,ncol(table1_sex)),
  #                             paste("Counts (Column percent)"), 1)
  table1_surg <- table1_tab(analysis_data2, "surgery", colvar, c("No", "Yes"),
                            paste("Surgery"))
  # ADD A TITLE ROW SPECIFYING TIMING
  table1_surg <- insertRow(table1_surg, rep(NA,ncol(table1_surg)), paste("\u2264 4 weeks \n prior to index VTE"), 1)
  table1_hosp <- table1_tab(analysis_data2, "hospitalized", colvar, c("No ", "Yes "), 
                            paste("Hospitalized"))
  table1_smoke <- table1_tab(analysis_data2, "smoke", colvar, c("No  ", "Yes  "),
                             paste("Smoking"))
  
  table1_hemo <- table1_tab(rm_na("hemoglobin", colvar), "hemoglobin", colvar, NULL, "Hemoglobin")
  table1_hemo <- table1_hemo[c("Hemoglobin","<7 g/dL", "[7, 10] g/dL", ">10 g/dL"),] # SWITCH ROW ORDER
  table1_hemo <- insertRow(table1_hemo, rep(NA,ncol(table1_hemo)),
                           paste("During the 30 days prior to Index VTE (most recent value from Index VTE)"), 1)
  table1_pla <- table1_tab(rm_na("platelet", colvar), "platelet", colvar, NULL, "Platelets")
  table1_pla <- table1_pla[c("Platelets","<30K", "[30, 50)K", "[50, 100]K",">100K"),] # SWITCH ROW ORDER
  table1_gfr <- table1_tab(rm_na("gfr", colvar), "gfr", colvar, NULL, "GFR")
  table1_gfr <- table1_gfr[c("GFR","<15","[15, 30)","[30, 60]",">60"),] # SWITCH ROW ORDER
  
  table1_history <- table1_tab(analysis_data2, "vte_history", colvar, c("No   ", "Yes   "),
                               "History of VTE (ICD 9: V12.51)")
  table1_history <- insertRow(table1_history, rep(NA,ncol(table1_history)),
                              paste("During the 12 mo prior to index VTE"), 1)
  table1_anti <- table1_tab(antiplainfo3, "antiplatelet", colvar, NULL, "Antiplatelets")
  table1_mag <- table1_tab(analysis_data2, "cancer_type_combined", colvar, NULL, "Maglinancy")
  table1_mag <- insertRow(table1_mag, rep(NA,ncol(table1_mag)), paste("Any time"), 1)
  table1_vte <- table1_tab(analysis_data2, "index_vte_type", colvar, NULL, "Index VTE Type")
  table1_comorb <- table1_tab(comorbinfo2, "comorbidities", colvar, NULL, "Charlson Co-morbidity index")
  
  
  ### COMBINE ALL TABLE 1 ROWS ------------------------------------------------
  table1_pre <- rbind(table1_age, table1_sex, table1_surg, table1_hosp, table1_smoke, 
                      table1_hemo, table1_pla, table1_gfr, table1_history,
                      table1_anti, table1_mag, table1_vte, table1_comorb)
  table1 <- rbind(overall_age, table1_pre)
  return(table1)
}

# CALCULATE COUNTS BY INDEX AC AND OUTCOME RESPECTIVELY
table1_indexac <- table_calculate("index_ac")

table1_outcome <- table_calculate("outcome")

# COMBINE THE COUNTS
gap_col <- as.matrix(rep(NA, nrow(table1_indexac)), ncol=1)
colnames(gap_col)[1] <- "Left: Index AC; \n Right: Outcome"
table1 <- cbind(table1_indexac, gap_col, table1_outcome)







# 
# 
# ### CONDITIONAL LOGISTIC MODEL FOR COMPARING PROPORTIONS --------------------
# 
# # SELECT DISTINCT ROWS
# data2 <- analysis_data %>%
#   distinct(patid, index_ac, .keep_all = TRUE)
# 
# data2 <- analysis_data2[analysis_data2[[rowvar]]==i & !is.na(analysis_data2[[rowvar]]),]
# data3 <- data2 %>%
#   mutate(lmwh1 = index_ac == "LMWH",
#          lmwh2 = outcome == "LMWH", 
#          warfarin1 = index_ac == "Warfarin",
#          warfarin2 = outcome == "Warfarin") 
# 
# 
# data3 <- data2 %>% select(patid, index_ac, outcome) %>%
#   melt(id=c("patid"))
# 
# data4 <- merge(x=analysis_data2, y=data3, by="patid", all.x = TRUE)
# 
# clogit(value~variable + strata(patid), data = data3)
# 
# 
# 
# 
# 
# 
# ### ADD MCNEMAR TESTS ----------------------------------------------------
# 
# # COMPLETE 2X2 TABLE BY FILLING IN 0 COUNTS TO AVOID ERROR IN MCNEMAR TEST
# fill0 <- function(table){
#   if(nrow(table) != 2 | ncol(table) != 2){
#     nm <- c("FALSE","TRUE")
#     
#     if (nrow(table) != 2) {
#       x <- which(!nm %in% rownames(table))
#       table <- rbind(table, rep(0,ncol(table)))
#       rownames(table)[2] <- nm[x]
#     }
#     
#     if (ncol(table) != 2) {
#       x <- which(!nm %in% colnames(table))
#       table <- cbind(table, rep(0,nrow(table)))
#       colnames(table)[2] <- nm[x]
#     }
#     table <- table[nm,nm]
#   }
#   table
# }
# 
# # ROUND P-VALUES AND ADD ANNOTATION TO SMALL SAMPLE SIZE
# mctest <- function(table){
#   result <- mcnemar.test(table, correct = FALSE)$p.value
#   if (!is.na(result)) {
#     if (result < 1e-3) result <- "<0.001"
#     else if (result>=1e-3) result <- as.character(round(result),3)
#   }
#   
#   if ((!all(suppressWarnings(chisq.test(table)$expected >= 5))) | is.na(result)){
#     result <- paste0(result, "*")
#   }
#   result
# }
# 
# 
# # MCNEMAR TEST
# tests <- function(rowvar){
#   res <- c()
#   analysis_data2 <- analysis_data 
# 
#   values <- sort(unique(analysis_data2[[rowvar]]))
# 
#   for(i in values){
#     data2 <- analysis_data2[analysis_data2[[rowvar]]==i & !is.na(analysis_data2[[rowvar]]),]
#     data3 <- data2 %>%
#       mutate(lmwh1 = index_ac == "LMWH",
#              lmwh2 = outcome == "LMWH", 
#              warfarin1 = index_ac == "Warfarin",
#              warfarin2 = outcome == "Warfarin") 
#     
#     # LMWH AT INDEX AC VS LMWH AT OUTCOME
#     lmwh12 <- fill0(xtabs(data=data3, formula=~lmwh1+lmwh2))
#     res1 <- mctest(lmwh12)
#     
#     # WARFARIN AT INDEX AC VS LMWH AT OUTCOME
#     warf1_lmwh2 <- fill0(xtabs(data = data3, formula = ~warfarin1+lmwh2))
#     res2 <- mctest(warf1_lmwh2)
#     
#     # WARFARIN AT INDEX AC VS WARFARIN AT OUTCOME
#     warf12 <- fill0(xtabs(data = data3, formula = ~warfarin1+warfarin2))
#     res3 <- mctest(warf12)
#     
#     res <- rbind(res,c(res1, res2, res3))
#   }
#   colnames(res) <- c("p-value \n (index LMWH vs LMWH 3Mo)",
#                      "p-value \n (index warfarin vs LMWH 3Mo)",
#                      "p-value \n index Warfarin vs Warfarin 3Mo")
#   res
# }
# 
# 
# # CONSTRUCT NULL SECTION OF TABLE 1 WHERE VARIABLES ARE CONTINUOUS AND COLUMNS ARE P-VALUES
# table_head <- data.frame(matrix(vector(), nrow = which(rownames(table1) == "Sex")-2, ncol=3),
#            stringsAsFactors=F)
# colnames(table_head) <- c("p-value \n (index LMWH vs LMWH 3Mo)",
#                            "p-value \n (index warfarin vs LMWH 3Mo)",
#                            "p-value \n index Warfarin vs Warfarin 3Mo")
# varlist <- list("male"="male", "surgery"="surgery", "hospitalized"="hospitalized",
#                 "smoke"="smoke", "hemoglobin"="hemoglobin", "platelet"="platelet",
#                 "gfr"="gfr", "vte_history"="vte_history", "antiplatelet"="antiplatelet", 
#                 "cancer_type_combined"="cancer_type_combined", "index_vte_type"="index_vte_type", 
#                 "comorbidity"="comorbidity")
# 
# x <- lapply(varlist, tests)
# pvalues <- do.call(rbind,x)
# 
# part2 <- rbind(table_head, pvalues)
# 
# 
# rownms <- rownames(table1)
# null_index <- as.numeric(which(is.na(table1[,1])))
# for (i in 1:length(null_index)){
#   part2 <- insertRow(part2,rep(NA,ncol(part2)), paste0("x",i), null_index[i])
# }
# rownames(part2) <- rownames(table1)
# 
# # COMBINE THE COUNTS
# gap_col2 <- as.matrix(rep(NA, nrow(table1)), ncol=1)
# colnames(gap_col2)[1] <- "McNemar's Test"
# gap_col2[1] <- "*: small expected sample size (<5)"
# table3 <- cbind(table1, gap_col2, part2)






# SAVE TO EXCEL
wb <- loadWorkbook("../data/summary_stats.xlsx")

removeSheet(wb, sheetName="table1") # replace with new data frame
sheet <- createSheet(wb, sheetName="table1")
addDataFrame(table1, sheet, row.names = TRUE)

saveWorkbook(wb, "../data/summary_stats.xlsx")


print("Average comorbidity score"); 
analysis_data2$outcome2 <- with(analysis_data2,
                                ifelse(outcome %in% c("Apixaban", "Dabigatran", "Edoxaban", "Rivaroxaban"),
                                       "DOAC", outcome))
s <- analysis_data2 %>% group_by(outcome2) %>%
  summarise("Average comorbidity score "=mean(charlson_comorb_score))
print(s)
