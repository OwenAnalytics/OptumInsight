########################################################################
#' Program name: prog_table1_v3.R
#' 
#' Description: The program calculates Table 1 summary statistics, based
#' on Copy of Optum Table 1 and 2 w regression 7-26-18.xlsx in
#' C:\Users\mengbing\Box Sync\Optum Insight, AIM 1 of CAT project\==paper\Active Version of Paper.
#' Variables include:
#' 1. AGE                                                                              
#' 2. SEX                                                                              
#' 3. SURGERY <= 4 WEEKS BEFORE INDEX VTE
#' 4. HOSPITALIZATION <= FOUR WEEKS BEFORE INDEX VTE                                   
#' 5. SMOKING <= 4 WEEKS BEFORE INDEX VTE
#' 6. LABS:
#'   1) HEMOGLOBIN                                                                     
#'   2) PLATELETS                                                                     
#'   3) GFR                                                                            
#' 7. HISTORY OF VTE                                                                   
#' 8. ANTIPLATELETS
#' 9. MALIGNANCY: INDEX CANCER TYPE                                                    
#' 10. VTE CATEGORY                                                                    
#' 11. CHARLSON COMORBIDITY INDEX
#' 
#' Author: Mengbing Li
#' 
#' Created: Wednesday 04/11/2018
#' 
#' Revisions: 08/01/2018 - Add summary of AC4Mo
#'            09/23/2018 - Add p-values from ANOVA; add header information 
########################################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/preprocessing/prep")

library(dplyr)
library(tidyr)
library(xlsx)
library(readxl)
library(reshape2)
library(survival)
library(data.table)
library(MASS)
options(knitr.kable.NA = '')

add_wb <- function(sheetnm, data, rownm = FALSE){
  removeSheet(wb, sheetName=sheetnm) # replace with new data frame
  sheet <- createSheet(wb, sheetName=sheetnm)
  addDataFrame(data, sheet, row.names = rownm) 
}

analysis_data <- fread("../data/patient_data.txt")

# SELECT DISTINCT ROWS AND CREATE NEW CATEGORIES
analysis_data2 <- analysis_data %>%
  distinct(patid, .keep_all=TRUE) %>%
  mutate(ac3mo2 = case_when(
    ac3mo %in% c("Apixaban", "Dabigatran", "Edoxaban", "Rivaroxaban") ~ "DOAC Total",
    ac3mo %in% c("Other", "Unknown/Multiple") ~ "Other/Unknown/Multiple",
    TRUE ~ ac3mo
  ),
  ac4mo2 = case_when(
    ac4mo %in% c("Apixaban", "Dabigatran", "Edoxaban", "Rivaroxaban") ~ "DOAC Total",
    ac4mo %in% c("Other", "Unknown/Multiple") ~ "Other/Unknown/Multiple",
    TRUE ~ ac4mo
  )
  )


# add antiplatelet information
antiplainfo <- read_excel("../data/prog9_antiplatelets.xlsx", sheet = "prog9_antiplatelets")
names(antiplainfo) <- tolower(names(antiplainfo))
names(antiplainfo)[10] <- "antiplatelet"
antiplainfo2 <- merge(x=unique(analysis_data2[,c("patid", "ac3mo2", "ac4mo2")]), 
                     y=unique(antiplainfo[,c("patid", "antiplatelet")]),
                     by="patid", all.x = TRUE)

# add comorbidity information
comorbinfo <- data.table(read_excel("../data/prog10_comorbidity.xlsx"))
colnames(comorbinfo)[2] <- "comorbidities"
comorbinfo2 <- merge(x=unique(analysis_data2[,c("patid", "ac3mo2", "ac4mo2")]), 
                y=comorbinfo, by="patid", all.x = TRUE)




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
  ac <- unique(data[[colvar]])

  # CHECK WHICH CATEGORIES DO NOT APPEAR IN THE COLUMN AND ASSIGN 0 TO THE COUNTS
  x <- which(! ac %in% colnames(table1_part))
  if (length(x) != 0) table1_part[,ac[x]] <- 0 

  # SPECIFY THE ORDER OF COLUMNS
  nm_order <- c("LMWH","Warfarin","DOAC Total",
                "Not captured", "Other/Unknown/Multiple")
  
  table1_part <- table1_part[,nm_order]
  if (!is.null(rownm)) rownames(table1_part) <- rownm

  # ADD COLUMN PERCENT FOR EACH COUNT
  table1_part <- as.matrix(table1_part)
  colperc <- as.matrix(round(prop.table(data.matrix(table1_part),2)*100,2))
  table1_part2 <- as.data.frame(matrix(paste0(table1_part, " (", colperc, "%)"), 
          nrow=nrow(table1_part), dimnames=dimnames(table1_part) ))
  
  table1_part2 <- insertRow(table1_part2, rep(NA,ncol(table1_part2)), titleRow, 1)
  table1_part2
}


round_pvalues <- function(x){
  if (x < 0.001) {
    return("<0.001 ***")
  } else if (0.001 <= x & x < 0.01) {
    return(paste(round(x, 3), "**"))
  } else if (0.01 <= x & x < 0.05) { 
    return(paste(round(x, 3), "*"))
  } else return(round(x, 3))
}


table_calculate <- function(colvar){
  # OUTPUT PATIENTS WHO GOT MULTIPLE AC ON THE FIRST FILL DATE AFTER INDEX VTE

  # REMOVE NA VALUES TO AVOID ERROR IN TABULATION
  rm_na <- function(variable, colvar){
    as.data.frame(analysis_data2 %>%
                    dplyr::select(patid, colvar, variable) %>%
                    filter(!is.na(variable)))
  }

  ### 1ST ROW: OVERALL COUNT
  overall <- data.matrix(table(analysis_data2[[colvar]]))
  # ADD ROW PERCENT TO OVERALL COUNT
  rowperc <- as.matrix(round(prop.table(overall,2)*100,2))
  overall2 <- as.data.frame(matrix(paste0(overall, " (", rowperc, "%)"), 
                                       nrow=nrow(overall), dimnames=dimnames(overall) ))
  overall2[,1] <- as.character(overall2[,1])
  overall_nm <- rownames(overall2)
  overall_wide <- as.data.frame(t(overall2))
  colnames(overall_wide) <- overall_nm
  
  # SPECIFY THE ORDER OF COLUMNS
  nm_order <- c("LMWH","Warfarin","DOAC Total",
                "Not captured", "Other/Unknown/Multiple")
  overall_wide <- overall_wide[,nm_order]
  rownames(overall_wide) <- "Overall Counts (Row %)"
  
  
  ### 2ND ROW: MEAN AGE AND SD
  table1_age_general <- analysis_data2 %>%
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
  table1_age_general2 <- cbind(table1_age_general, table1_age_quantiles)

  # TRANSPOSE THE SUMMARY TABLE TO HAVE AC AS COLUMNS
  table1_age_nm <- table1_age_general2[[colvar]]
  table1_age_wide <- as.data.frame(t(table1_age_general2[,-1]))
  colnames(table1_age_wide) <- table1_age_nm
  
  # CALCULATE TOTAL COUNTS AND AVERAGE AGE
  analysis_data_dist <- analysis_data2 %>%
    distinct(patid, .keep_all=TRUE)
  table1_age_wide$Total <- c(paste0(round(mean(analysis_data_dist$age),2), "±", round(sd(analysis_data_dist$age),2)),
                      quantile(analysis_data_dist$age))
  
  # REORDER COLUMNS
  table1_age_wide <- table1_age_wide[,nm_order]
  table1_age_wide[,1:ncol(table1_age_wide)] <- lapply(table1_age_wide[,1:ncol(table1_age_wide)], as.character)
  
  age_wide <- insertRow(table1_age_wide, rep(NA,ncol(table1_age_wide)),
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
  table1_anti <- table1_tab(antiplainfo2, "antiplatelet", colvar, NULL, "Antiplatelets")
  table1_mag <- table1_tab(analysis_data2, "cancer_type_combined", colvar, NULL, "Malignancy")
  table1_mag <- insertRow(table1_mag, rep(NA,ncol(table1_mag)), paste("Any time"), 1)
  table1_vte <- table1_tab(analysis_data2, "index_vte_type", colvar, NULL, "Index VTE Type")
  table1_comorb <- table1_tab(comorbinfo2, "comorbidities", colvar, NULL, "Charlson Co-morbidity index")
  
  
  ### COMBINE ALL TABLE 1 ROWS ------------------------------------------------
  table1_pre <- rbind(table1_age, table1_sex, table1_surg, table1_hosp, table1_smoke, 
                      table1_hemo, table1_pla, table1_gfr, table1_history,
                      table1_anti, table1_mag, table1_vte, table1_comorb)
  table1 <- rbind(overall_age, table1_pre)
  # table1 <- table1[, c("LMWH","Warfarin","DOACS Total",
  #                      "Not captured", "Other/Unknown/Multiple")]

  return(table1)
}

# CALCULATE COUNTS BY ac3mo AND ac4mo RESPECTIVELY
table1_ac3mo <- table_calculate("ac3mo2")
table1_ac4mo <- table_calculate("ac4mo2")




### Add p-values comparing columns ------------------------------------------

#' Add p-values of LRT from independence loglinear model:
#' add_p_catVar adds p values calculated from likelihood ratio test of an
#' independence loglinear model. 
#' data = input data set
#' summary_table = pre-calculated table. table1_ac3mo or table1_ac4mo
#' row.variable = covariates in Table 1
#' col.variable = AC at 3 months or at 4 months
#' table_rowname = row name of summary_table, to which row the p-value should go
add_p_catVar <- function(data, summary_table, row.variable, col.variable, table_rowname){
  nlevels_row <- length(unique(data[[row.variable]]))
  nlevels_col <- length(unique(data[[col.variable]]))
  tab <- array(table(data[[row.variable]], data[[col.variable]]),
               c(nlevels_row, nlevels_col))
  fit <- loglm(~ 1 + 2, tab)
  p_var <- round_pvalues(1 - pchisq(fit$pearson, fit$df))
  row.index <- which(rownames(summary_table) == table_rowname)
  summary_table$p[row.index] <- p_var
  return(summary_table)
}


add_p_to_AC <- function(data, ac.var, summary_table_name){
  # initialize p-value column
  summary_table_name[["p"]] <- NA
  
  # add p-value of overall counts
  overall <- table(data[[ac.var]])
  p_overall <- round_pvalues(chisq.test(overall)$p.value)
  overall.index <- which(rownames(summary_table_name) == "Overall Counts (Row %)")
  summary_table_name[["p"]][overall.index] <- p_overall
  
  # Add p-values of ANOVA 
  formula.age <- paste("age ~ ", ac.var, sep = "")
  fit_age <- anova(lm(formula.age, data = analysis_data2))
  p_age <- round_pvalues(fit_age$`Pr(>F)`[1])
  age.index <- which(rownames(table1_ac3mo) == "Mean ± SD")
  summary_table_name[["p"]][age.index] <- p_age
  
  
  # add p-values to the following variables using analysis_data2
  vars_to_test <- rbind(
    c("age_cat", "Age category"),
    c("male", "Sex"),
    c("surgery", "Surgery"),
    c("hospitalized", "Hospitalized"),
    c("smoke", "Smoking"),
    c("hemoglobin", "Hemoglobin"),
    c("platelet", "Platelet"),
    c("gfr", "GFR"),
    c("vte_history", "History of VTE (ICD 9: V12.51)"),
    c("cancer_type_combined", "Malignancy"),
    c("index_vte_type", "Index VTE Type")
  )
  
  for(j in 1:nrow(vars_to_test)){
    summary_table_name <- add_p_catVar(data = data,
                         summary_table = summary_table_name,
                         row.variable = vars_to_test[j, 1], 
                         col.variable = ac.var, 
                         table_rowname = vars_to_test[j, 2])
  }
  
  
  # add p-values to the variables using other data
  summary_table_name <- add_p_catVar(data = antiplainfo2,
                               summary_table = summary_table_name,
                               row.variable = "antiplatelet", col.variable = ac.var, 
                               table_rowname = "Antiplatelets")
  
  summary_table_name <- add_p_catVar(data = comorbinfo2,
                               summary_table = summary_table_name,
                               row.variable = "comorbidities", col.variable = ac.var, 
                               table_rowname = "Charlson Co-morbidity index")
  
  return(summary_table_name)
}


# remove people who have Other AC
data_ac3mo <- analysis_data2 %>%
  filter(!ac3mo2 %in% c("Other/Unknown/Multiple", "Not captured"))
data_ac3mo$ac3mo2 <- factor(data_ac3mo$ac3mo2)

data_ac4mo <- analysis_data2 %>%
  filter(!ac4mo2 %in% c("Other/Unknown/Multiple", "Not captured"))
data_ac4mo$ac4mo2 <- factor(data_ac4mo$ac4mo2)


table1_ac3mo <- add_p_to_AC(data = data_ac3mo, "ac3mo2", table1_ac3mo)
table1_ac4mo <- add_p_to_AC(data = data_ac4mo, "ac4mo2", table1_ac4mo)



# COMBINE THE COUNTS
gap_col <- as.matrix(rep(NA, nrow(table1_ac3mo)), ncol=1)
colnames(gap_col)[1] <- "Left: AC3Mo; \n Right: AC4Mo"
table1 <- cbind(table1_ac3mo, gap_col, table1_ac4mo)


# SAVE TO EXCEL
wb <- loadWorkbook("../data/summary_stats.xlsx")

removeSheet(wb, sheetName="table1_cleanedForAbstract") # replace with new data frame
sheet <- createSheet(wb, sheetName="table1_cleanedForAbstract")
addDataFrame(table1, sheet, row.names = TRUE)

saveWorkbook(wb, "../data/summary_stats.xlsx")


print("Average comorbidity score"); 
s <- analysis_data2 %>% group_by(ac3mo2) %>%
  summarise("Average comorbidity score " = mean(charlson_comorb_score))
print(s)

fit_comorb_score <- anova(lm(charlson_comorb_score ~ ac3mo2, data = analysis_data2))
fit_comorb_score




## TEST CODES -------------------------------------------- 

# running ANOVA to compare mean ages across groups
# fit_age.anova <- anova(lm(age ~ ac3mo2, data=data_ac3mo))
# s.fit_age <- summary(fit_age)
# 
# # same as above
# # fit_age <- aov(age ~ ac3mo2, data=data_ac3mo)
# # s.fit_age <- summary(fit_age)
# 
# # running ANOVA to compare sex proportions across groups
# fit_sex <- aov(male ~ ac3mo2, data=data_ac3mo)
# summary(fit_sex)
# 
# # run chi square test on sex
# sex_table <- table(data_ac3mo[["male"]], data_ac3mo[["ac3mo2"]])
# chisq.test(sex_table)
# 
# 
# # run chi square test on sex
# agecat_table <- table(data_ac3mo[["age_cat"]], data_ac3mo[["ac3mo2"]])
# chisq.test(agecat_table)
# 
# 
# summary(fit.age_cat <- glm(age_cat ~ ac3mo2, family = "binomial", data = data_ac3mo))
# 

