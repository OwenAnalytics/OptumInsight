########################################################################
#' Program name: multinom_logit_ac3mo_v2_ORPlot.R
#' 
#' Description: The program creates a plot for the odds ratios estimated
#' from multinomial logistic model.
#' 
#' Author: Mengbing Li
#' 
#' Created: Wednesday 11/28/2018
#' 
#' Revisions: 
########################################################################

setwd('C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/preprocessing/analysis/multinominial_logit_ac3mo')

library(dplyr)
library(tidyr)  
library(data.table)
library(knitr)
library(kableExtra)
library(reshape2)
library(xlsx)
library(readxl)
library(mlogit)
library(nnet)

dat <- fread("../../data/patient_data.txt")

vars_not_wanted <- c("clmid", "fill_dt", "category", "brand_name", "gen_name", "copay", "copay_sum", "days_sup", "quantity", "strength", "npi")
dat1 <- unique(dat[,(vars_not_wanted) := NULL])

# UNKNOWN VALUE IN THESE VARIABLES ARE CODED AS "U"
ses1 <- c("education", "fed_poverty",  "occupation")
dat2 <- dat1[, (ses1) := lapply(.SD, function(x) ifelse(is.na(x), "U", x)), .SDcols=ses1]

# UNKNOWN VALUE IN THESE VARIABLES ARE CODED AS "0"
ses2 <- c("home_ownership", "networth_range", "income_range")
dat2[, (ses2) := lapply(.SD, function(x) ifelse(is.na(x), "0", x)), .SDcols=ses2]


# combine all doacs categories into doac
dat2$ac3mo2 <- with(dat2, ifelse(ac3mo %in% c("Apixaban", "Dabigatran", "Edoxaban", "Rivaroxaban"), "DOAC", ac3mo))
dat2[, ac3mo2 := as.factor(ac3mo2)]
dat2 <- within(dat2, ac3mo2 <- relevel(ac3mo2, ref = "LMWH"))

dat2$ac4mo2 <- with(dat2, ifelse(ac4mo %in% c("Apixaban", "Dabigatran", "Edoxaban", "Rivaroxaban"), "DOAC", ac4mo))
dat2[, ac4mo2 := as.factor(ac4mo2)]
dat2 <- within(dat2, ac4mo2 <- relevel(ac4mo2, ref = "LMWH"))


# unwrap combined index VTE types into columns of binary indicators
vte_info <- read_excel("../../data/prog8_vte.xlsx", sheet = "raw_VTE_POS")
vte_info2 <- unique(data.table(vte_info)[!is.na(VTE_type),.(patid, VTE_type)])
vte <- model.matrix(patid~VTE_type+0, data=vte_info2)
colnames(vte) <- gsub("type", "", colnames(vte))
vte2 <- data.frame(cbind(vte_info2[,1], vte))
vte3 <- aggregate(. ~ patid, vte2, sum)
dat2 <- merge(x=dat2, y=vte3, by="patid", all.x = TRUE)
rm(vte_info, vte_info2, vte, vte2, vte3)

# combine the 3 rarest VTE types
dat2[,VTE_ivc_rv_pv:= as.numeric((VTE_IVC | VTE_Portal.vein | VTE_Renal.vein))]

comorb_names <- colnames(dat2)[grep("comorbidity", colnames(dat2))]
dat2[, (comorb_names) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = comorb_names]
dat2[,comorb01 := !is.na(comorbidities)]
# count the number of comorbidities
dat2[, ("n_comorb") := rowSums(.SD), .SDcols=comorb_names]
# summary(dat2$n_comorb)

dat2[, race := ifelse(race=="", "U", race)]

antip_names <- colnames(dat2)[grep("antiplatelet", colnames(dat2))]
dat2[, (antip_names) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = antip_names]
dat2[, ("antiplatelet") := (rowSums(.SD) > 0), .SDcols=antip_names]

dat2[,c("race", "division", "product", "education", "fed_poverty","income_range", "networth_range", "occupation")] <- lapply(dat2[,c("race", "division", "product", "education", "fed_poverty","income_range", "networth_range", "occupation")], as.factor)

# scaled age
dat2$age_s <- scale(dat2$age)
# center age by mean
dat2$age_c <- dat2$age - mean(dat2$age)

# combine the smallest education level to the next level
dat2[,education2 := as.factor(ifelse(as.character(education) %in% c("B", "A"), "AB", as.character(education)))]

# use the largest level as the reference level
dat2 <- within(dat2, education <- relevel(education, ref = "C"))
dat2 <- within(dat2, education2 <- relevel(education2, ref = "C"))
dat2 <- within(dat2, race <- relevel(race, ref = "W"))
dat2 <- within(dat2, income_range <- relevel(income_range, ref = "6"))
dat2 <- within(dat2, product <- relevel(product, ref = "HMO"))

# combine asian and hispanic for race
dat2[,race2 := as.factor(ifelse(as.character(race) %in% c("H", "A"), "AsianHispanic", as.character(race)))]

# combine regions for division
dat2 <- data.table(dat2 %>%
  mutate(division2 = case_when(
    division %in% c("WEST SOUTH CENTRAL", "WEST NORTH CENTRAL") ~ "WEST CENTRAL",
    division %in% c("EAST SOUTH CENTRAL", "EAST NORTH CENTRAL") ~ "EAST CENTRAL",
    division %in% c("SOUTH ATLANTIC", "MIDDLE ATLANTIC", "NEW ENGLAND") ~ "ATLANTIC_ENGLAND",
    division %in% c("MOUNTAIN", "PACIFIC") ~ "MOUNTAIN_PACIFIC",
    TRUE ~ "UNKNOWN"
  )))

# combine insurace type
dat2[,product2 := as.factor(ifelse(as.character(product) %in% c("EPO", "IND", "PPO"), "EPO_IND_PPO", as.character(product)))]
dat2 <- within(dat2, product2 <- relevel(product2, ref = "HMO"))

# obtain variable names for index VTE types
# all distinct VTE categories
vte_names0 <- colnames(dat2)[grep("VTE_", colnames(dat2))]
vte_names1 <- vte_names0[-which(vte_names0 %in% c("VTE_IVC","VTE_Portal.vein", "VTE_Renal.vein"))]


# Add combined malignancies
malignancy_names <- colnames(dat2)[grep("malignancy_", colnames(dat2))]


# remove people who have Other AC
data_ac3mo <- dat2[!ac3mo %in% c("Other", "Not captured"),]
data_ac3mo$ac3mo2 <- factor(data_ac3mo$ac3mo2)
data_ac3mo$ac3mo_copay_rng <- factor(data_ac3mo$ac3mo_copay_rng)

data_ac4mo <- dat2[!ac4mo %in% c("Other", "Not captured"),]
data_ac4mo$ac4mo2 <- factor(data_ac4mo$ac4mo2)
data_ac4mo$ac4mo_copay_rng <- factor(data_ac4mo$ac4mo_copay_rng)

round_pvalues <- function(x){
  if (x < 0.001) {
    return("p < 0.001 ***")
  } else if (0.001 <= x & x < 0.01) {
    return("p < 0.01 **")
  } else if (0.01 <= x & x < 0.05) { 
    return(paste("p =", round(x, 3), "*"))
  } else return(paste("p =", round(x, 3)))
}

create_coefficients_table <- function(model, title_index){
  s <- summary(model)

  # s_coef <- round(data.matrix(s$coefficients), 3)
  s_coef_exp <- round(data.matrix(exp(s$coefficients)), 3)
  s_se <- round(data.matrix(s$standard.errors), 3)
  
  z <- s$coefficients/s$standard.errors
  # 2-tailed Wald z tests to test significance of coefficients
  p <- data.matrix(sapply((1-pnorm(abs(z), 0, 1))*2, round_pvalues))
  ci.lower <- round(exp(s$coefficients-1.96*s$standard.errors), 3)
  ci.higher <- round(exp(s$coefficients+1.96*s$standard.errors), 3)
  ci <- paste0("(", ci.lower, ", ", ci.higher, ")")

  coef_table <- t(matrix(paste0(s_coef_exp, " (", p, ")", "\\\n", ci), nrow=3))
  colnames(coef_table) <- rownames(s_coef_exp)
  rownames(coef_table) <- colnames(s_coef_exp)
  title_name1 <- paste0("Model ", title_index, ". Results of multinomial logistic regression model. \\\n Output format: \\\nExponentiated coefficient (p-value from Wald test) \\\n 95% confidence interval of the exponentiated coefficient")
  kable(coef_table, format="html", booktabs = T, caption = title_name1) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
     footnote(general = "\\*\\*\\*: p < 0.001; **: p < 0.01; *: p < 0.05")
}



## Multinomial logistic regression on AC at 3 months

### Model:  

fm.ac3mo <- as.formula(paste("ac3mo2~", paste(c(vte_names1), collapse="+"), "+ vte_history + hospitalized + ", paste(malignancy_names, collapse = "+"), "+ charlson_comorb_score + division + education2 + income_range + age_s + male + race"))
fit.ac3mo <- multinom(fm.ac3mo, data=data_ac3mo, trace=FALSE)
# create_coefficients_table(fit.ac3mo, "")




### Create forest plot ------------------------------------------------------
s <- summary(fit.ac3mo)

# obtain coefficients
s_coef_exp <- t(data.matrix(exp(s$coefficients)))
# transform into long format
covariateNames <- rownames(s_coef_exp)
responseNames <- colnames(s_coef_exp)
s_coef_exp <- data.table(s_coef_exp)
colnames(s_coef_exp) <- rep("beta", 3)
s_coef_exp <- rbind(s_coef_exp[,1], s_coef_exp[,2], s_coef_exp[,3])
s_coef_exp[, covariate := rep(covariateNames, 3)]
s_coef_exp[, response := rep(responseNames, each = length(covariateNames))]

# calculate confidence intervals
ci.lower <- data.table(t(exp(s$coefficients-1.96*s$standard.errors)))
colnames(ci.lower) <- rep("ci_lower", 3)
ci.lower <- rbind(ci.lower[,1], ci.lower[,2], ci.lower[,3])

ci.higher <- data.table(t(exp(s$coefficients+1.96*s$standard.errors)))
colnames(ci.higher) <- rep("ci_higher", 3)
ci.higher <- rbind(ci.higher[,1], ci.higher[,2], ci.higher[,3])

plot_data <- cbind(s_coef_exp, ci.lower, ci.higher)

# create y-axis values
plot_data$yAxis[plot_data$response == "Warfarin"] <- (1:length(covariateNames))*2
plot_data$yAxis[plot_data$response == "DOAC"] <- (1:length(covariateNames))*2-0.5
plot_data$yAxis[plot_data$response == "Unknown/Multiple"] <- (1:length(covariateNames))*2-1
plot_data <- plot_data[response != "(Intercept)",]

yLabels <- c("VTE: Lower Extremity DVT", "VTE: Other"                   "VTE_Pulmonary.embolism"     
             [5] "VTE_Upper.extremity.DVT"     "VTE_ivc_rv_pv")

## Plot of all categories
# ggplot(plot_data, aes(x = beta, y = yAxis)) +
#   geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
#   geom_errorbarh(aes(xmax = ci_higher, xmin = ci_lower), size = 1,
#                  height = .2, color = "gray50") +
#   geom_point(aes(size = 0.1, color = response)) +
#   # facet_grid(.~response) +
#   theme_bw() +
#   theme(panel.grid.minor = element_blank()) +
#   scale_y_continuous(breaks = (1:length(covariateNames))*2, labels = covariateNames) +
#   scale_x_continuous(breaks = seq(0,7,1)) +
#   coord_trans(x = "log10") +
#   ylab("") +
#   xlab("Odds ratio (log scale)") +
#   ggtitle("Log odds ratio and 95% confidence intervals")
# ggsave("plot_logOR.pdf")



ggplot(plot_data[response != "Unknown/Multiple",], aes(x = beta, y = yAxis)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_higher, xmin = ci_lower), size = 0.5,
                 height = 1, color = "black") +
  geom_point(aes(shape = response), size = 2, color = "black") +
  scale_shape_manual(values = c(1, 16)) + 
  # facet_grid(.~response) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = (1:length(covariateNames))*2, labels = covariateNames) +
  scale_x_continuous(breaks = seq(0,7,1)) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Odds ratio (log scale)") +
  ggtitle("Log odds ratio and 95% confidence intervals")

ggsave("plot_logOR2.pdf")










