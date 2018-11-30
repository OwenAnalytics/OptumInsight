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


## Multinomial logistic regression on AC at 3 months

### Model:  
fm.ac3mo <- as.formula(paste("ac3mo2~", paste(c(vte_names1), collapse="+"), "+ vte_history + hospitalized + ", paste(malignancy_names, collapse = "+"), "+ charlson_comorb_score + division + education2 + income_range + age_s + male + race"))
fit.ac3mo <- multinom(fm.ac3mo, data=data_ac3mo, trace=FALSE)



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

# specify row order
vte_names <- c("VTE_Pulmonary.embolism", "VTE_Lower.extremity.DVT",
               "VTE_Upper.extremity.DVT", "VTE_ivc_rv_pv", "VTE_Other")
malignancy_names <- grep("malignancy_", covariateNames, value = TRUE)
division_names <- c("divisionEAST SOUTH CENTRAL", "divisionMIDDLE ATLANTIC",
  "divisionMOUNTAIN", "divisionNEW ENGLAND", "divisionPACIFIC", "divisionSOUTH ATLANTIC",
  "divisionWEST NORTH CENTRAL", "divisionWEST SOUTH CENTRAL", "divisionUNKNOWN")
education_names <- grep("education", covariateNames, value = TRUE)
income_names <- paste("income_range", c(1:5,0), sep = "")
race_names <- grep("race", covariateNames, value = TRUE)
other_names <- c("vte_history", "hospitalizedTRUE", "charlson_comorb_score",
  "age_s", "male")
covariateNames_ordered <- c("(Intercept)", vte_names, malignancy_names, division_names,
  education_names, income_names, race_names, other_names)

response_ordered <- c("DOAC", "Warfarin", "Unknown/Multiple")

plot_data <- data.table(plot_data %>%
  arrange(match(covariate, covariateNames_ordered), 
          match(response, response_ordered)))

plot_data_sub <- plot_data[covariate != "(Intercept)" & response != "Unknown/Multiple",]


# create y-axis values
plot_data_sub$yAxis[plot_data_sub$covariate %in% other_names] <- 
  c(rbind(1:length(other_names)-0.25, 1:length(other_names)))

ymax <- max(plot_data_sub$yAxis, na.rm = TRUE)
plot_data_sub$yAxis[plot_data_sub$covariate %in% race_names] <- 
  c(rbind(length(race_names):1-0.25, length(race_names):1)) + ymax + 2

ymax <- max(plot_data_sub$yAxis, na.rm = TRUE)
plot_data_sub$yAxis[plot_data_sub$covariate %in% income_names] <- 
  c(rbind(length(income_names):1-0.25, length(income_names):1)) + ymax + 2

ymax <- max(plot_data_sub$yAxis, na.rm = TRUE)
plot_data_sub$yAxis[plot_data_sub$covariate %in% education_names] <- 
  c(rbind(length(education_names):1-0.25, length(education_names):1)) + ymax + 2

ymax <- max(plot_data_sub$yAxis, na.rm = TRUE)
plot_data_sub$yAxis[plot_data_sub$covariate %in% division_names] <- 
  c(rbind(length(division_names):1-0.25, length(division_names):1)) + ymax + 2

ymax <- max(plot_data_sub$yAxis, na.rm = TRUE)
plot_data_sub$yAxis[plot_data_sub$covariate %in% malignancy_names] <- 
  c(rbind(length(malignancy_names):1-0.25, length(malignancy_names):1)) + ymax + 2

ymax <- max(plot_data_sub$yAxis, na.rm = TRUE)
plot_data_sub$yAxis[plot_data_sub$covariate %in% vte_names] <- 
  c(rbind(length(vte_names):1-0.25, length(vte_names):1)) + ymax + 2




# plot_data_sub$yAxis <- 80 - plot_data_sub$yAxis


vte_labels <- c("VTE --- Pulmonary embolism", "Lower extremity DVT",
    "Upper extremity DVT", "IVC/RV/PV", "Other")
malignancy_labels <- c("Malignancy --- Brain CNS", "Breast", "Gastrointestinal",
    "Genitourinary", "Gynecololgic", "Hematologic", "Lung", "Other")
division_labels <- c("Division (compared to East North Central) --- East South Central", 
    "Middle Atlantic",
    "Mountain", "New England", "Pacific", "South Atlantic", "West North Central",
    "West South Central", "Unknown")
education_labels <- c("Education (compared to < Bachelor Degree) --- <= high school diploma",
    ">= Bachelor Degree", "Unknown")
income_labels <- c("Household Income (compared to $100K+) --- <$40K", "$40K-$49K",
    "$50K-$59K", "$60K-$74K", "$75K-$99K", "Unknown")
race_labels <- c("Race (compared to White) --- Asian", "Black", "Hispanic", "Unknown")
other_labels <- c("VTE history", "Hospitalization", "Charlson comorbidity score",
    "Age (normalized)", "Male")
covariate_labels <- c(vte_labels, malignancy_labels, division_labels,
    education_labels, income_labels, race_labels, other_labels)

n_vars <- length(covariateNames_ordered) - 1 # intercept was removed

# create y axis breaks
yAxisBreaks <- plot_data_sub$yAxis[(1:n_vars)*2]

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



ggplot(plot_data_sub, aes(x = beta, y = yAxis)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_higher, xmin = ci_lower), size = 0.5,
                 height = 0.7, color = "black") +
  geom_point(aes(shape = response), size = 2, color = "black") +
  scale_shape_manual(values = c(1, 16), name = "Anticoagulant") + 
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxisBreaks, labels = covariate_labels) +
  scale_x_continuous(breaks = seq(0,7,1)) +
  coord_trans(x = "log10") +
  ylab("") +
  xlab("Odds ratio (log10 scale)") +
  ggtitle("Odds ratios of anticoagulants compared to LMWH and 95% confidence intervals")

ggsave("plot_OR_log10Scale.png")


ggplot(plot_data_sub, aes(x = beta, y = yAxis)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_higher, xmin = ci_lower), size = 0.5,
                 height = 0.7, color = "black") +
  geom_point(aes(shape = response), size = 2, color = "black") +
  scale_shape_manual(values = c(1, 16), name = "Anticoagulant") + 
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = yAxisBreaks, labels = covariate_labels) +
  scale_x_continuous(breaks = seq(0,7,1)) +
  ylab("") +
  xlab("Odds ratio") +
  ggtitle("Odds ratios of anticoagulants compared to LMWH and 95% confidence intervals")

ggsave("plot_OR_originalScale.png")








