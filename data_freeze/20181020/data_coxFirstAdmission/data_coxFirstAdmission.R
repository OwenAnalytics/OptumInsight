##################################################
#' Program name: data_coxFirstAdmission.R
#' 
#' Description: This program runs Cox model on the
#' first hospital admission.
#' 
#' Author: Mengbing Li
#' 
#' Created: 11/11/2018
#' 
##################################################

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(survival)
library(frailtypack)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_coxFirstAdmission")


# read in confinement data ------------------------------------------------

diagData <- fread("../diagData_20181020_freeze.csv",
                  colClasses = list(character = 1:73))

# obtain admission data
confinement <- diagData[source == "confinement.inpatient",
                        .(patid, index_dt, conf_id, fst_dt, lst_dt, 
                          charge, copay, pos, description, category)]
rm(diagData)


# calculate length of stay
confinement$index_dt <- as.Date(confinement$index_dt)
confinement$fst_dt <- as.Date(confinement$fst_dt)
confinement$lst_dt <- as.Date(confinement$lst_dt)
confinement$los <- as.numeric(confinement$lst_dt - confinement$fst_dt) + 1
confinement$pos <- as.factor(confinement$pos)
confinement$charge <- as.numeric(confinement$charge)

confinementAfterVTE <- confinement[fst_dt >= index_dt,]

confinementAfterVTE$admit_daysAfterVTE <- 
  as.numeric(confinementAfterVTE$fst_dt - confinementAfterVTE$index_dt)
confinementAfterVTE$disch_daysAfterVTE <- 
  as.numeric(confinementAfterVTE$lst_dt - confinementAfterVTE$index_dt)
confinementAfterVTE$admit <- 1

# add baseline covariates
baseline <- fread("../baseline_20181020_freeze.csv")
baseline_vars <- unique(baseline[, .(patid, male, age, race)])
confinementAfterVTE <- merge(x = confinementAfterVTE,
                             y = baseline_vars,
                             by = "patid",
                             all.x = TRUE)

# get the first admission only
confinementAfterVTE_firstAdmit <-
  confinementAfterVTE[
    confinementAfterVTE[,
      .I[admit_daysAfterVTE == min(admit_daysAfterVTE)], by = patid]$V1]



# coxph(Surv(admit_daysAfterVTE, admit) ~
#         los + charge + cluster(patid),
#       confinementAfterVTE_firstAdmit)

cox.model <- coxph(Surv(admit_daysAfterVTE, admit) ~
        los + age + male + charge + cluster(patid),
      confinementAfterVTE_firstAdmit)
cox.ph <- cox.zph(cox.model)
cox.ph
# plot(cox.ph)


### Using frailtypack to fit recurrent admissions ---------------------------
library(frailtypack)

# to prevent start time = stop time
confinementAfterVTE$admit_daysAfterVTE2 <- 
  ifelse(confinementAfterVTE$admit_daysAfterVTE == 0,
         0.1,
         confinementAfterVTE$admit_daysAfterVTE)

confinementAfterVTE$disch_daysAfterVTE2 <- 
  ifelse(confinementAfterVTE$admit_daysAfterVTE == confinementAfterVTE$disch_daysAfterVTE,
         confinementAfterVTE$disch_daysAfterVTE+0.1,
         confinementAfterVTE$disch_daysAfterVTE)

set.seed(2018)
patids <- unique(confinementAfterVTE$patid)
subid <- sample(patids, 10, replace = FALSE)
subdata <- confinementAfterVTE[patid %in% subid,]

#' Shared frailty model:
#' lambda_ij(t|v_i) = v_i * lambda_0(t) exp(beta^top X_ij) = v_i lambda_ij(t)
#' where v_i iid~ Gamma(1/theta, 1/theta)
subdata$relative_event_time <- subdata$disch_daysAfterVTE-subdata$admit_daysAfterVTE
mod.coxAG <- frailtyPenal(
  Surv(relative_event_time, admit) ~
    cluster(patid) + age,
  n.knots = 10,kappa=1,
  data = subdata)


additivePenal(Surv(admit_daysAfterVTE, admit) ~
                cluster(patid) + age + charge + slope(los) + los,
              data = subdata, correlation = FALSE, n.knots = 10,
              cross.validation = FALSE, kappa = 1, maxit = 350)






