##################################################
#' Program name: simulation_recurrentEvent.R
#' 
#' Description: Simulate recurrent event data and
#' test functions in frailtypack and survival
#' packages.
#' 
#' Author: Mengbing Li
#' 
#' Created: 11/14/2018
#' 
##################################################

library(reda)
library(frailtypack)
library(data.table)

set.seed(2015)
## recurrent events from two processes
## with different time-invariant covariates and time origins
n <- 100

mydata <- simEventData(z = cbind(rnorm(n), rbinom(n, 1, 0.5)),
             zCoef = c(1, - 0.5), recurrent = TRUE,
             rho = 0.2, origin = 0, endTime = rgamma(n, 10, 1))

## convert data into counting process format
# transform from long to wide
mydata <- data.table(mydata)
mydata[, tstart := lag(time), by = ID]
mydata[, tstart := ifelse(is.na(tstart), 0, tstart)]
mydata[, tend := time]


coxph(Surv(time, event) ~ cluster(ID) + X.1 + X.2, mydata)


mod.coxAG <- frailtyPenal(
  Surv(tstart, tend, event) ~
    cluster(ID) + X.1 + X.2,
  n.knots = 10, kappa = 1, recurrentAG = TRUE,
  data = mydata, cross.validation = TRUE)
# estimated coefficients: beta1=0.995, beta2=-0.566






