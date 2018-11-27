##################################################
#' Program name: marked_point_process_PtProcess.R
#' 
#' Description: This program tests modeling of marked point
#' processes using package PtProcess. 
#' Model: lambda(x, t | Ht) = lambda(t|Ht) f(x|Ht)
#' Estimation method: MLE
#' 
#' Author: Mengbing Li
#' 
#' Created: 10/06/2018
#' 
#' Revisions: 11/20/2018 - Run marked point process
#'  on admission data of one patient
##################################################

library(ggplot2)
library(PtProcess)
library(data.table)

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_markedPointProcess")

# fit a model to the Phuket earthquake data --------------------------
# adopted from the JSS paper -----------------------------------------

data(Tangshan)

#   increment magnitudes a fraction so none are zero
Tangshan[,"magnitude"] <- Tangshan[,"magnitude"] + 0.01


#' x is a data.frame of mark values and times of specific events,
#' generally a subset of the history.
#' Mark density functions must return a vector with length being
#' equal to the number of rows in x. Each element contains the 
#' logarithm of the joint density of the marks corresponding to
#' each event time (row) in x.
dmagn_mark <- function(x, data, params){
  #  Gamma distribution
  #  exponential density when params[7]=0
  lambda <- etas_gif(data, x[,"time"], params=params[1:5])
  y <- dgamma(x[,"magnitude"], shape=1+sqrt(lambda)*params[7],
              rate=params[6], log=TRUE)
  return(y)
}

# time interval
TT <- c(0, 4018)
# params[6:7] are the parameters for Gamma distribution
params <- c(0.007, 1.1, 1.08, 0.02, 0.95, 1.92, 0.47)

#' gmap is an expression that maps the model parameters
#' (params) into the parameter sub-space of the ground 
#' intensity function
x <- mpp(data=Tangshan,
         gif=etas_gif,
         marks=list(dmagn_mark, NULL),
         params=params,
         gmap=expression(params[1:5]),
         mmap=expression(params[1:7]),
         TT=TT)

allmap <- function(y, p){
  #    one to one mapping, all p positive
  y$params <- exp(p)
  return(y)
}


#    Parameters must be positive. Transformed so that nlm
#    can use entire real line (no boundary problems, see
#    topic "neglogLik" for further explanation).
#    Argument "iterlim" has been restricted to 2 to avoid
#    excessive time in package checks, set much larger to
#    ensure convergence.

#' pmap: a user provided function mapping the revised 
#' parameter values params into the appropriate locations
#' in object
z <- nlm(neglogLik, log(params), object=x, pmap=allmap,
         print.level=1, iterlim=100, typsize=abs(params))

x1 <- allmap(x, z$estimate)

#    print parameter estimates
print(x1$params)

print(logLik(x))
print(logLik(x1))
plot(x1, log=TRUE)







### Run the model on admissions of one patient ------------------------------

confinement <- fread("../separateData/confinementClaims.txt",
                     colClasses = list(character = 1:17))
colnames(confinement) <- tolower(colnames(confinement))

confinement$index_dt <- as.Date(confinement$index_dt)
confinement$admit_dt <- as.Date(confinement$admit_dt)
confinement$disch_dt <- as.Date(confinement$disch_dt)
confinement$pos <- as.factor(confinement$pos)
confinement$charge <- as.numeric(confinement$charge)
confinement$los <- as.numeric(confinement$los)


# calculate the number of admissions of each patient
confinement[, admissionCount := .N, by=patid]

# read in baseline covariates
patinfo <- fread("../baseline_20181020_freeze.csv",
                 colClasses = list(character = 1:90))
baseline <- unique(patinfo[, .(patid, male, age, race)])
confinement <- merge(x = confinement,
                     y = baseline,
                     by = "patid",
                     all.x = TRUE)

# get the patient who has the most admissions
mydata <- confinement[admissionCount == max(admissionCount), ]


## distribution of admission times
firstAdmissionDate <- min(mydata$admit_dt)
mydata[, daysAfterFirstAdmissions := as.numeric(admit_dt - firstAdmissionDate)]
# generate position on the y axis
mydata <- mydata[order(patid, admit_dt, disch_dt),]
mydata[, plot_y := seq_len(.N)*0.2]


# plot admissions and lengths of stay -------------------------
plot_admissions <- function(data){
  ggplot(data = data,
         aes(x = daysAfterFirstAdmissions, y = plot_y)) +
    geom_point(alpha = 0.5, size = 0.5) +
    geom_segment(aes(x = daysAfterFirstAdmissions, y = plot_y,
                     xend = daysAfterFirstAdmissions+los, yend = plot_y,
                     colour = "salmon"), size = 1) +
    geom_vline(aes(xintercept = as.numeric(index_dt - firstAdmissionDate),
                   linetype="Index VTE Date")) +
    labs(title="Patient 802666560657011. Admissions and length of stay
         Black dots = Admissions, line segments = lengths of stay",
         x="Days after the first admission",
         y="") + 
    theme(axis.ticks.y=element_blank(), 
          axis.text.y=element_blank(),
          panel.background = element_rect(fill = "grey92"))
}

plot_admissions(mydata)
ggsave("admissionsAndLOS_all.pdf")
plot_admissions(mydata[daysAfterFirstAdmissions < 300])
ggsave("admissionsAndLOS_period1.pdf")
plot_admissions(mydata[daysAfterFirstAdmissions > 450 &
                         daysAfterFirstAdmissions < 650])
ggsave("admissionsAndLOS_period2.pdf")
plot_admissions(mydata[daysAfterFirstAdmissions >= 650 &
         daysAfterFirstAdmissions < 2000])
ggsave("admissionsAndLOS_period3.pdf")


# distribution of marks ------------------------------------
ggplot(data = mydata, aes(x = los), stat = "bin") + 
  geom_histogram(bins = 100) +
  labs(title="Patient 802666560657011. Lengths of stay",
       x="Days",
       y="Count") 
ggsave("LOS_histogram.pdf")


### want to find the MLE of parameters of the ground intensity -----------------

# log-likelihood function
# intensity function:
# lambda(t) = mu + \int_0^t alpha * exp(-beta*(t-s)) dN(s)
negloglik <- function(params, times){
  n <- length(times)
  times <- times[2:n]
  mu <- exp(params[1]) # immigration intensity
  alpha <- exp(params[2]) # branching ratio
  beta <- exp(params[3]) # parameter of exponential distribution
  
  Ai <- c(0, sapply(c(2:(n-1)), function(z) {
    exp(-beta*times[z]) * sum( exp(-beta*times[1:(z-1)]) )
    # sum(exp( -beta * (times[z]- times[1:(z-1)])))
  }))
  s1 <- sum(log( mu + alpha * Ai))

  s2 <- -mu*times[n-1] # mu(t) = -mu * t
  s3 <- sum(alpha/beta*(exp( -beta * (times[n-1] - times)) - 1))
  return(-(s1+s2+s3)) # return negative loglikelihood
}

test <- negloglik(c(1, 2, 3),
  mydata$daysAfterFirstAdmissions / 30) # convert to month to avoid underflow

# convert to month to avoid underflow
timesInMonth <- mydata$daysAfterFirstAdmissions / 30
solution <- nlm(negloglik, c(1, 2, 3), hessian = TRUE,
                times = timesInMonth)

paste( c("mu", "alpha", "beta" ), round(solution$estimate,2), sep=" = ")
# "mu = 0.48"     "alpha = -6.17" "beta = 11.17" 
### alpha and beta are unstable
### TOO BAD

(solution2 <- optim(par = c(1, 2, 3),
                    negloglik, NULL,
                    times = mydata$daysAfterFirstAdmissions / 30))
paste( c("mu", "alpha", "beta" ), round(solution2$par,2), sep=" = ")
# [1] "mu = 0.48"     "alpha = -2.23" "beta = 15.31"
### alpha and beta are unstable
### TOO BAD



### ground intensity function -----------------------------------------
groundIntensity <- function(params){
  n <- length(timesInMonth)
  times <- timesInMonth[2:n]
  mu <- params[1] # immigration intensity
  # eta <- params[2] # branching ratio
  alpha <- params[2] # branching ratio
  beta <- params[3] # parameter of exponential distribution
  
  Ai <- c(0, sapply(c(2:(n-1)), function(z) {
    exp(-beta*times[z]) * sum( exp(-beta*times[1:(z-1)]) )
  }))
  lambda <- mu + alpha * Ai
  return(lambda)
}




## consider gamma distribution for marks -----------------------

#' x is a data.frame of mark values and times of specific events,
#' generally a subset of the history.
#' Mark density functions must return a vector with length being
#' equal to the number of rows in x. Each element contains the 
#' logarithm of the joint density of the marks corresponding to
#' each event time (row) in x.
densityMark <- function(x, data, params){
  #  Gamma distribution
  #  exponential density when params[7]=0
  lambda <- etas_gif(data, 
                     x[,"daysAfterFirstAdmissions"],
                     params=params[1:3])
  y <- dgamma(x[,"los"], shape=1+sqrt(lambda)*params[4],
              rate=params[5], log=TRUE)
  return(y)
}


# time interval
TT <- c(0, 2500)
# params[6:7] are the parameters for Gamma distribution
params <- c(1, 2, 3, 4, 5)

#' gmap is an expression that maps the model parameters
#'  (params) into the parameter sub-space of the ground 
#'  intensity function
#' mmap maps the model parameters (params) into the 
#'  parameter sub-space of the mark distribution
x <- mpp(data=mydata,
         gif=groundIntensity,
         marks=list(densityMark, NULL),
         params=params,
         gmap=expression(params[1:3]),
         mmap=expression(params[1:5]),
         TT=TT)

allmap <- function(y, p){
  #    one to one mapping, all p positive
  y$params <- exp(p)
  return(y)
}


#    Parameters must be positive. Transformed so that nlm
#    can use entire real line (no boundary problems, see
#    topic "neglogLik" for further explanation).
#    Argument "iterlim" has been restricted to 2 to avoid
#    excessive time in package checks, set much larger to
#    ensure convergence.

#' pmap: a user provided function mapping the revised 
#' parameter values params into the appropriate locations
#' in object
z <- nlm(neglogLik, params, object=x, pmap=allmap,
         print.level=1, iterlim=100, typsize=abs(params))

x1 <- allmap(x, z$estimate)

#    print parameter estimates
print(x1$params)

print(logLik(x))
print(logLik(x1))
plot(x1, log=TRUE)



