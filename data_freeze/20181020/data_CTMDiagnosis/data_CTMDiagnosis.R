##################################################
#' Program name: data_icd9TreeStructured_list.R
#' 
#' Description: The program summarizes the tree-structured
#' ICD-9 codes for individual patients.
#' The output is a list containing medical diagnoses of each
#' patient, including code structures and counts.
#' 
#' Author: Mengbing Li
#' 
#' Created: 11/24/2018
#' 
#' Revisions: 
#' 12/06/2018 - 1. Add names of the ICD-9 codes;
#'  2. Find out diagnosis codes that are not available in the ICD-9 dictionary
##################################################

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_CTMDiagnosis")

library(dplyr)
library(tidyr)
library(data.table)
library(reshape2)
library(data.tree)
library(Matrix)
library(ggplot2)
library(tidytext)
library(tm)
library(topicmodels)
library(purrr)
library(gridExtra)


load("diagnosisCTM.RData")


### get 3-digit ICD-9 code names --------------------------
codeList <- fread("../ICD-9-CM-codes/icd9_3digitCodeList.txt",
                  colClasses = list(character = 1:2), header = TRUE)
colnames(codeList)[1] <- "term"
codeList$term <- tolower(codeList$term)


n <- 10 # top n topics

nModels <- length(diagnosisCTM) # number of models

### Document-topic probabilities --------------------------------------------
toptopics_prob <- c()
k_vector <- c()
alpha_vector <- c()
for (j in 1:nModels){
  # get the model
  CTMModel <- diagnosisCTM[[j]]
  k <- CTMModel@k
  CTM_gamma <- as.data.frame(CTMModel@gamma)
  names(CTM_gamma) <- 1:k
  
  # get the most likely topic for each patient --------------------------
  toptopics <- as.data.frame(cbind(document = row.names(CTM_gamma), 
                                   topic = apply(CTM_gamma, 1, 
                                                 function(x) names(CTM_gamma)[which(x==max(x))])))
  for(kk in 1:k){
    toptopics[, ncol(toptopics)+1] <- as.integer(grepl(kk, toptopics$topic))
    colnames(toptopics)[ncol(toptopics)] <- paste("topic", kk, sep="")
  }
  print(barplot(colSums(toptopics[,3:ncol(toptopics)]),
                main = 
                  paste("Distribution of the most likely topics of patients when k=",
                        k, sep=""),
                ylab = "Count"))
  
  k_vector <- c(k_vector, k)
  alpha_vector <- c(alpha_vector, alpha)
  
  
  # get the probability of the most likely topic ------------------------
  toptopics_prob <- cbind(toptopics_prob, apply(CTM_gamma, 1, max))
  colnames(toptopics_prob)[ncol(toptopics_prob)] <- paste("k=", k, sep="")
}

toptopics_prob <- data.table(document = 1:nrow(toptopics_prob), toptopics_prob)

# transform wide to long
toptopics_prob_long <- melt(toptopics_prob,
                            id.vars = 1, 
                            variable.name = "method", value.name = "prob")

# plot top probabilities
scale_labels <- paste0("k=", k_vector, ", alpha=", alpha)
print(ggplot(data = toptopics_prob_long, aes(x = prob)) +
        geom_histogram(aes(fill = method), alpha = 0.7, bins = 40) +
        facet_grid(method~.) +
        scale_fill_discrete(name = "Model",
                            labels = scale_labels) +
        labs(title = "Probability of assignment to the most likely topic",
             x = "Probability", y = "Count") +
        theme(text = element_text(size=10)))































tidy_ctm_gamma  <- function(CTM_object){
  CTM_object %>% 
    slot("gamma")  %>% 
    as_data_frame()  %>% 
    mutate(document = row_number()) %>% 
    gather(topic, gamma, -document) %>%
    mutate(topic = strtoi(stringr::str_sub(topic,2)))
}


lemma_tm <- diagnosisCTM %>%
  mutate(ctm_gamma = map(.x=ctm, .f=tidy_ctm_gamma))

lemma_tm %>% 
  unnest(ctm_gamma) %>% 
  group_by(k, document) %>%
  arrange(desc(gamma)) %>%
  slice(1) %>%
  #top_n(1, ctm_gamma) %>%
  ungroup() %>% 
  ggplot(aes(x=gamma, fill=factor(k))) +
  geom_histogram(bins = 15) +
  scale_fill_discrete(name = "Number of\nTopics") +
  facet_wrap(~k) +
  geom_vline(aes(xintercept = 1/k), 
             tibble(k=lemma_tm %$% unique(k)),
             color="darkred")



















