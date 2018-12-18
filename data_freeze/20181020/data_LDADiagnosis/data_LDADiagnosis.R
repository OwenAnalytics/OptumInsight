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

setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20181020/data_LDADiagnosis")

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

load("diagnosisInformation_list.RData")

### Create a document-term matrix -----------------------------------------
# create a data frame to store the diagnosis

diagnosisText <- c()
for(patient in unique(diagnosisInformation$patients)){
  patientIndex <- which(diagnosisInformation$patients == patient)
  diagnoses <- paste(diagnosisInformation$roots[patientIndex],
                     sep = " ", collapse = " ")
  diagnosisText <- c(diagnosisText, diagnoses)
}
diagnosisText <- as.data.frame(diagnosisText)
colnames(diagnosisText) <- "diagnoses"
diagnosisTextCorpus <- Corpus(VectorSource(diagnosisText$diagnoses)) 
diagnosisText_dtm <- DocumentTermMatrix(diagnosisTextCorpus)




### Run the LDA model -----------------------------------------------------
para <- tibble(k = c(2,3,9,12,50))

diagnosis_LDA <- para %>%
  mutate(lda = map(k, 
                 function(k) LDA(
                   k=k, 
                   x=dtm_lemma, 
                   method="Gibbs", 
                   control=control_list_gibbs)))


k <- 10
SEED <- 2010

diagnosis_TM <- list(
  VEM = LDA(diagnosisText_dtm, k = k, control = list(seed = SEED)),
  Gibbs = LDA(diagnosisText_dtm, k = k, method = "Gibbs",
              control = list(seed = SEED, burnin = 1000,
                             thin = 100, iter = 1000)))

rm(list=setdiff(ls(), c("diagnosis_TM", "diagnosisText_dtm")))
save(file="diagnosis_TM.RData")



### get 3-digit ICD-9 code names --------------------------
codeList <- fread("../ICD-9-CM-codes/icd9_3digitCodeList.txt",
                  colClasses = list(character = 1:2), header = TRUE)
colnames(codeList)[1] <- "term"


### find the most common terms within topics ------------------------------
load("diagnosis_TM.RData")
diagnosis_Gibbs <- tidy(diagnosis_TM[["Gibbs"]], matrix = "beta")

# add icd9 names to terms
diagnosis_Gibbs$term <- toupper(diagnosis_Gibbs$term)
diagnosis_Gibbs <- merge(x = diagnosis_Gibbs,
                         y = codeList,
                         by = "term", all.x = TRUE)

# get the top 10 terms
n <- 10
top_terms_Gibbs <- diagnosis_Gibbs %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(id = rep(1:n, k)) %>%
  mutate(term = reorder(term, -id)) 

top_terms_Gibbs %>%
  ggplot(mapping = aes(term, beta, label = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  geom_bar(
    stat="identity",
    color="grey4",
    fill="salmon",
    position="dodge"
  ) +
  geom_text(angle=0, hjust=0.6, size = 3, nudge_x = 0.1) +
  coord_flip() +
  labs(title = 
      paste("The most common ICD-9 codes within each topic using Gibbs estimation with alpha=", diagnosis_TM$Gibbs@alpha, sep="")) +
  theme(plot.title = element_text(size=28),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20)) +
  ylab("Per-topic-per-word probability") +
  xlab("ICD-9 code")

ggsave("k10_TopWordTopicProbability_Gibbs.pdf",
       width = 60, height = 30, units = "cm")




### Document-topic probabilities --------------------------------------------

## Gibbs
patients_Gibbs <- tidy(diagnosis_TM[["Gibbs"]], matrix = "gamma")

# extract estimated posterior distributions of icd-9 codes for 20 patients
patients_Gibbs_20 <- patients_Gibbs[patients_Gibbs$document == 1,]

for(doc in 2:20) {
  patients_Gibbs_20 <- 
    rbind(patients_Gibbs_20, 
          patients_Gibbs[patients_Gibbs$document == doc,])
}
patients_Gibbs_20$topic <- factor(patients_Gibbs_20$topic)

patients_Gibbs_20 %>%
  ggplot(aes(document, gamma, fill = topic)) +
  geom_bar(stat = "identity", position = 'stack') +
  coord_flip() +
  labs(title = paste("The most common ICD-9 codes within each topic
    using Gibbs estimation with alpha=", diagnosis_TM$Gibbs@alpha, sep="")) +
  theme(text = element_text(size = 15)) +
  ylab("Per-patient-per-topic probability") +
  xlab("Patient")
ggsave("k10_DocumentTopic_Gibbs.pdf",
       width = 40, height = 20, units = "cm")



### The most likely topic assignment of each document -----------------------
gammaDF <- as.data.frame(diagnosis_TM$Gibbs@gamma) 
names(gammaDF) <- c(1:k)

# inspect...
toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
            topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))

toptopics_prob <- lapply(patients_Gibbs, function(x) apply(x, 1, max))
toptopics_prob <- do.call("cbind", toptopics_prob)
toptopics_prob <- data.table(document = row.names(gammaDF), toptopics_prob)
toptopics_prob[, 2:6] <- lapply(toptopics_prob[, 2:6], as.numeric)

# transform wide to long
toptopics_prob_long <- melt(toptopics_prob,
                            id.vars = 1, measure.vars = 2:6,
                            variable.name = "method", value.name = "prob")
# plot top probabilities
ggplot(data = toptopics_prob_long, aes(x = prob)) +
  geom_histogram(aes(fill = method), alpha = 0.7, bins = 40) +
  facet_grid(method~.) +
  scale_fill_discrete(name="Method",
                      labels=c("VEM: alpha = 0.022",
                               "Gibbs: alpha = 1.67", 
                               "Gibbs: alpha = 0.8",
                               "Gibbs: alpha = 0.5",
                               "Gibbs: alpha = 0.1")) +
  labs(title = "Probability of assignment to the most likely topic",
       x = "Probability", y = "Count") +
  theme(text = element_text(size=10))
# plot.title = element_text(size=18),
# axis.title.x = element_text(size=15),
# axis.title.y = element_text(size=15),
# legend.title=element_text(size=18),
# legend.text= element_text(size=18)
ggsave("slides/prob_assignment_to_top_topics_histogram.pdf",
       width = 6, height = 7)






### Correlated topic model --------------------------------------------------
k <- 10
SEED <- 2018
diagnosis_ctm <- CTM(diagnosisText_dtm, k = k,
          control = list(seed = SEED,
            var = list(tol = 10^-4), em = list(tol = 10^-3)))


## Extract the gamma matrix containing the topic probabilities per document
tidy_ctm_gamma  <- function(CTM_object){
  CTM_object %>% 
    slot("gamma")  %>% 
    as_data_frame()  %>% 
    mutate (document = row_number()) %>% 
    gather(topic, gamma, -document) %>%
    mutate(topic = strtoi(stringr::str_sub(topic,2)))
}























