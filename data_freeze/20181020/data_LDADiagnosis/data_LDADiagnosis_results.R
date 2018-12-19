##################################################
#' Program name: data_LDADiagnosis.R
#' 
#' Description: The program summarizes the tree-structured
#' ICD-9 codes for individual patients.
#' The output is a list containing medical diagnoses of each
#' patient, including code structures and counts.
#' 
#' Author: Mengbing Li
#' 
#' Created: 12/15/2018
#' 
#' Revisions: 
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
library(purrr)
library(gridExtra)

load("diagnosisLDA.RData")

### get 3-digit ICD-9 code names --------------------------
codeList <- fread("../ICD-9-CM-codes/icd9_3digitCodeList.txt",
                  colClasses = list(character = 1:2), header = TRUE)
colnames(codeList)[1] <- "term"
codeList$term <- tolower(codeList$term)


### find the most common terms within topics ------------------------------
n <- 10 # top n topics

nModels <- length(diagnosisLDA) # number of models

pdf("data_LDADiagnosis_plots.pdf", width = 30, height = 15)
for (j in 1:nModels){
  # get the model
  ldaModel <- diagnosisLDA[[j]]
  k <- ldaModel@k
  assignedname <- paste("lda", k, "_beta", sep="")
  assign(assignedname, tidy(ldaModel, matrix = "beta"))
  
  # add icd9 names to terms
  assign(assignedname, merge(x = get(assignedname), y = codeList, 
                    by = "term", all.x = TRUE))
  
  # plot
  topTerms <- get(assignedname) %>%
    group_by(topic) %>%
    top_n(n, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(id = rep(1:n, k)) %>%
    mutate(term = reorder(term, -id)) 
  
  alpha <- ldaModel@alpha
  print(topTerms %>%
    ggplot(mapping = aes(term, beta, label = name)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    geom_bar(stat="identity", color="grey4",
      fill="salmon", position="dodge") +
    geom_text(angle=0, hjust=0.6, size = 3, nudge_x = 0.1) +
    coord_flip() +
    labs(title = 
        paste("The most common ICD-9 codes within each topic using ",
              ldaModel@call[1], " with k=", k, ", alpha=", alpha, sep="")) +
    theme(plot.title = element_text(size=28),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20)) +
    ylab("Per-topic-per-word probability") +
    xlab("ICD-9 code"))
}

# dev.off()







### Document-topic probabilities --------------------------------------------
toptopics_prob <- c()
k_vector <- c()
alpha_vector <- c()
for (j in 1:nModels){
  # get the model
  ldaModel <- diagnosisLDA[[j]]
  k <- ldaModel@k
  lda_gamma <- as.data.frame(ldaModel@gamma)
  names(lda_gamma) <- 1:k
  
  # get the most likely topic for each patient --------------------------
  toptopics <- as.data.frame(cbind(document = row.names(lda_gamma), 
      topic = apply(lda_gamma, 1, 
                    function(x) names(lda_gamma)[which(x==max(x))])))
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
  toptopics_prob <- cbind(toptopics_prob, apply(lda_gamma, 1, max))
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

dev.off()



### get the posterior distributions of icd-9 codes for sampled patients ------
nPatients <- 20

p <- list()
for (j in 1:nModels){
  # get the model
  lda_gamma <- tidy(diagnosisLDA[[j]], matrix = "gamma")
  k <- diagnosisLDA[[j]]@k
  patients_gamma <- lda_gamma[lda_gamma$document %in% 1:nPatients,]
  patients_gamma$topic <- factor(patients_gamma$topic)
  p[[j]] <- patients_gamma %>%
    ggplot(aes(document, gamma, fill = topic)) +
    geom_bar(stat = "identity", position = 'stack') +
    coord_flip() +
    labs(title = 
           paste("Posterior topic distribution for individual patients with alpha=",
                 alpha_vector[j], " and k=", k_vector[j], sep="")) +
    theme(text = element_text(size = 15)) +
    ylab("Topic probability") +
    xlab("Patient")
}

g <- do.call("grid.arrange", c(p, ncol=1))

ggsave("plot_topicDistributions.pdf", g,
       width = 30, height = 50, units = "cm")

