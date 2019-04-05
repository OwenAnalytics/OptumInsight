##################################################
#' Program name: stm_example.R
#' 
#' Description: Run an example of structural topic model from 
#' the stm package.
#' Reference: https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf
#' 
#' Author: Mengbing Li
#' 
#' Created: 03/10/2019
#' 
#' Revisions: 
##################################################


# setwd("C:/Users/mengbing/Box Sync/OptumInsight_DataManagement/data_freeze/20190110/data_word2vec")
setwd("C:/Users/mengbing/Box Sync/research/code_library/stm_package")

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(stm)
library(wordcloud)

# we will use a collection of blogposts about American
# politics that were written in 2008, from the CMU 2008 Political Blog Corpus



# 1.  Reading and processing text data ------------------------------------------

data <- read.csv("poliblogs2008.csv")

#' textProcessor: Function that takes in a vector of raw texts and performs 
#' basic operations. This function is essentially a wrapper tm package 
#' where various user specified options can be selected.
processed <- textProcessor(data$documents, metadata = data)



# 2. Prepare: Associating text with metadata ------------------------------------

#' prepDocuments: Performs several corpus manipulations including removing 
#' words and renumbering word indices (to correct for zero-indexing and/or
#'  unusued words in the vocab vector). Also removes infrequent terms 
#'  depending on the user-set parameter lower.thresh
out <- prepDocuments(processed$documents, processed$vocab, processed$meta,
                     lower.thresh = 15)

#' plotRemoved: plot the number of words and documents removed for 
#' different thresholds.
plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
out <- prepDocuments(processed$documents, processed$vocab,
                     processed$meta, lower.thresh = 15)

# docs <- out$documents
# vocab <- out$vocab
# meta <-out$meta




# 3. Estimate: Estimating the structural topic model ----------------------------

#' let prevalence be a function of the "rating" variable, which is coded 
#' as either "Liberal" or "Conservative", and the variable "day". Here 
#' we allow for the variable "date" to be estimated with a spline
poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab,
                       K = 20, prevalence =~ rating + s(day),
                       max.em.its = 75, data = out$meta,
                       init.type = "Spectral")

# to avoid running the model for too long
load(url("http://goo.gl/VPdxlS"))


#' selectModel first casts a net where "run" (below 10) models are run for
#'  two EM steps, and then models with low likelihoods are discarded. Next,
#'  the default returns the 20% of models with the highest likelihoods, 
#'  which are then run until convergence or the EM iteration maximum is 
#'  reached.
poliblogSelect <- selectModel(out$documents, out$vocab, K = 20,
                              prevalence =~ rating + s(day), max.em.its = 75,
                              data = out$meta, runs = 3, seed = 8458159)

selectedmodel <- poliblogSelect$runout[[3]]

#' plotModels: calculates the average across all topics for each run 
#' of the model and plots these by labeling the model run with a numeral. 
plotModels(poliblogSelect, pch=c(1,2,3,4), legend.position="bottomright")


#' searchK:  choose the number of topics including calculating the held 
#' out likelihood and performing a residual analysis
storage <- searchK(out$documents, out$vocab, K = c(7, 10),
                   prevalence =~ rating + s(day), data = meta)




### 4. Understand: Interpreting the STM by plotting and inspecting result --------

#' 1. Displaying words associated with topics (labelTopics, 
#' plot.STM(,type = "labels"), sageLabels, plot.STM(,type = "perspectives"))
#'  or documents highly associated with particular topics (findThoughts,
#'   plotQuote).
#' 2. Estimating relationships between metadata and topics/topical content
#'  (estimateEffect).
#' 3. Calculating topic correlations (topicCorr).


#' labelTopics: To explore the words associated with each topic
labelTopics(poliblogPrevFit, c(3, 7, 20))



#' findThoughts: To examine documents that are highly associated with topics
thoughts3 <- findThoughts(poliblogPrevFit, texts = shortdoc,
                          n = 2, topics = 3)$docs[[1]]
thoughts20 <- findThoughts(poliblogPrevFit, texts = shortdoc,
                          n = 2, topics = 20)$docs[[1]]

# Example documents highly associated with topics 3 and 20:
par(mfrow = c(1, 2),mar = c(.5, .5, 1, .5))
plotQuote(thoughts3, width = 30, main = "Topic 3")
plotQuote(thoughts20, width = 30, main = "Topic 20")



#' estimateEffect: extract the relationships and associated uncertainty 
#' on all K topics. It simulates a set of parameters which can 
#' then be plotted
out$meta$rating <- as.factor(out$meta$rating)
prep <- estimateEffect(1:20 ~ rating + s(day), poliblogPrevFit,
                      meta = out$meta, uncertainty = "Global")
summary(prep, topics=3)






### 5. Visualize: Presenting STM results ------------------------------------

### 5.(1) Metadata/topic relationship visualization -------------------------

#' expected proportion of the corpus that belongs to each topic:
par(mfrow = c(1, 1), mar = c(3,2,2,2))
plot(poliblogPrevFit, type = "summary", xlim = c(0, .3))


#' When the covariate of interest is binary, or users are interested in a 
#' particular contrast, the method = "difference" option will plot the 
#' change in topic proportion shifting from one specific value to another.
#' Here we plot the difference between being liberal and being conservative,
#' which are two values of the categorical variable rating
plot(prep, covariate = "rating", topics = c(3, 7, 20),
     model = poliblogPrevFit, method = "difference",
     cov.value1 = "Liberal", cov.value2 = "Conservative",
     xlab = "More Conservative ... More Liberal",
     main = "Effect of Liberal vs. Conservative",
     xlim = c(-.1, .1), labeltype = "custom",
     custom.labels = c('Obama', 'Sarah Palin','Bush Presidency'))


#' plot the effect of day, which is a continuous variable, on topics
plot(prep, "day", method = "continuous", topics = 7,
     model = z, printlegend = FALSE, xaxt = "n", xlab = "Time (2008)")
monthseq <- seq(from = as.Date("2008-01-01"),
                to = as.Date("2008-12-01"), by = "month")
monthnames <- months(monthseq)
axis(1,at = as.numeric(monthseq) - min(as.numeric(monthseq)),
    labels = monthnames)



### 5.(2) Topical content -----------------------------

#' A topical content variable allows  for the vocabulary used to talk 
#' about a particular topic to vary. First, the STM must be fit with 
#' a variable specified in the content option. In the below example, 
#' ratings serves this purpose.
poliblogContent <- stm(out$documents, out$vocab, K = 20,
                       prevalence =~ rating + s(day), content =~ rating,
                       max.em.its = 75, data = out$meta, init.type = "Spectral")


#' plot the influence of a topical content covariate. It shows which 
#' words within a topic are more associated with one covariate value
#' versus another.
plot(poliblogContent, type = "perspectives", topics = 11)

#' plot the contrast in words across two topics:
#' topic 12 (Iraq war) and 20 (Bush presidency). 
#' This plot calculates the difference in probability of a word for 
#' the two topics, normalized by the maximum difference in probability 
#' of any word between the two topics.
plot(poliblogPrevFit, type = "perspectives", topics = c(12, 20))


### 5.(3) Plotting covariate interactions -------------------

#' we re-estimated the STM to allow for an interaction between day 
#' (entered linearly) and ratings.
poliblogInteraction <- stm(out$documents, out$vocab, K = 20,
                           prevalence =~ rating * day, max.em.its = 75,
                           data = out$meta, init.type = "Spectral")

#' Plot the influence of interactions
#' Were other variables included in the model, they would be held 
#' at their sample medians.
prep <- estimateEffect(c(20) ~ rating * day, poliblogInteraction,
                       metadata = out$meta, uncertainty = "None")
plot(prep, covariate = "day", model = poliblogInteraction,
    method = "continuous", xlab = "Days", moderator = "rating",
    moderator.value = "Liberal", linecol = "blue", ylim = c(0, .12),
    printlegend = F)
plot(prep, covariate = "day", model = poliblogInteraction,
    method = "continuous", xlab = "Days", moderator = "rating",
    moderator.value = "Conservative", linecol = "red", add = T,
    printlegend = F)
legend(0, .08, c("Liberal", "Conservative"),
    lwd = 2, col = c("blue", "red"))



### 5.(4) Other visualization tools

#' Correlation between topics
mod.out.corr <- topicCorr(poliblogPrevFit)
plot(mod.out.corr)

#' Word cloud display of vice President topic
cloud(poliblogPrevFit, topic = 7, scale = c(2,1))




# 6. Convergence criterion ------------------------------------------------

#' Denoting by l_t the approximate variational objective at time t, 
#' convergence is declared when the quantity 
#' (l_t - 1 - l_{t-1}) / abs(l_{t-1})
#' drops below tolerance. The default tolerance is 1e-5 and can be 
#' changed using the emtol argument.
plot(poliblogPrevFit$convergence$bound, type = "l",
     ylab = "Approximate Objective",
     main = "Convergence") 
