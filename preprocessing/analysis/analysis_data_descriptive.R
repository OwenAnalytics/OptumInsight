library(dplyr)
library(tidyr)  
library(data.table)
library(ggplot2)


mydata <- data.table(readRDS("../data/analysis_data.rds"))


### count the number of observation per patient ---------------------
observationCount <- mydata[, .N, by=patid]

png("n_observation_count.png")

par(mfrow = c(2, 1))
hist(observationCount$N, breaks=seq(from=1, to=460, length.out = 460/5),
     main="Histogram of the number of AC prescriptions of each patient",
     xlab="# prescriptions")

# enlarge the plot
hist(observationCount$N[observationCount$N>10],
     breaks=seq(from=1, to=460, length.out = 460/5),
     main="Histogram of the number of AC prescriptions (> 10) of each patient \n ",
     xlab="# prescriptions")

dev.off()


### Count the number of AC categories -------------------------------
# bar plot
ggplot(mydata, aes(x=factor(1), fill=gen_name)) +
  geom_bar(width = 0.3) +
  coord_flip() +
  scale_fill_brewer(palette = 12) +
  labs(title = "Bar plot of prescribed anticoagulants",
       y = "Count",
       x = "Anticoagulant",
       fill = "Anticoagulant name") +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
ggsave("n_ac_categories.png", width = 7, height = 4)
# unlink("n_ac_categories.png")
