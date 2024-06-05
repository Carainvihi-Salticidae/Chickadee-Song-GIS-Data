


rm(list = ls()) # clear the R memory

library(ggplot2)
library(dplyr)
library(plyr)
library(psych)
library(sjstats)
library(tidyverse)
library(ggfortify)
library(pwr)
library(summarytools)
library(emmeans)




##########============= Complexity Point system =============########


my.data <- read.csv("Complexity tests points.csv", header = T) #import the data
View(my.data)

#Complexity against connectivity
ggplot(data = my.data, aes(x = my.data$Close.plus.med.con, 
                           y = my.data$Max.Complexity)) + 
  #geom_point(size = 3) +
  geom_jitter() +
  geom_smooth(colour = "black",   # black line
              width = 1,          # slightly thicker
              se = T,         # turn off confidence band
              method = "gam",     # use 'gam' instead of default (loess)
              formula = y ~ s(x)) +
  theme_classic() +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(color = "Site") +
  xlab ("Connectivity") +
  ylab ("Complexity")



#create a linear model for this relationship
my.lm <- lm(Max.Complexity ~ Close.plus.med.con, data=my.data)

autoplot(my.lm) #check assumptions of linear model
summary(my.lm) #get significance

#non-parametric test to use if assumptions of linear model are not met
cor.test(my.data$Max.Complexity, my.data$Close.plus.med.con, method = "spearman")
