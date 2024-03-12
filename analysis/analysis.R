this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(simr)
library(lmerTest)
library(bootstrap)
library(ggpubr)
library(broom)    
library(entropy)

`%notin%` <- Negate(`%in%`)

data <- read.csv("data_full.csv")

#Filter out the participants who responded incorrectly more than once to the practice questions:


excluded_subjects <- c()
practice_data=subset(data,item_type %in% c("practice_bad", "practice_good"))
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- c(excluded_subjects, subset(data, !is.element(workerid, practice_good_data$workerid))$workerid)
data=subset(data, is.element(workerid, practice_good_data$workerid))

length(unique(data$workerid))


data_no_practice <- filter(data, is.na(block_num) == FALSE)

data_no_practice = data_no_practice%>%
  mutate(new_block = if_else(block_num < 6, block_num, block_num - 5))%>%
  filter(alt_status %in% c("target","gram", "ungram"))%>%
  mutate(new_mode = if_else(mode == "a", "target-first", "alternative-first"))

trial_means = data_no_practice %>%
  group_by(new_block,condition,new_mode) %>%
  summarise(response = mean(response))


satiation_plot1 <- ggplot(data_no_practice, aes(x=new_block, y=response, color = condition, fill = condition)) + 
  geom_point(data=trial_means,alpha=.9) + 
  xlab("Presentation Order") +
  ylab("Acceptability rating")+
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()+
  facet_wrap(~new_mode)

satiation_plot1

satiation_plot2 <- ggplot(data_no_practice, aes(x=new_block, y=response, color = new_mode, fill = new_mode)) + 
  geom_point(data=trial_means,alpha=.9) + 
  xlab("Presentation Order") +
  ylab("Acceptability rating")+
  geom_smooth(method=lm, aes(fill=new_mode))+theme_bw()+
  facet_wrap(~condition)

satiation_plot2

data_no_practice$new_mode <- as.factor(data_no_practice$new_mode)
data_no_practice$new_mode <- relevel(as.factor(data_no_practice$new_mode), ref = "alternative-first")
data_no_practice$condition <- relevel(as.factor(data_no_practice$condition), ref = "GRAM")

data_no_practice = data_no_practice %>%
  mutate(mode_num = if_else(new_mode == "alternative-first", 0, 1)) %>%
  mutate(mode_centered = mode_num - mean(mode_num))

model <- lmer(response~condition*new_block*mode_centered + 
                (1+condition*new_block|workerid) + (1+mode_centered*new_block|item), data = data_no_practice)
summary(model)

