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

data <- read.csv("pilot_data.csv")
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

data_no_practice$new_mode <- as.factor(data_no_practice$new_mode)
data_no_practice$condition <- relevel(as.factor(data_no_practice$condition), ref = "GRAM")

contrasts(data_no_practice$new_mode) <- contr.sum(2)
model <- lmer(response~condition*new_block*new_mode + 
                (1|workerid), data = data_no_practice)
summary(model)
