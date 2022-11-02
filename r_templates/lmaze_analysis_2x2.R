##################################################
## Project: L-Maze Analysis Template
## Script purpose: Analyze data using LMER and output line graphs/target region graphs
## To be used for 2x2 factorial design
## USE Alt+O to make more readable!
## Date: 11.01.22
## Author: Devin Johnson
##################################################

## Section: Library Imports
##################################################
library(plyr)
library(dplyr)
library(ggplot2)
library(lmerTest)
library(ggsignif)
library(lattice)
library(stringr)
library(ggpubr)
library(cowplot)
library(lsmeans)
library(tidyverse)
library(scales)

## Section: Setup WD and Global Constants (YOU EDIT)
##################################################
rm(list=ls())
setwd("C:/Users/devin/Documents/Code/Johnson_10-22/Results")
F1_LEVELS = c("each", "adj")
F2_LEVELS = c("do", "does")
MY_CONDITIONS = c("be_many_a", "be_many_b", "be_many_c", "be_many_d")
CSV_NAME = "results_main.csv"
MIN_RT = 300
MAX_RT = 3500
EXAMPLE_SENTENCE = c("How","many"," accessories/shiny","each/accessories,", "based", "on", "the", "shelves,", "do/does", "the", "easygoing",
                     "but", "skeptical", "customer(s)", "think", "that", "the", "salesmen", "sell", "on", "a", "good", "day?")
BAD_P = c()

## Section: Functions
##################################################
rt_means = function(data){
  # Get mean reading time per position per condition (each participant)
  data.sum.rt = ddply(data, .(subj, cond, word_num, f1, f2), summarize, mean_rt_p = mean(rt, na.rm=TRUE))
  
  # Get mean reading time per position per condition (all participants)
  data.sum.rt.means = ddply(data.sum.rt, .(cond, word_num, f1, f2), summarize, 
                            mean_rt = mean(mean_rt_p),
                            n = length(unique(data.sum.rt$subj)),
                            sd = sd(mean_rt_p),
                            se = sd/sqrt(n))
  
  return(data.sum.rt.means)
}

all_region_plot = function(data, name, x_breaks){
  
  data$cond = as.factor(data$cond)
  
  levels(data$cond) = c(paste(F1_LEVELS[1], F2_LEVELS[1], sep = "_"),
                        paste(F1_LEVELS[1], F2_LEVELS[2], sep = "_"), 
                        paste(F1_LEVELS[2], F2_LEVELS[1], sep = "_"), 
                        paste(F1_LEVELS[2], F2_LEVELS[2], sep = "_"))
  
  ggplot(data, aes(x=word_num, y=mean_rt, color=cond)) +
    geom_errorbar(aes(ymin=mean_rt-se, ymax=mean_rt+se), color = "black", width=.1) +
    geom_line(aes(color = cond)) +
    geom_point() +
    scale_x_continuous(breaks=1:length(x_breaks)*1, labels = str_wrap(x_breaks, width = 1)) +
    labs(title = element_blank(), x = "Regions", y = "Log Reading Time (ms)", color = "Condition") +
    theme_classic() +
    theme_linedraw() +
    theme(text = element_text(family = "Times New Roman", size = 10),
          legend.position = "top",
          legend.title = element_blank(),
          legend.key.size = unit(0.7, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(name, width = 40, height = 15, units = c("cm"), dpi = 600)
}

target_region_plot = function(data, name){
  ggplot(data, aes(x = f1, y = mean_rt, fill = f2)) + 
    geom_bar(stat = 'identity', color = "black", position=position_dodge(width=0.9)) + 
    geom_errorbar(position = position_dodge(width = 0.9), width = .05, aes(ymax =mean_rt+se, ymin = mean_rt-se)) +
    labs(title = "Target Region", x = "f1", y = "Log Reading Time (ms)", fill = "f2")+
    theme_classic()+
    theme_linedraw()+
    theme(text = element_text(family = "Times New Roman", size =15),
          legend.position = "right",
          legend.key.size = unit(0.5, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(name, width = 15, height =12, units = c("cm"), dpi = 600)
}


## Section: Load/Format Data
##################################################
data = read.csv(CSV_NAME) %>% subset(cond %in% MY_CONDITIONS) # Get only my conditions
data = droplevels(subset(data, !(subj %in% BAD_P))) # Drop bad participants

# Add two new columns for factors (YOU EDIT)
data = data %>%
  mutate(f1 = case_when(cond ==  "be_many_a" ~ "each",
                        cond == "be_many_b" ~ "each",
                        cond == "be_many_c" ~ "adj",
                        cond == "be_many_d" ~ "adj"), .after = cond) %>%
  mutate(f2 = case_when(cond == "be_many_a" ~ "do",
                        cond == "be_many_b" ~ "does",
                        cond == "be_many_c" ~ "do",
                        cond == "be_many_d" ~ "does"), .after = f1)

data$f1 = as.factor(data$f1)
data$f2 = as.factor(data$f2)
data$rt = as.numeric(data$rt)
data$word_num = as.numeric(data$word_num) + 1 # Start word numbers from 1
data = data %>% filter(!(rt < MIN_RT | rt > MAX_RT)) # Remove outliers
data = droplevels(subset(data, (correct =="yes"))) ## remove wrong answers
data$rt = log(data$rt) # Log RT
data_target = droplevels(subset(data, (word %in% F2_LEVELS))) # Get target region (YOU EDIT)


## Section: Means and Plotting
##################################################
data_rt_means = rt_means(data)
data_target_rt_means = rt_means(data_target)
data_rt_means %>% all_region_plot("../Plots/BE1_lmaze_lineplot.png", EXAMPLE_SENTENCE)


## Section: Stats on Target Region
##################################################
cat("F1 Levels:", "{", levels(data_target$f1), "}", sep=" ")
cat("F2 Levels:", "{", levels(data_target$f2), "}", sep=" ")

# Contrast coding tells model how to treat categorical variables (there are other ways)
contrasts(data_target$f1) = contr.sum(2) # Tell the model that f1 has two levels
contrasts(data_target$f2) = contr.sum(2) # Tell the model that f2 has two levels

contr.sum(2)
contrasts(data_target$f1)
contrasts(data_target$f2)

summary(data_target)

m1 = lmer(rt ~ f1 * f2 + (1|subj), data=data_target)
m2 = lmer(rt ~ f1 * f2 + (1+f1|subj), data=data_target)

anova(m1,m2) # No sig difference btw. models
summary(m1)
# summary(m2)

lsmeans(m1, pairwise ~ f1 | f2) ### Post hoc pairwise comparison
lsmeans(m1, pairwise ~ f2 | f1)










