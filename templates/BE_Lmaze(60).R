##########
# Set-Up #
##########
rm(list=ls())
setwd("C:/Users/devin/Downloads")

# Load necessary packages 
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

######################
# Data Pre-Processing#
######################

md <- read.csv("BE_Lmaze(60).csv") 
md <- droplevels(subset(md, (correct =="yes"))) ## remove wrong answers

length(levels(md$subj)) 
levels(md$subj) <- c(1:60)
md <- droplevels(subset(md, !(subj %in% c("52","53","54","55","56","57"))))
length(levels(md$subj)) 
### 범인을 찾아라 (52,53,54,55,56,57)

################################ specify conditions #########################
md <- md %>% mutate(C_Command = case_when(cond == "a" ~ "CC+", cond == "b" ~ "CC-", cond == "c" ~ "CC+", cond == "d" ~ "CC-"))
md <- md %>% mutate(Sluicing = case_when(cond == "a" ~ "each", cond == "b" ~ "each", cond == "c" ~ "total", cond == "d" ~ "total"))
md$C_Command <- as.factor(md$C_Command)
md$Sluicing <- as.factor(md$Sluicing)

################################ item number #################################
str(md$item)
md$item <- as.factor(md$item)
levels(md$item)
levels(md$item) <- c(rep(1:24, each=4))

################################ word number #################################
str(md$wordnumber)
levels(md$wordnumber)
md$wordnumber <- as.character(md$wordnumber)
md$wordnumber <- as.numeric(md$wordnumber)
md$wordnumber[] <- sapply(md$wordnumber, function(x) ifelse(x>=0, x+1, x)) ### number starts from 1 instead of 0
md$wordnumber <- as.factor(md$wordnumber)

A <- droplevels(subset(md, wordnumber == "17"))
levels(A$word) ## Target Region for Complexity Effect

B <- droplevels(subset(md, wordnumber == "21"))
levels(B$word) ## Target Region for Reanalysis

#md <- droplevels(subset(md, !(wordnumber == 21 & word == c("the")))) ### drop some region mismatch items

#B <- droplevels(subset(md, wordnumber == "21"))
#levels(B$word) ## Target region

test_RT_subj = ddply(md, .(subj), summarize, RT = mean(RT,na.rm=TRUE))
test_RT_item = ddply(md, .(item), summarize, RT = mean(RT,na.rm=TRUE))
subj_count = count(md,subj)
item_count = count(md,item)

################################ outliers (rt) #################################
str(md$RT)
md$RT <- as.numeric(md$RT)
md <- md %>% 
  filter(!(RT<300 | RT>3500))%>%
  mutate(mean_rt = mean(RT, na.rm = TRUE))

################################ Residualized RT ###############################
md$subj <- as.factor(md$subj)
md$sentence <- as.character(md$sentence)
md <- subset(md, !is.na(RT) ) %>%
  group_by(subj) %>%
  mutate(ResidRT = resid(lm(as.numeric(RT)~as.numeric(nchar(as.character(sentence))))))

######## Compute the z-score per region, subject and condition for the residual RTs
md <- filter(md) %>%
  group_by(subj, wordnumber, cond) %>%
  mutate(Zscore = as.vector(scale(ResidRT)))

######## Omit data 2 SDs greater or less (optional)
#sd_min=-2
#sd_max=2
#md <- md[md$Zscore < sd_max & md$Zscore > sd_min,]

##################
## Define MSDSE ##
##################
### RRT
str(md)
md.sum.RRT = ddply(md, .(subj, cond, wordnumber), summarize, RRT = mean(ResidRT))
md.sum.RRT <- md.sum.RRT %>% drop_na() 
md.sum.RRT.means = ddply(md.sum.RRT, .(cond, wordnumber), summarize, 
                   N = length(unique(md.sum.RRT$subj)),
                   mean = mean(RRT),
                   sd = sd(RRT),
                   se = sd/sqrt(N))

###############
### plotting ##
###############

### line plot for each region and condition 
V1 <- c("The The The The","teachers teacher teachers teacher","who who who who","the the the the","student students student students", "respects respect respects respect",
        "wrtoe wrote wrote wrote", "many many many many", "books, books, books, books,","but but but but", "I I I I", "don't don't don't don't", "know know know know",
        "how how how how", "many many many many", "books books books books", "each, each, total, total,", "according according according according", "to to to to",
        "John, John, John, John,","we we we we","are are are are","supposed supposed supposed supposed", "to to to to", "read. read. read. read.")
length(V1)
levels(md.sum.RRT.means$cond) <- c("CC+/each", "CC-/each", "CC+/total", "CC-/total")
md.sum.RRT.means$wordnumber <- as.numeric(md.sum.RRT.means$wordnumber)
md.sum.RRT.means <- droplevels(subset(md.sum.RRT.means, (wordnumber < 26)))

ggplot(md.sum.RRT.means, aes(x=wordnumber, y=mean, color=cond)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), color = "black", width=.1) +
  geom_line(aes(color = cond))+
  geom_point() +
  #scale_linetype_manual(values=c("dotdash", "solid", "dotdash", "solid" )) +
  #scale_color_manual(values = c("orange2", "deepskyblue3","orange2", "deepskyblue3")) +
  #scale_shape_manual(values=c(15, 15, 17, 17))+
  scale_y_continuous(breaks=-100:100*50) +
  scale_x_continuous(breaks=1:25*1, labels= str_wrap(V1, width = 1))+
  labs(title = element_blank(), x = "Regions", y = "Residualized Reading Times (ms)", color = "Condition") +
  theme_classic()+
  theme_linedraw()+
  theme(text = element_text(family = "Times New Roman", size =15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.7, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("lineplot(56).jpg", width = 40, height =15, units = c("cm"), dpi = 600)


################################# Target regions (each/total) #################################
md_target <- droplevels(subset(md, wordnumber == "17"))
md_target$word <- as.factor(md_target$word)
levels(md_target$word)

### RRT
md.sum.target = ddply(md_target, .(subj, wordnumber,Sluicing, C_Command ), summarize, RRT = mean(ResidRT))
md.sum.tarrget.means = ddply(md.sum.target, .(wordnumber, Sluicing, C_Command), summarize, 
                         N = length(unique(md.sum.target$subj)),
                         mean = mean(RRT),
                         sd = sd(RRT),
                         se = sd/sqrt(N))

ggplot(md.sum.tarrget.means, aes(x = Sluicing, y = mean, fill = factor(C_Command, level = c("CC+","CC-")))) + 
  geom_bar(stat = 'identity', color = "black", position=position_dodge(width=0.9)) + 
  geom_errorbar(position = position_dodge(width = 0.9), width = .05, aes(ymax =mean+se, ymin = mean-se)) +
  scale_y_continuous(breaks=-100:100*20) +
  labs(title = expression('Target Region:'~italic(each)~italic(vs.)~italic(total)), x = "Sluicing Type", y = "Residualized Reading Times (ms)", fill = "C-Command")+
  theme_classic()+
  theme_linedraw()+
  theme(text = element_text(family = "Times New Roman", size =15),
        legend.position = "right",
        #legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("barplot_targetRG(56).jpg", width = 15, height =12, units = c("cm"), dpi = 600)

################################# Spillover regions (according) #################################
md_spov <- droplevels(subset(md, wordnumber == "18"))
md_spov$word <- as.factor(md_spov$word)
levels(md_spov$word)

### RRT
md.sum.spov = ddply(md_spov, .(subj, wordnumber,Sluicing, C_Command ), summarize, RRT = mean(ResidRT))
md.sum.spov.means = ddply(md.sum.spov, .(wordnumber, Sluicing, C_Command), summarize, 
                             N = length(unique(md.sum.spov$subj)),
                             mean = mean(RRT),
                             sd = sd(RRT),
                             se = sd/sqrt(N))

ggplot(md.sum.spov.means, aes(x = Sluicing, y = mean, fill = factor(C_Command, level = c("CC+","CC-")))) + 
  geom_bar(stat = 'identity', color = "black", position=position_dodge(width=0.9)) + 
  geom_errorbar(position = position_dodge(width = 0.9), width = .05, aes(ymax =mean+se, ymin = mean-se)) +
  scale_y_continuous(breaks=-100:100*50) +
  labs(title = expression('Spillover Region:'~italic(according)), x = "Sluicing Type", y = "Residualized Reading Times (ms)", fill = "C-Command")+
  theme_classic()+
  theme_linedraw()+
  theme(text = element_text(family = "Times New Roman", size =15),
        legend.position = "right",
        #legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("barplot_spovRG(56).jpg", width = 15, height =12, units = c("cm"), dpi = 600)


################################# disambiguation (each/total) #################################
md_dis <- droplevels(subset(md, wordnumber == "22"))
md_dis$word <- as.factor(md_dis$word)
levels(md_dis$word)
### not controlled: cannot test reanalysis

#### plot together
#ggdraw() +
#  draw_plot(q, x = 0, y = 0, width = 0.5, height = 1) +
#  draw_plot(z, x = 0.5, y = 0, width = 0.5, height = 1) +
#  draw_plot_label(label = c("A","B"), size = 20, x = c(0, 0.5), y = c(1, 1))
#ggsave("plots_BC(56).jpg", width = 30, height = 15, units = c("cm"), dpi = 600)

##################
### statistics ###
##################

# ################## Target RG: Sum(Deviation) contrast coding ################## 
# contrasts(md_target$C_Command) = contr.sum(2)
# contrasts(md_target$Sluicing) = contr.sum(2)

model1 = lmer(ResidRT ~ C_Command * Sluicing + (1+C_Command|subj), data = md_target)
model2 = lmer(ResidRT ~ C_Command * Sluicing + (1|subj), data = md_target)
anova(model1, model2) ### model1 is better fit 
summary(model1) ### main effect of Sluicing + interaction

lsmeans(model1, pairwise ~ C_Command | Sluicing) ### Post hoc pairwise comparison
### C-Command effect in each, but no effect in total --> interaction 

################## Spov RG: Sum(Deviation) contrast coding ################## 
# contrasts(md_spov$C_Command) = contr.sum(2)
# contrasts(md_spov$Sluicing) = contr.sum(2)

model3 = lmer(ResidRT ~ C_Command * Sluicing + (1+C_Command+Sluicing|subj) + (1|item)  , data = md_spov)
model4 = lmer(ResidRT ~ C_Command * Sluicing + (1|subj) + (1|item) , data = md_spov)
anova(model3, model4) ### model3 is better fit 
summary(model3) ### main effect of Sluicing

