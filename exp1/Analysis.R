# Setup -------------------------------------------------------------------
library(dplyr); library(reshape2)                 # for data manipulation
library(ggplot2); library(Hmisc); library(ggpubr) # for plotting
library(lme4); library(car); library(emmeans)     # for mixed-effects regressions 
library(effsize)                                  # for effect sizes

setwd("~/Desktop/Ongoing/EffortProg/Exp1/Analysis")
files = list.files(path = 'data/', pattern = '.csv')
d <- do.call("rbind", lapply(paste0('data/',files) , read.csv, stringsAsFactors = F))

# Compute useful variables ------------------------------------------------
d$id = factor(d$id)
d$condition = factor(d$condition, ordered = F)
d = d %>% group_by(id, condition, block) %>% mutate(sr = sum(switch=='switch', na.rm=T)/length(switch), 
                                                    difficulty = ifelse(sr < 0.5, 'easy', 'hard'), 
                                                    choice = ifelse(sr > 0.5, 1,0))
d = d %>% group_by(id) %>% mutate(trial = 1:length(id))


# Descriptives & exclusion ------------------------------------------------
# compute descriptives before exclusion
descpre = d %>% mutate(sex = ifelse(sex %in% c('male', 'Male', 'MALE', 'M'), 1, 
                                     ifelse(sex %in% c('female', 'Female', 'FEMALE', 'F'), 0, 2)), 
                        
                        age = ifelse(as.numeric(age) < 150, age, NA)) %>% 
  group_by(condition) %>% summarise(n = length(unique(id)), 
                                    mage = mean(as.numeric(age), na.rm=T),
                                    sdage = sd(as.numeric(age), na.rm=T), 
                                    msex = mean(sex), 
                                    sdsex = sd(sex))

# see prereg at https://osf.io/gfrsh; deviate due to too many exclusions
# also check if results hold when we skip this step (they do)
maxtimeouts = 25 # maximum number of timeouts
minrepacc = 0.5 # minimum accuracy needed on repeat trials

# Timeouts
d = d %>% group_by(id) %>% mutate(ntimeout = sum(is.na(key_press))) %>% filter(ntimeout < maxtimeouts) %>% 
   ungroup() %>% select(everything(), -ntimeout)

# Accuracy 
d = d[d$id %in% (d %>% group_by(id, switch) %>% summarise(macc = mean(acc, na.rm=T)) %>% 
  filter(switch == 'repeat', macc >= minrepacc))$id,]

# Understanding of the task 
# look at this by hand
understand = d %>% group_by(id, condition) %>% 
  summarise(comments = comments[1], pref = preference[1], not = notice[1])

# Descriptives after exclusion 
descpost = d %>% mutate(sex = ifelse(sex %in% c('male', 'Male', 'MALE', 'M'), 1, 
                                     ifelse(sex %in% c('female', 'Female', 'FEMALE', 'F'), 0, 2)), 
                        
                        age = ifelse(as.numeric(age) < 150, age, NA)) %>% 
  group_by(condition) %>% summarise(n = length(unique(id)), 
                                    mage = mean(as.numeric(age), na.rm=T), 
                                    sdage = sd(as.numeric(age), na.rm=T), 
                                    msex = mean(sex), 
                                    sdsex = sd(sex))
                                    
# Task-switching analysis -------------------------------------------------
# * Analyse ---- 
# compare rt and accuracy across demand levels
# rt
# compare random effects structure 
# m0 = lmer(rt ~ difficulty*condition + (1 |id), data=d)
# m1 = lmer(rt ~ difficulty*condition + (trial|id), data=d, control=lmerControl(optimizer="bobyqa")) # convergence error
# m2 = lmer(rt ~ difficulty*condition + (1|id) + (1|block), data=d, control=lmerControl(optimizer="bobyqa")) 
# m3 = lmer(rt ~ difficulty*condition + (1|id) + (trial|block), data=d, control=lmerControl(optimizer="bobyqa")) # singular fit
# AIC(m0, m1, m2, m3) # m3 has lowest AIC
# anova(m0, m2) # m2 outperforms m0

demand_rt = lmer(rt ~ difficulty*condition + (1|id) + (1|block), 
                 data=d[d$acc==1,], # correct rts only  
                 control=lmerControl(optimizer="bobyqa"))
confint(demand_rt, method='Wald') # 95% CI 
car::Anova(demand_rt) # get p-values (requires car package to be installed)
# emmeans(demand_rt, list(pairwise ~ difficulty*condition), adjust = "bonferroni") # post-hocs

# accuracy
# compare random effects structure  
# m0 = glmer(acc ~ difficulty + (1|id), data=d, family = 'binomial')
# m1 = glmer(acc ~ difficulty + (trial|id), data=d, family = 'binomial', control=glmerControl(optimizer="bobyqa")) # convergence error
# m2 = glmer(acc ~ difficulty + (1|id) + (1|block), data=d, family = 'binomial') # singular fit (simplify RE structure)

demand_acc = glmer(acc ~ difficulty*condition + (1|id), data=d, family='binomial')
confint(demand_acc, method='Wald') # 95% CI 
fixef(demand_acc)[2] *(sqrt(3)/pi) # compute logit d 
emmeans(demand_acc, list(pairwise ~ difficulty*condition), adjust = "bonferroni") # post-hocs

# compare rt and accuracy across trial types
# rt
# compare random effects structure
# m0 = lmer(rt ~ switch*condition + (1|id), data=d)
# m1 = lmer(rt ~ switch*condition + (trial|id), data=d, control=lmerControl(optimizer="bobyqa")) # failed to converge
# m2 = lmer(rt ~ switch*condition + (1|id) + (1|block), data=d, control=lmerControl(optimizer="bobyqa"))
# m3 = lmer(rt ~ switch*condition + (trial|id) + (trial|block), data=d, control=lmerControl(optimizer="bobyqa")) # failed to converge
# AIC(m0, m1, m2, m3) # m2 has lower AIC
# anova(m0, m2) # m2 significantly better than m0

switch_rt = lmer(rt ~ switch*condition + (1|id) + (1|block), 
                 data=na.omit(d[d$acc==1, ]), # correct rts only 
                 control=lmerControl(optimizer="bobyqa"))
confint(switch_rt, method='Wald') # 95% CI 
car::Anova(switch_rt) # extract p-values 
emmeans(switch_rt, list(pairwise ~ switch*condition), adjust = "bonferroni") # post-hocs

# accuracy
# compare random effects structure  
# m0 = glmer(acc ~ switch + (1|id), data=d, family = 'binomial')
# m1 = glmer(acc ~ switch + (trial|id), data=d, family = 'binomial', control=glmerControl(optimizer="bobyqa")) # convergence error
# m2 = glmer(acc ~ switch + (1|id) + (1|block), data=d, family = 'binomial') # singular fit (simplify RE structure)

switch_acc = glmer(acc~switch*condition + (1|id), data=d, family = 'binomial')
confint(switch_acc, method='Wald') # 95% CI 
fixef(switch_acc)[2] *(sqrt(3)/pi) # compute logit d 
emmeans(switch_acc, list(pairwise ~ switch*condition), adjust = "bonferroni") # post-hocs

# * Visualise ----
blockcostplot <- 
  d %>% mutate(difficulty = factor(ifelse(difficulty == 'easy', '10%', '90%'), levels = c('10%', '90%')), 
               corrt = ifelse(acc==1, rt, NA)) %>% 
    group_by(id, difficulty) %>% 
    summarise(meanRT = mean(as.numeric(corrt), na.rm = T), 
              meanACC = mean(acc, na.rm = T)) %>% 
    melt(id.vars=c('id', 'difficulty')) %>% 
    ggplot(aes(x=difficulty,y=value, fill=difficulty)) + 
    stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
    geom_point(alpha=0.05) +
    geom_line(aes(group=id), alpha=0.05) +
    facet_wrap(~variable, scales = 'free_y', 
               strip.position = "left", 
               labeller = as_labeller(c(meanRT = "Correct RT (in ms.)", meanACC = "% Correct")))  +
    labs(x = 'Block-Wise Switch Rate', y='', fill = 'block difficulty', title='Performance Across Blocks') + 
    theme_classic() +     
    theme(strip.background = element_blank(), strip.placement = "outside",
          plot.title = element_text(hjust=0.5), 
          legend.position = 'none') + 
    scale_fill_grey()
  
switchcostplot <- 
  d %>% group_by(id, switch) %>%
    mutate(corrt = ifelse(acc==1, rt, NA)) %>%
    summarise(meanRT = mean(as.numeric(corrt), na.rm =T), 
              meanACC = mean(acc, na.rm = T)) %>% 
    filter(is.na(switch)==F) %>% 
    melt(id.vars=c('id',  'switch')) %>% 
    ggplot(aes(x=switch,y=value, fill=switch)) + 
    stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
    geom_point(alpha=0.05) +
    geom_line(aes(group=id), alpha=0.05) +
    facet_wrap(~variable, scales = 'free_y', 
               strip.position = "left", 
               labeller = as_labeller(c(meanRT = "Correct RT (in ms.)", meanACC = "% Correct")))  +
    labs(x = 'Trial Type', y='', fill = 'condition', title='Performance Within Blocks') + 
    theme_classic() +     
    theme(strip.background = element_blank(), strip.placement = "outside", 
          plot.title = element_text(hjust=0.5), 
          legend.position='none') + 
    scale_fill_grey()
  
ggarrange(blockcostplot, switchcostplot, nrow = 2, labels = letters[1:2])

# Choice analysis ---------------------------------------------------------
# * Analyse ----
choice_glmer = d %>% 
  mutate(condition_rlv = relevel(condition, ref = "1")) %>% # change reference to check baseline for progress against chance
  group_by(id, condition, condition_rlv, block) %>% 
  summarise(choice=choice[1]) %>% 
  list(glmer(data=., formula=choice~condition + (1|id), family='binomial'), 
       glmer(data=., formula=choice~condition_rlv + (1|id), family='binomial'))
# summary(choice_glmer[[2]]) # ref = no progress
# summary(choice_glmer[[3]]) # ref = progress

confint(choice_glmer[[2]], method='Wald') # 95% CI 
confint(choice_glmer[[3]], method='Wald') # 95% CI 

# * Visualise ----
overallchoiceplot <- 
  d %>% group_by(id, condition, block) %>% 
  summarise(sr = sum(switch=='switch', na.rm=T)/length(switch), 
            choice = ifelse(sr > 0.5, 1, 0)) %>%
  summarise(pHard = sum(choice==1)/length(choice)) %>% 
  mutate(condition = ifelse(condition==0, 'No Progress', 'Progress')) %>% 
  ggplot(aes(x = condition, y = pHard*100, fill = condition)) + 
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  coord_cartesian(ylim=c(35, 60)) + 
  geom_hline(yintercept = 50, linetype='dashed') + 
  labs(x = '', y = '% High Demand Chosen', fill = 'Condition') +
  scale_fill_grey() + 
  theme_classic() + theme(plot.title = element_text(hjust=0.5), 
                          axis.title = element_text(size=15),
                          axis.text = element_text(size = 12),
                          legend.position = 'none')

choiceblockplot <- 
  d %>% mutate(condition = ifelse(condition==1, 'Progress', 'No Progress')) %>%
  group_by(id,condition, block) %>% 
  summarise(mchoice = mean(choice)) %>% 
  ggplot(aes(x = block, y = mchoice, color = condition)) + 
  stat_summary(fun.y = mean, geom = "point", size = 2) + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.2) + 
  labs(x = 'Block', y='% High Demand Chosen', color = 'Condition', title='Demand Selection Per Block') + 
  scale_colour_grey() + 
  theme_classic() + theme(plot.title = element_text(hjust=0.5), 
                          axis.text = element_text(size=12), 
                          axis.title = element_text(size=15), 
                          legend.text = element_text(size=12), 
                          legend.title = element_text(size=15))


# TLX ---------------------------------------------------------------------
tlx = d[c(1:2, 12, 19:28)] %>% 
  group_by(id) %>% summarise_all(unique) %>%
  melt(id.vars=c('id', 'condition', 'easy_colour')) %>%
  filter(grepl('purple', variable, fixed = TRUE)|grepl('blue', variable, fixed = TRUE)) %>%
  mutate(diff = ifelse(easy_colour == sub(".*_", "", variable), 'easy', 'hard')) %>% 
  mutate(variable = as.factor(sub("_.*", "", variable))) %>%
  mutate(value = as.numeric(value)) %>%
  dcast(formula = id + condition +diff~variable,fun.aggregate = sum,value.var = "value") 

# * Analyse ----
tlx.mmr = list(
  lmer(effort ~ condition*diff + (1|id), data=tlx),
  lmer(frustration ~ condition*diff + (1|id), data=tlx),
  lmer(mental ~ condition*diff + (1|id), data=tlx),
  lmer(performance ~ condition*diff + (1|id), data=tlx),
  lmer(temporal ~ condition*diff + (1|id), data=tlx))

# extract boneferonni corrected p-values (adjusted for 5 comparisons)
tlx.mmr.p = lapply(tlx.mmr, function(x) p.adjust(Anova(x)$`Pr(>Chisq)`, method='bonferroni', n=5)) 

# * Visualise ----
tlx %>% 
  mutate(
    diff = factor(ifelse(diff=='easy', 'low', 'high'), 
                  levels = c('low', 'high')), 
    condition = factor(ifelse(condition==1, 'Progress', 'No Progress'),
                       levels = c('No Progress', 'Progress'))) %>% 
  melt(id.vars = c('id', 'condition', 'diff')) %>% 
  ggplot(aes(x=variable, y = value, fill = diff)) + 
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) +
  coord_cartesian(ylim=c(5, 7.5)) + 
  facet_wrap(~condition) + 
  labs(x = 'TLX Subscale', y = 'Score', fill = 'Demand Level') + 
  scale_fill_brewer(palette="Set1") + 
  theme_classic()
