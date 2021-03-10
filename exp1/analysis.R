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
# rt
d$corrt <- ifelse(d$acc==1, d$rt, NA)
m0.rt.b = lmer(corrt ~ 1 + (1|id), data=d, REML=F)
m1.rt.b = lmer(corrt ~ difficulty + (1|id), data=d, REML=F, control=lmerControl(optimizer="bobyqa")) 
m2.rt.b = lmer(corrt ~ difficulty + condition + (1|id), data=d, REML = F, control=lmerControl(optimizer="bobyqa"))
m3.rt.b = lmer(corrt ~ difficulty*condition + (1|id), data=d, REML=F, control=lmerControl(optimizer="bobyqa")) 

anova(m0.rt.b, m1.rt.b, m2.rt.b, m3.rt.b) # m1 is best
anova(m1.rt.b, m3.rt.b)
confint(m1.rt.b, method='Wald', oldNames=F) # 95% CI 
Anova(m1.rt.b) # for pvalues

# accuracy
m0.acc.b = glmer(acc ~ 1 + (1|id), data=d, family = 'binomial')
m1.acc.b = glmer(acc ~ difficulty + (1|id), data=d, family = 'binomial')
m2.acc.b = glmer(acc ~ difficulty+condition + (1|id), data=d, family = 'binomial')
m3.acc.b = glmer(acc ~ difficulty*condition + (1|id), data=d, family = 'binomial')

anova(m0.acc.b, m1.acc.b, m2.acc.b, m3.acc.b) 
anova(m1.acc.b, m3.acc.b)
confint(m1.acc.b, method='Wald') # CI for main effect
confint(m3.acc.b, method='Wald') # CI for interaction 

# compare rt and accuracy across trial types
# rt
d.complete <- d[which(complete.cases(d[,c('switch')])),]
m0.rt.t = lmer(corrt ~ 1 + (1|id), data=d.complete, REML=F)
m1.rt.t = lmer(corrt ~ 1 + switch + (1|id), data=d.complete, REML=F, control=lmerControl(optimizer="bobyqa")) 
m2.rt.t = lmer(corrt ~ 1 + switch + condition + (1|id), data=d.complete, REML = F, control=lmerControl(optimizer="bobyqa"))
m3.rt.t = lmer(corrt ~ 1 + switch*condition + (1|id), data=d.complete, REML=F, control=lmerControl(optimizer="bobyqa")) 

anova(m0.rt.t, m1.rt.t, m2.rt.t, m3.rt.t) # m1 is best
anova(m1.rt.t, m3.rt.t)
confint(m1.rt.t, method='Wald') # 95% CI 
Anova(m1.rt.t) # for pvalues

# accuracy
m0.acc.t = glmer(acc ~ 1 + (1|id), data=d.complete, family = 'binomial')
m1.acc.t = glmer(acc ~ switch + (1|id), data=d.complete, family = 'binomial')
m2.acc.t = glmer(acc ~ switch+condition + (1|id), data=d.complete, family = 'binomial')
m3.acc.t = glmer(acc ~ switch*condition + (1|id), data=d.complete, family = 'binomial')

anova(m0.acc.t, m1.acc.t, m2.acc.t, m3.acc.t) 
anova(m1.acc.t, m3.acc.t)
confint(m1.acc.t, method='Wald') # CI for main effect
confint(m3.acc.t, method='Wald') # CI for interaction 

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
tlx.models = list('effort'=list(), 'frustration'=list(), 'mental'=list(), 'performance'=list(), 'temporal'=list())
for(p in c('effort', 'frustration', 'mental', 'performance', 'temporal')) {
  tlx.models[[p]][['m0']] = lmer(tlx[,p] ~ 1 + (1|id) , data=tlx)
  tlx.models[[p]][['m1']] = lmer(tlx[,p] ~ diff + (1|id), data=tlx)
  tlx.models[[p]][['m2']] = lmer(tlx[,p] ~ condition+diff + (1|id), data=tlx)
  tlx.models[[p]][['m3']] = lmer(tlx[,p] ~ condition*diff + (1|id), data=tlx)
}

tlx.aov = sapply(names(tlx.models), function(x) anova(tlx.models[[x]]$m0, tlx.models[[x]]$m1,tlx.models[[x]]$m2,tlx.models[[x]]$m3), simplify = F)
tlx.sum = sapply(names(tlx.models), function(x) summary(tlx.models[[x]]$m1), simplify = F)
tlx.models.p = sapply(names(tlx.models), function(x) p.adjust(Anova(tlx.models[[x]]$m1)$`Pr(>Chisq)`, method='bonferroni', n=5)) 
tlx.models.ci = sapply(names(tlx.models), function(x) confint(tlx.models[[x]]$m1, method='Wald')[4,], simplify = F)
tlx.m1.d =  sapply(names(tlx.models), function(x) cohen.d(d=tlx[,x], f=tlx$diff)$estimate)

# easy summary for write-up
# for(p in names(tlx.models)) {
#   b = paste0('b = ', round(tlx.sum[[p]]$coefficients['diffhard', 'Estimate'], 4), ',')
#   se = paste0('SE = ', round(tlx.sum[[p]]$coefficients['diffhard', 'Std. Error'], 4), ',')
#   pval = paste0('p = ', round(tlx.models.p[p], 4), ',')
#   ci = paste0('95% CI = [', round(tlx.models.ci[[p]][1], 2), ', ', round(tlx.models.ci[[p]][2], 2), '],')
#   cd = paste0('d = ', round(tlx.m1.d[p], 2))
#   
#   print(toupper(p))
#   print(paste(b, se, pval, ci, cd ))
#   }

# Choice analysis ---------------------------------------------------------
# * Analyse ----
choice_data = d %>% group_by(id, condition, block) %>% summarise(choice=choice[1])

# MLM
m0.choice <- glmer(choice ~ 1 + (1|id), data=choice_data, family='binomial')
m1.choice <- glmer(choice ~ 0 + condition + (1|id), data=choice_data, family='binomial')
anova(m0.choice, m1.choice)
confint(m1.choice, method='Wald')



