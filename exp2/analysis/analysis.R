# Setup ####
library(lme4); library(lmerTest); library(emmeans)
library(dplyr); library(reshape2)
library(sjPlot)
rm(list=ls()) # clear all

# Load + clean ####
thisexp <- 'b'
files = list.files(path = paste0(thisexp, '/data/'), pattern = '.csv')
d <- do.call("rbind", lapply(paste0(thisexp, '/data/',files) , read.csv))

# * Clean and compute useful variables ####
d <- 
  d %>% 
  filter(run != 'learning') %>% 
  mutate(switch = ifelse(subtrial == 1, NA,
                         ifelse(task == lag(task), 'repeat', 'switch')), 
         acc = ifelse(switchresp == '69' & task == 'magnitude' & probe < 5, 1, 
                      ifelse(switchresp == '73' & task == 'magnitude' & probe > 5, 1, 
                             ifelse(switchresp == '69' & task == 'parity' & probe %% 2 == 0, 1, 
                                    ifelse(switchresp == '73' & task == 'parity' & probe %% 2 != 0, 1, 
                                           0)))))

# * Descriptives pre-exclusion ####
descpre <- 
  d  %>% 
  mutate(age = ifelse(as.numeric(age) > 120, NA, as.numeric(age))) %>% 
  summarise(n = length(unique(id)), 
            mage = mean(age, na.rm = T),
            sdage = sd(age, na.rm = T), 
            msex = mean(ifelse(tolower(gender) == 'male', 1, 0), na.rm=T))


# * exclusion ####
rtcutoff = 200
acccutoff = 0.6
d <- d[d$id %in%
         (d %>%
            group_by(id) %>%
            summarise(mrt = mean(switchrt, na.rm=T),
                      macc = mean(acc, na.rm = T)) %>%
            filter(mrt > rtcutoff & macc > acccutoff))$id, ]

# * Desciptives post-exclusion ####
descpost <- 
  d  %>% 
  mutate(age=ifelse(as.numeric(age) > 1900, 2021-as.numeric(age), age)) %>% 
  summarise(n = length(unique(id)), 
            mage = mean(as.numeric(age), na.rm = T),
            sdage = sd(as.numeric(age), na.rm = T), 
            msex = mean(ifelse(tolower(gender) == 'male', 1, 0), na.rm = T))

# Manipulation Checks ####
# * Switch Costs ####
manipdat <- 
  d %>% 
  mutate(corrt = ifelse(acc==1, switchrt, NA), 
         chosendeck = ifelse(deckchoice == 76, deckl, deckr), 
         chosendeck = as.numeric(gsub('deck', '', chosendeck)), 
         isprog = factor(ifelse(chosendeck %in% 4:6, 1, 0)), 
         efflevel = factor(match(as.numeric(chosendeck) %% 3, 1:6 %% 3))) %>% 
  filter(!is.na(switch))

#trial
m0.rt.t <- lmer(corrt ~ 1 + (1|decisionnum) + (1|id), data=manipdat, REML = F)
m1.rt.t <- lmer(corrt ~ switch + (1|decisionnum) + (1|id), data=manipdat, REML = F)
m2.rt.t <- lmer(corrt ~ switch+isprog + (1|decisionnum) + (1|id), data=manipdat, REML = F)
m3.rt.t <- lmer(corrt ~ switch*isprog + (1|decisionnum) + (1|id), data=manipdat, REML = F)
m4.rt.t <- lmer(corrt ~ 0 + switch*efflevel + (1|decisionnum) + (1|id), data=manipdat, REML = F)
anova(m0.rt.t, m1.rt.t, m2.rt.t, m3.rt.t, m4.rt.t)
anova(m1.rt.t, m4.rt.t)

m0.acc.t <- glmer(acc ~ 1 + (1|decisionnum) + (1|id), data=manipdat, family='binomial')
m1.acc.t <- glmer(acc ~ switch + (1|decisionnum) + (1|id), data=manipdat, family='binomial')
m2.acc.t <- glmer(acc ~ switch+isprog + (1|decisionnum) + (1|id), data=manipdat, family='binomial')
m3.acc.t <- glmer(acc ~ switch*isprog + (1|decisionnum) + (1|id), data=manipdat, family='binomial')
anova(m0.acc.t, m1.acc.t, m2.acc.t, m3.acc.t)

# block
m0.rt.b <- lmer(corrt ~ 1 + (1|id), data=manipdat, REML = F)
m1.rt.b <- lmer(corrt ~ efflevel + (1|id), data=manipdat, REML = F)
m2.rt.b <- lmer(corrt ~ isprog+efflevel + (1|id), data=manipdat, REML = F)
m3.rt.b <- lmer(corrt ~ isprog*efflevel + (1|id), data=manipdat, REML = F)
anova(m0.rt.b, m1.rt.b, m2.rt.b, m3.rt.b)

m0.acc.b <- glmer(acc ~ 1 + (1|id), data=manipdat, family='binomial')
m1.acc.b <- glmer(acc ~ efflevel  + (1|id), data=manipdat, family='binomial')
m2.acc.b <- glmer(acc ~ isprog+efflevel + (1|id), data=manipdat, family='binomial')
m3.acc.b <- glmer(acc ~ isprog*efflevel + (1|id), data=manipdat, family='binomial')
anova(m0.acc.b, m1.acc.b, m2.acc.b, m3.acc.b)

# Make HTML Table for Switch Costs 
# tab_model(m2.rt, m2.acc,
#           pred.labels = c('Intercept', 'Trial Type', 'Progress Cond.', 'Trial Type x Progress Cond.'),
#           dv.labels = c('Correct RT', 'Accuracy'),
#           show.icc = F, show.re.var = F,
#           show.stat = T, string.stat = 'z',
#           string.ci = '95% CI', file='figures/full/switchcosttable.html')

# * TLX ####
tlx <- d %>% 
  select(id, contains('tlx_')) %>%
  melt(id.vars='id') %>% 
  group_by(id, variable) %>% 
  slice(1) %>% 
  mutate(demand = factor(c(10, 50, 90, 10, 50, 90)[as.numeric(gsub('tlx_deck', '', variable))]), 
         variable = LETTERS[as.numeric(gsub('tlx_deck', '', variable))], 
         prog = ifelse(variable %in% c('A', 'B', 'C'), 'No Progress', 'Progress'))

tlx.m0 <- lmer(value ~ 1 + (1|id), data=tlx)
tlx.m1 <- lmer(value~demand + (1|id), data=tlx)
tlx.m2 <- lmer(value~demand+prog + (1|id), data=tlx)
tlx.m3 <- lmer(value~demand*prog + (1|id), data=tlx)
anova(tlx.m3, tlx.m2, tlx.m1, tlx.m0)

# Main hypotheses ####
fit_data <- 
  d %>% 
  group_by(id, decisionnum) %>% 
  summarise(deckl = deckl[1], 
            deckr = deckr[1], 
            deckchoice = deckchoice[1], 
            pair = paste0(
              min(as.numeric(gsub("deck", "", deckl)), as.numeric(gsub("deck", "", deckr))), 
              '-',
              max(as.numeric(gsub("deck", "", deckl)), as.numeric(gsub("deck", "", deckr))))) %>% 
  group_by(id, decisionnum) %>% 
  mutate(eff_l = match(as.numeric(gsub('deck', '', deckl)) %% 3, 1:6 %% 3),
         eff_r =  match(as.numeric(gsub('deck', '', deckr)) %% 3, 1:6 %% 3),
         effchoice = ifelse(eff_l == eff_r, NA,
                            ifelse(deckchoice == 76 & eff_l > eff_r, 1,
                                   ifelse(deckchoice == 82 & eff_r > eff_l, 1,
                                          0))), 
         effort_type = paste0(
           min(match(as.numeric(substr(pair, 1, 1)) %% 3, 1:6 %% 3),
                match(as.numeric(substr(pair, 3, 3)) %% 3, 1:6 %% 3)), 
           max(match(as.numeric(substr(pair, 1, 1)) %% 3, 1:6 %% 3),
               match(as.numeric(substr(pair, 3, 3)) %% 3, 1:6 %% 3))
           ),
         prog_l = ifelse(substr(pair, 1, 1) %in% c('1', '2', '3'), 0, 1), 
         prog_r = ifelse(substr(pair, 3, 3) %in% c('1', '2', '3'), 0, 1),
         prog_type = ifelse(prog_l==prog_r, paste0(prog_l, prog_r), 
                            ifelse(match(as.numeric(substr(pair, 1, 1)) %% 3, 1:6 %% 3) > 
                                   match(as.numeric(substr(pair, 3, 3)) %% 3, 1:6 %% 3), '10','01')),  
         chosendeck = ifelse(deckchoice==76, deckl, deckr), 
         progchoice = ifelse(chosendeck %in% 1:3, 0, 1)) %>% 
  select(id, decisionnum, deckl, deckr, pair, deckchoice, effort_type, prog_type,
         effchoice, progchoice)

fit_data$effort_type = factor(fit_data$effort_type)

# * * H1 AND H3 at once
# most complex causes convergence: try different optimizers, use best one
# af = allFit(h1.h3_main_m3)
# h1.h3_main_m0 <- glmer(effchoice ~ 1 + (1|id), data=fit_data, family = 'binomial')
# h1.h3_main_m1 <- glmer(effchoice ~ 0 + effort_type + (1|id), data=fit_data, family = 'binomial')
# #h1.h3_main_m2 <- glmer(effchoice ~ 0 + effort_type+prog_type + (1|id), data=fit_data, family = 'binomial')
# h1.h3_main_m2 <- glmer(effchoice ~ 0 + effort_type:prog_type + (1|id), data=fit_data, family = 'binomial', glmerControl(optimizer = 'bobyqa'))
# anova(h1.h3_main_m0, h1.h3_main_m1)
# anova(h1.h3_main_m0, h1.h3_main_m2)

# * * H1 ####
h1.0 <- glmer(effchoice ~ 1 + (1|id), data=fit_data[fit_data$prog_type %in% c('00', '11'), ], family = 'binomial')
h1.1 <- glmer(effchoice ~ 0 + effort_type + (1|id), data=fit_data[fit_data$prog_type %in% c('00', '11'), ], family = 'binomial')
h1.2 <- glmer(effchoice ~ 0 + effort_type:prog_type + (1|id), data=fit_data[fit_data$prog_type %in% c('00', '11'), ], family = 'binomial', glmerControl(optimizer = 'bobyqa'))

anova(h1.0, h1.1)
anova(h1.1, h1.2)

# * * H2 ####
h2.0 <- glmer(progchoice ~ 1 + (1|id), family = 'binomial', data=fit_data[is.na(fit_data$effchoice), ])
h2.1 <- glmer(progchoice ~ 0+effort_type + (1|id), family = 'binomial', data=fit_data[is.na(fit_data$effchoice), ])

anova(h2.0, h2.1)

# * * H3 ####
h3.0 <- glmer(effchoice ~ 1 + (1|id), data=fit_data[fit_data$prog_type %in% c('01', '10'), ], family = 'binomial')
h3.1 <- glmer(effchoice ~ 0 + effort_type + (1|id), data=fit_data[fit_data$prog_type %in% c('01', '10'), ], family = 'binomial')
h3.2 <- glmer(effchoice ~ 0 + effort_type+prog_type + (1|id), data=fit_data[fit_data$prog_type %in% c('01', '10'), ], family = 'binomial')
h3.3 <- glmer(effchoice ~ 0 + effort_type:prog_type + (1|id), data=fit_data[fit_data$prog_type %in% c('01', '10'), ], family = 'binomial')

anova(h3.0, h3.1)
anova(h3.0, h3.2)
anova(h3.2, h3.3)

# * Reg. Table ####
tablelabs = c('A vs. B', 'A vs. C', 'B vs. C', 'D vs. E', 'D vs. F', 'E vs. F', # h1 
              'A vs. D', 'B vs. E', 'C vs. F',                                  # h2 
              'A vs. E', 'A vs. F', 'B vs. F', 'B vs. D', 'C vs. D', 'C vs. E') # h3
tab_model(h1.2, h2.1, h3.3, 
          pred.labels = tablelabs, 
          dv.labels = c('Effort Choice', 'Progress Choice'), 
          string.pred = 'Deck Pairing', 
          show.icc = T, show.re.var = T, 
          show.stat = T, string.stat = 'z', 
          string.ci = '95% CI')
          #file = 'figures/full/MLMtable.html') 