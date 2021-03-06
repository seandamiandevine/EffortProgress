# SETUP ####
library(lme4); library(lmerTest); library(emmeans)
library(dplyr); library(reshape2)
library(ggplot2); library(gridExtra); library(grid); library(ggpubr); 
library(sjPlot)
rm(list=ls()) # clear all

# LOAD AND CLEAN ####
setwd("~/Desktop/Ongoing/EffortProg/Exp2/a/analysis/")
files = list.files(path = 'full/data/', pattern = '.csv')
d <- do.call("rbind", lapply(paste0('full/data/',files) , read.csv))

# * CLEAN AND COMPUTE USEFUL VARIABLES ####
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

# * DESCRIPTIVES PRE-EXCLUSION ####
descpre <- 
  d  %>% 
  mutate(age = ifelse(as.numeric(age) > 120, NA, as.numeric(age))) %>% 
  summarise(n = length(unique(id)), 
            mage = mean(age, na.rm = T),
            sdage = sd(age, na.rm = T), 
            msex = mean(ifelse(tolower(gender) == 'male', 1, 0)))


# * EXCLUSION ####
rtcutoff = 200
acccutoff = 0.6
d <- d[d$id %in%
         (d %>%
            group_by(id) %>%
            summarise(mrt = mean(switchrt, na.rm=T),
                      macc = mean(acc, na.rm = T)) %>%
            filter(mrt > rtcutoff & macc > acccutoff))$id, ]

# * DESCRIPTIVES POST-EXCLUSION ####
descpost <- 
  d  %>% 
  summarise(n = length(unique(id)), 
            mage = mean(as.numeric(age), na.rm = T),
            sdage = sd(as.numeric(age), na.rm = T), 
            msex = mean(ifelse(tolower(gender) == 'male', 1, 0)))

# MANIPULATION CHECK ####
# * ANALYSIS ####
manipdat <- 
  d %>% 
  mutate(corrt = ifelse(acc==1, switchrt, NA), 
         chosendeck = ifelse(deckchoice == 76, deckl, deckr), 
         chosendeck = as.numeric(gsub('deck', '', chosendeck)), 
         isprog = factor(ifelse(chosendeck %in% 4:6, 1, 0)), 
         efflevel = factor(match(as.numeric(chosendeck) %% 3, 1:6 %% 3))) 
m0.rt <- lmer(corrt ~ 1 + (1|decisionnum) + (1|id), data=manipdat)
m1.rt <- lmer(corrt ~ switch + (1|decisionnum) + (1|id), data=manipdat)
m2.rt <- lmer(corrt ~ switch+isprog + (1|decisionnum) + (1|id), data=manipdat)
m3.rt <-  lmer(corrt ~ switch*isprog + (1|decisionnum) + (1|id), data=manipdat)

anova(m1.rt, m2.rt, m3.rt)

m0.acc <- glmer(acc ~ 1 + (1|decisionnum) + (1|id), data=manipdat, family="binomial")
m1.acc <- glmer(acc ~ switch + (1|decisionnum) + (1|id), data=manipdat, family="binomial")
m2.acc <- glmer(acc ~ switch+isprog + (1|decisionnum) + (1|id), data=manipdat, family="binomial")
m3.acc <- glmer(acc ~ switch*isprog + (1|decisionnum) + (1|id), data=manipdat, family="binomial")

anova(m1.acc, m2.acc, m3.acc)

# Make HTML Table for Switch Costs 
tab_model(m3.rt, m3.acc,
          pred.labels = c('Intercept', 'Trial Type', 'Progress Cond.', 'Trial Type x Progress Cond.'),
          dv.labels = c('Correct RT', 'Accuracy'),
          show.icc = F, show.re.var = F,
          show.stat = T, string.stat = 'z',
          string.ci = '95% CI', file='figures/full/switchcosttable.html')

# * VISUALISE ####
sc.p1 <- 
  d %>% 
  group_by(id, switch) %>%
  mutate(corrt = ifelse(acc==1, switchrt, NA)) %>%
  summarise(meanRT = mean(as.numeric(corrt), na.rm =T), 
            meanACC = mean(acc, na.rm = T)) %>% 
  filter(is.na(switch)==F) %>% 
  melt(id.vars=c('id',  'switch')) %>% 
  ggplot(aes(x=switch,y=value)) + 
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  geom_point(alpha=0.2) +
  geom_line(aes(group=id), alpha=0.2) +
  facet_wrap(~variable, scales = 'free_y', 
             strip.position = "left", 
             labeller = as_labeller(c(meanRT = "Correct RT (in ms.)", meanACC = "% Correct")))  +
  labs(x = '', y='', fill = 'condition') + 
  theme_classic() +     
  theme(strip.background = element_blank(), strip.placement = "outside", 
        plot.title = element_text(hjust=0.5), 
        legend.position='none') 

# by deck 
sc.p2 <- 
  d %>% 
  mutate(corrt = ifelse(acc==1, switchrt, NA),
         chosendeck = ifelse(deckchoice == 76, deckl, deckr), 
         tmp = as.numeric(gsub('deck', '', chosendeck)), 
         sr = ifelse(tmp %% 3 == 1, '0.1', 
                     ifelse(tmp %% 3 == 2, '0.5', 
                            0.9))) %>% 
  group_by(id, chosendeck, sr, switch) %>%
  summarise(meanRT = mean(as.numeric(corrt), na.rm =T), 
            meanACC = mean(acc, na.rm = T)) %>% 
  filter(is.na(switch)==F) %>% 
  melt(id.vars=c('id',  'switch', 'chosendeck', 'sr')) %>% 
  ggplot(aes(x=switch,y=value, fill=sr)) + 
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  geom_point(alpha=0.2) +
  geom_line(aes(group=id), alpha=0.2) +
  facet_grid(variable ~ chosendeck, scales = 'free_y', 
             labeller = labeller(
               chosendeck = c('1' = 'No Progress', '2' = 'No Progress', '3' = 'No Progress', 
                              '4' = 'Progress', '5' = 'Progress', '6' = 'Progress'),
               variable = c(`meanRT` = "Correct RT (in ms.)", `meanACC` = "% Correct"))) +
  labs(x = '', y='', fill = 'Switch Rate', title='Switch costs by deck') + 
  theme_classic() +     
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_fill_brewer(palette="Accent")

sc.p3 <- 
  d %>% 
  mutate(corrt = ifelse(acc==1, switchrt, NA), 
         chosendeck = ifelse(deckchoice == 76, deckl, deckr), 
         chosendeck = as.numeric(gsub('deck', '', chosendeck)), 
         isprog = factor(ifelse(chosendeck %in% 4:6, 'Progress', 'No Progress')), 
         efflevel = factor(match(as.numeric(chosendeck) %% 3, 1:6 %% 3)), 
         efflevel = ifelse(efflevel == 1, '10%', ifelse(efflevel == 2, '50%', '90%'))) %>%
  group_by(id, efflevel, isprog) %>% 
  summarise(meanRT = mean(as.numeric(corrt), na.rm =T), 
            meanACC = mean(acc, na.rm = T)) %>% 
  melt(id.vars=c('id',  'efflevel', 'isprog')) %>% 
  ggplot(aes(x = efflevel, y = value, color = isprog)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.2) + 
  stat_summary(aes(y = value,group = isprog), fun.y=mean, geom="line") + 
  facet_wrap(~variable, scales = 'free_y', 
             strip.position = "left", 
             labeller = as_labeller(c(meanRT = "Correct RT (in ms.)", meanACC = "% Correct"))) +
  labs(x = 'Switch Rates', y='', color = 'Progress Condition') + 
  theme_classic() + 
  theme(strip.background = element_blank(), strip.placement = "outside", 
        legend.position = 'bottom') + 
  scale_color_brewer(palette = 'Set2')


ggarrange(sc.p2, labels='a',
          ggarrange(sc.p1, sc.p3, labels = c('b', 'c')),
          nrow=2) %>% 
  ggexport(filename = 'figures/full/switchcosts.png', height = 500, width = 850)


# MAIN HYPOTHESES ####
# * ANALYSE ####
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
         prog_type = ifelse(prog_l==prog_r, paste0(prog_l, prog_r), ifelse(eff_l < eff_r, '01', '10')), 
         chosendeck = ifelse(deckchoice==76, deckl, deckr), 
         progchoice = ifelse(chosendeck %in% 1:3, 0, 1)) %>% 
  select(id, decisionnum, deckl, deckr, pair, deckchoice, effort_type, prog_type,
         effchoice, progchoice)

fit_data$effort_type = factor(fit_data$effort_type)

# * * H1 AND H3 #### 
# most complex causes convergence: try different optimizers, use best one
# af = allFit(h1.h3_main_m3)
h1.h3_main_m0 <- glmer(effchoice ~ 1 + (1|id), data=fit_data, family = 'binomial')
h1.h3_main_m1 <- glmer(effchoice ~ 0 + effort_type + (1|id), data=fit_data, family = 'binomial')
#h1.h3_main_m2 <- glmer(effchoice ~ 0 + effort_type+prog_type + (1|id), data=fit_data, family = 'binomial')
h1.h3_main_m2 <- glmer(effchoice ~ 0 + effort_type:prog_type + (1|id), data=fit_data, family = 'binomial', glmerControl(optimizer = 'bobyqa'))

anova(h1.h3_main_m0, h1.h3_main_m1)
anova(h1.h3_main_m0, h1.h3_main_m2)

# * * H2 ####
h2_main_m0 <- glmer(progchoice ~ 1 + (1|id), family = 'binomial', data=fit_data[is.na(fit_data$effchoice), ])
h2_main_m1 <- glmer(progchoice ~ 0+effort_type + (1|id), family = 'binomial', data=fit_data[is.na(fit_data$effchoice), ])

anova(h2_main_m0, h2_main_m1)

# * VISUALISE ####
progresslabels = list('NP-NP' = 'Neither deck yields progress', 
                      'P-P' = 'Both decks yields progress', 
                      'P-NP' = 'One deck yields progress, but the other does not')
resp.h1 <-
  d %>% 
  group_by(id, decisionnum) %>% 
  mutate(eff_l = match(as.numeric(gsub('deck', '', deckl)) %% 3, 1:6 %% 3), 
         eff_r =  match(as.numeric(gsub('deck', '', deckr)) %% 3, 1:6 %% 3), 
         effchoice = ifelse(eff_l == eff_r, NA, 
                            ifelse(deckchoice == 76 & eff_l > eff_r, 1, 
                                   ifelse(deckchoice == 82 & eff_r > eff_l, 1, 
                                          0))), 
         tmpl = ifelse(eff_l == 1, '10%', ifelse(eff_l == 2, '50%', '90%')), 
         tmpr = ifelse(eff_r == 1, '10%', ifelse(eff_r == 2, '50%', '90%')),
         effpair = factor(paste0(min(tmpl, tmpr),'-', max(tmpl, tmpr)), 
                          levels = c('10%-50%', '50%-90%', '10%-90%'))) %>% 
  ungroup() %>% 
  group_by(id, decision_type, effpair) %>%
  summarise(phigheff = mean(effchoice, na.rm=T)) %>% 
  filter(effpair != '10%-10%', effpair != '50%-50%', effpair != '90%-90%', 
         decision_type != 'P-NP') %>% 
  {
    .$decision_type = sapply(.$decision_type, function(x) progresslabels[[as.character(x)]])
    .
  } %>% 
  ggplot(aes(x = decision_type, y=phigheff, fill = effpair, group = effpair)) +
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data= mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  #geom_point(position=position_dodge(0.9), alpha = 0.2) + 
  geom_hline(yintercept = 0.5, linetype='dotted', size=1.2) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim=c(0, .8)) + 
  scale_fill_brewer(palette = 'Set1') + 
  labs(x = '', y = '% High Effort Deck Chosen', fill = 'Switch Rate Pairings') + 
  theme_classic()

# progress preference in p-np condition 
resp.h2.1 <-
  d %>% 
  group_by(id, decisionnum) %>% 
  mutate(eff_l = match(as.numeric(gsub('deck', '', deckl)) %% 3, 1:6 %% 3), 
         eff_r =  match(as.numeric(gsub('deck', '', deckr)) %% 3, 1:6 %% 3), 
         tmpl = ifelse(eff_l == 1, '10%', ifelse(eff_l == 2, '50%', '90%')), 
         tmpr = ifelse(eff_r == 1, '10%', ifelse(eff_r == 2, '50%', '90%')),
         effpair = paste0(min(tmpl, tmpr),'-', max(tmpl, tmpr)), 
         chosendeck = ifelse(deckchoice == 76, deckl, deckr), 
         chosendeck = as.numeric(gsub('deck', '', chosendeck)), 
         progchosen = ifelse(chosendeck %in% 4:6, 1, 0)) %>% 
  ungroup() %>% 
  group_by(id, decision_type, effpair) %>%
  summarise(pprog = mean(progchosen, na.rm=T)) %>% 
  filter(effpair != '10%-50%', effpair != '50%-90%', effpair != '10%-90%') %>% 
  {
    .$decision_type = sapply(.$decision_type, function(x) progresslabels[[as.character(x)]])
    .
  } %>% 
  ggplot(aes(x = decision_type, y=pprog, fill = effpair, group = effpair)) +
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data= mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  #geom_point(position=position_dodge(0.9), alpha = 0.2) + 
  geom_hline(yintercept = 0.5, linetype='dotted', size=1.2) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_fill_brewer(palette = 'Dark2') + 
  labs(x = '', y = '% Progress Deck Chosen', fill = 'Switch Rate Pairings') + 
  theme_classic() 

# overall histogram
resp.h2.2 <- 
  d %>% 
  filter(decision_type == 'P-NP') %>% 
  mutate(chosendeck = ifelse(deckchoice == 76, deckl, deckr), 
         chosendeck = as.numeric(gsub('deck', '', chosendeck)), 
         progchosen = ifelse(chosendeck %in% 4:6, 1, 0)) %>% 
  group_by(id, decisionnum) %>%
  slice(1) %>% 
  ungroup() %>% group_by(id) %>% 
  summarise(p_prog = mean(progchosen)) %>% 
  ggplot(aes(x = p_prog)) + 
  geom_histogram(binwidth = 0.1, colour='black', fill='gray') + 
  geom_vline(xintercept = 0.5, linetype = 'dotted', size = 1.2) + 
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = scales::percent_format(accuracy = 1)) +
  labs(x = '% Progress Choice in N-P Condition', y = 'Number of Participants') + 
  theme_classic()

resp.h3 <- 
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
           match(as.numeric(substr(pair, 1, 1)) %% 3, 1:6 %% 3), 
           match(as.numeric(substr(pair, 3, 3)) %% 3, 1:6 %% 3)
         ),
         prog_l = ifelse(substr(pair, 1, 1) %in% c('1', '2', '3'), 0, 1), 
         prog_r = ifelse(substr(pair, 3, 3) %in% c('1', '2', '3'), 0, 1),
         prog_type = paste0(prog_l, prog_r), 
         chosendeck = ifelse(deckchoice==76, deckl, deckr), 
         progchoice = ifelse(chosendeck %in% 1:3, 0, 1)) %>% 
  select(id, decisionnum, deckl, deckr, pair, deckchoice, effort_type, prog_type,
         effchoice, progchoice) %>% 
  ungroup() %>% 
  filter(prog_type=='01', is.na(effchoice)==F) %>%
  mutate(isprogeasy = factor(ifelse(as.numeric(substring(effort_type, 1, 1)) > 
                                      as.numeric(substring(effort_type, 2, 2)), 
                                    'Low Demand Deck Yield Progress', 'High Demand Deck Yields Progress')),
         effort_type = paste0(c('10%', '50%', '90%')[as.numeric(substring(effort_type, 1, 1))], 
                              '-', 
                              c('10%', '50%', '90%')[as.numeric(substring(effort_type, 2, 2))]) 
  ) %>% 
  group_by(1:n()) %>% 
  mutate(effort_type = paste0(min(as.numeric(substring(effort_type, 1, 2)), as.numeric(substring(effort_type, 5, 6))), 
                              '%-',
                              max(as.numeric(substring(effort_type, 1, 2)), as.numeric(substring(effort_type, 5, 6))),
                              '%')) %>% 
  group_by(id, effort_type, isprogeasy) %>% 
  summarise(phigheff = mean(effchoice)) %>% 
  ggplot(aes(x = effort_type, y = phigheff, fill=isprogeasy)) + 
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data= mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  #geom_point(position=position_dodge(0.9), alpha = 0.2) + 
  geom_hline(yintercept = 0.5, linetype='dotted', size=1.2) + 
  labs(x = 'Switch Rate Pairing', y = '% High Effort Deck Chosen', 
       fill = '') + 
  scale_fill_brewer(palette = 'Accent') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  theme_classic() 


# behaviour in each pair
pairs <- 
  sort(unique(
    (d %>% 
       group_by(decisionnum) %>% 
       summarise(deckl = deckl[1], 
                 deckr = deckr[1], 
                 pair = paste0(
                   min(as.numeric(gsub("deck", "", deckl)), as.numeric(gsub("deck", "", deckr))), 
                   '-',
                   max(as.numeric(gsub("deck", "", deckl)), as.numeric(gsub("deck", "", deckr)))))
    )$pair))
plots = list()
for(i in 1:length(pairs)) { 
  
  deck1 = LETTERS[as.numeric(substring(pairs[i], 1, 1))]
  deck2 = LETTERS[as.numeric(substring(pairs[i], 3, 3))]
  
  p <- 
    d %>% 
    group_by(id, decisionnum) %>% 
    summarise(deckl = deckl[1], 
              deckr = deckr[1], 
              chosendeck = ifelse(deckchoice[1] == 76, gsub("deck", "", deckl), gsub("deck", "", deckr)), 
              pair = paste0(
                min(as.numeric(gsub("deck", "", deckl)), as.numeric(gsub("deck", "", deckr))), 
                '-',
                max(as.numeric(gsub("deck", "", deckl)), as.numeric(gsub("deck", "", deckr))))) %>% 
    filter(pair == pairs[i]) %>% 
    group_by(id, pair) %>% 
    summarise(!!deck1 := sum(chosendeck==sub("\\-.*", "", pairs[i]))/length(chosendeck), 
              !!deck2 := sum(chosendeck==sub("*.\\-", "", pairs[i]))/length(chosendeck)) %>% 
    ungroup() %>% 
    melt(id.vars=c('id', 'pair')) %>% 
    ggplot(aes(x = variable, y = value)) + 
    stat_summary(fun.y = mean, geom = 'bar', position=position_dodge(width=.9), size=2) + 
    stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.2, position=position_dodge(width=.9)) + 
    #geom_point(alpha=0.2) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                       limits = c(0, 0.8)) + 
    labs(x = '', y='', 
         title=paste0(deck1, ' vs. ', deck2)) +
    theme_bw() + theme(plot.title = element_text(hjust=0.5))
  
  plots[[i]] = p
}

# Make reference grid for figure 4
ref <- data.frame(
  Deck = LETTERS[1:6], 
  Switch = rep(c('10%', '50%', '90%'), 2), 
  Progress = c(rep('No', 3), rep('Yes', 3))
)

blank = ggplot() + theme_void()
reftable = ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 100) +
  annotation_custom(tableGrob(ref, rows = NULL), xmin=0, xmax=10, ymin=0, ymax=100) + 
  theme_void()


plots = list(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], 
             blank,      plots[[6]], plots[[7]], plots[[8]], plots[[9]], 
             blank,      blank,      plots[[10]], plots[[11]], plots[[12]], 
             blank,      reftable,      blank,       plots[[13]], plots[[14]], 
             blank,      blank,      blank,       blank,       plots[[15]])

# Forrest plot 
labs = c('A vs. B', 'A vs. C', 'B vs. C', 'A vs. E', 'A vs. F', 'B vs. D', 'B vs. F', 'C vs. D', 'C vs. E', 'D vs. E', 'D vs. F', 'E vs. F', 'A vs. D', 'B vs. E', 'C vs. F')

fp = plot_models(h1.h3_main_m2, h2_main_m1,
            m.labels = c('Demand Preference', 'Progress Preference'),
            show.p = T,
            show.values = T,
            value.size = 3.5,
            vline.color = 'grey') +
  theme_bw() +
  labs(x = 'Deck Pairing') +
  theme(legend.justification = c(1, 0),
        legend.position = c(1, 0),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

fp$data$term = labs
tablelabs = fp$data$term # save for later
fp = fp + scale_x_discrete(limits = rev(levels(factor(sort(fp$data$term), levels = sort(labs)))))
tbl = tableGrob(ref, rows = NULL)

# Save images 
ggarrange(
  resp.h1,
  ggarrange(resp.h2.1, resp.h2.2, ncol = 2, labels = c("b", "c")),
  nrow = 2,
  labels = "a") %>% 
  ggexport(filename = 'figures/full/H1_H2.png', width = 650)

ggexport(resp.h3, filename = 'figures/H3.png')

ggsave(grid.arrange(grobs = plots, 
                    nrow = 5,
                    ncol = 5,
                    left=textGrob("% of Trials Deck Was Chosen", 
                                  gp=gpar(fontsize=22), rot = 90)),
       filename = 'figures/full/p_deck.png', height = 7, width = 10)


ggsave(grid.arrange(fp, tbl, nrow = 1, widths= c(1.25, 0.5)), 
         filename = 'figures/full/forrestplot.png', height = 7, width = 10)

# * FORMATTED REG. RABLE ####
tab_model(h1.h3_m, h2_m, 
          pred.labels = tablelabs, 
          dv.labels = c('Effort Choice', 'Progress Choice'), 
          string.pred = 'Deck Pairing', 
          show.icc = F, show.re.var = F, 
          show.stat = T, string.stat = 'z', 
          string.ci = '95% CI', 
          file = 'figures/full/MLMtable.html') 


# MISC ####
# * TLX ####
tlx <- d %>% 
  select(id, contains('tlx_')) %>%
  melt(id.vars='id') %>% 
  group_by(id, variable) %>% 
  slice(1) %>% 
  mutate(demand = factor(c(10, 50, 90, 10, 50, 90)[as.numeric(gsub('tlx_deck', '', variable))]), 
         variable = LETTERS[as.numeric(gsub('tlx_deck', '', variable))], 
         prog = ifelse(variable %in% c('A', 'B', 'C'), 'No Progress', 'Progress'))

tlxplot <- 
  tlx %>% 
  ggplot(aes(x = variable, y = value, fill = prog)) + 
  stat_summary(fun.y = mean, geom = 'bar', position=position_dodge(width=.9), size=2) + 
  stat_summary(fun.data = mean_se, geom = 'errorbar', width = 0.2, position=position_dodge(width=.9)) + 
  geom_point(alpha=0.2) + 
  labs(x = 'Deck', y = 'Score', fill = '', title='TLX Subscale: Demand') +
  scale_fill_brewer(palette = 'Set2') + 
  theme_bw() + 
  theme(plot.title = element_text(hjust=0.5), 
        legend.position = 'bottom')

tlx.m0 <- lmer(value ~ 1 + (1|id), data=tlx)
tlx.m1 <- lmer(value~demand + (1|id), data=tlx)
tlx.m2 <- lmer(value~demand+prog + (1|id), data=tlx)
tlx.m3 <- lmer(value~demand*prog + (1|id), data=tlx)
anova(tlx.m3, tlx.m2, tlx.m1, tlx.m0)

# * NFC ####
fitdata_nfc = 
  full_join(fit_data, d[,c(1, 27:44)]) %>% 
  mutate_at(vars(starts_with("nfc")),funs(as.numeric)) %>% 
  mutate(nfc3 = 4 - nfc3, 
         nfc4 = 4 - nfc4, 
         nfc5 = 4 - nfc5, 
         nfc7 = 4 - nfc7, 
         nfc8 = 4 - nfc8, 
         nfc9 = 4 - nfc9, 
         nfc12 = 4 - nfc12, 
         nfc16 = 4 - nfc16, 
         nfc17 = 4 - nfc17) %>% 
  mutate(sumnfc = rowSums(select(., starts_with("nfc")), na.rm = TRUE))

nfcplot <- 
  fitdata_nfc %>% 
  filter(!is.na(effchoice)) %>% 
  mutate(effort_type= ifelse(effort_type=='12'|effort_type=='21', '50%-90%',
                             ifelse(effort_type=='13' | effort_type=='31', '10%-90%', 
                                    '90%-90%')), 
         prog_type = ifelse(prog_type=='00', 'NP-NP', ifelse(prog_type=='01', 'P-NP', 'P-P'))) %>% 
  group_by(id, sumnfc, effort_type, prog_type) %>% 
  summarise(meff = mean(effchoice)) %>% 
  ggplot(aes(x = sumnfc, y = meff)) + 
  geom_point(alpha=0.5) + 
  geom_smooth(method='lm', se=F, colour='red') + 
  facet_grid(prog_type~effort_type) + 
  labs(x = 'Cum. NFC Score', y = '% High Effort Deck Chosen') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1)) +  
  theme_bw()
  
nfc_h1_plot <- 
  fitdata_nfc %>% 
  filter(prog_type!='01') %>% 
  mutate(nfc_bin = factor(ntile(sumnfc, 2), labels = c('low NFC', 'high NFC')), 
         prog_type = ifelse(prog_type=='00', 
                            'Neither deck\nyields progress', 
                            'Both decks\nyields progress'), 
         effort_type = ifelse(effort_type=='12', '10%-50%', 
                              ifelse(effort_type=='13', '10%-90%', 
                                     '50%-90%'))) %>% 
  group_by(id, sumnfc, nfc_bin, prog_type, effort_type) %>%
  summarise(phigh = mean(effchoice)) %>% 
  ggplot(aes(x = prog_type, y = phigh, fill = effort_type)) + 
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data= mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  geom_hline(yintercept = 0.5, linetype='dotted', size=1.2) + 
  facet_wrap(~nfc_bin) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim=c(0, .8)) + 
  scale_fill_brewer(palette = 'Set1') + 
  labs(x = '', y = '% High Effort\nDeck Chosen', fill = 'Switch Rate Pairings') + 
  theme_classic()

nfc_h2_plot <- 
  fitdata_nfc %>% 
  filter(is.na(effchoice)) %>% 
  mutate(nfc_bin = factor(ntile(sumnfc, 2), labels = c('low NFC', 'high NFC')), 
         prog_type = 'One deck yields progress,\nbut the other does not', 
         effort_type = ifelse(effort_type=='11', '10%-10%', 
                              ifelse(effort_type=='22', '50%-50%', 
                                     '90%-90%'))) %>% 
  group_by(id, sumnfc, nfc_bin, prog_type, effort_type) %>%
  summarise(pprog = mean(progchoice)) %>% 
  ggplot(aes(x = prog_type, y = pprog, fill = effort_type)) + 
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data= mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  geom_hline(yintercept = 0.5, linetype='dotted', size=1.2) + 
  facet_wrap(~nfc_bin) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim=c(0, .8)) + 
  scale_fill_brewer(palette = 'Dark2') + 
  labs(x = '', y = '% Progress\nDeck Chosen', fill = 'Switch Rate Pairings') + 
  theme_classic()

nfc_h3_plot <- 
  fitdata_nfc %>% 
  filter(prog_type=='01', !effort_type %in% c('11', '22', '33')) %>% 
  mutate(nfc_bin = factor(ntile(sumnfc, 2), labels = c('low NFC', 'high NFC')), 
         prog_type = ifelse(effort_type %in% c('12', '23', '13'), 
                            'High Demand\nYields Progress', 
                            'Low Demand\nYields Progress'), 
         effort_type = ifelse(effort_type=='12' | effort_type == '21', '10%-50%', 
                              ifelse(effort_type=='23' | effort_type=='32', '50%-90%', 
                                     '10%-90%'))) %>% 
  group_by(id, sumnfc, nfc_bin, prog_type, effort_type) %>%
  summarise(phigh = mean(effchoice)) %>% 
  ggplot(aes(x = effort_type, y = phigh, fill = prog_type)) + 
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data= mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  geom_hline(yintercept = 0.5, linetype='dotted', size=1.2) + 
  facet_wrap(~nfc_bin) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim = c(0, 0.8)) + 
  scale_fill_brewer(palette = 'Accent') + 
  labs(x = 'Switch Rate Pairing', y = '% High Effort\nDeck Chosen', fill = '') + 
  theme_classic()

ggarrange(nfc_h1_plot, nfc_h2_plot, nfc_h3_plot, labels = letters[1:3], 
          nrow = 3)

# h1.h3_main_m3_nfc <- glmer(effchoice ~  effort_type * prog_type + sumnfc + (1|id), data=fitdata_nfc, family = 'binomial')

# * INTOLERANCE TO UNCERTAINTY  ####
# see scoring and factors here: 
# https://www.phenxtoolkit.org/protocols/view/650701#:~:text=Description,the%20inability%20to%20take%20action.
fitdata_ius = 
  d %>% 
  select(id, contains('uncertainty')) %>% 
  group_by(id) %>% 
  slice(1) %>% 
  mutate(factor1 = sum(uncertainty1, uncertainty2, uncertainty3, 
                       uncertainty9, uncertainty12, uncertainty13, 
                       uncertainty14, uncertainty15, uncertainty16, 
                       uncertainty17, uncertainty20, uncertainty22, 
                       uncertainty23, uncertainty24, uncertainty25), 
         factor2 = sum(uncertainty4, uncertainty5, uncertainty6, 
                       uncertainty7, uncertainty8, uncertainty10, 
                       uncertainty11, uncertainty18, uncertainty19, 
                       uncertainty21, uncertainty26, uncertainty27), 
         all = factor1+factor2) %>% 
  full_join(fit_data)


iusplot_f1 <- 
  fitdata_ius %>% 
  filter(!is.na(effchoice)) %>% 
  mutate(effort_type= ifelse(effort_type=='12'|effort_type=='21', '50%-90%',
                             ifelse(effort_type=='13' | effort_type=='31', '10%-90%', 
                                    '90%-90%')), 
         prog_type = ifelse(prog_type=='00', 'NP-NP', ifelse(prog_type=='01', 'P-NP', 'P-P'))) %>% 
  group_by(id, all, factor1, factor2, effort_type, prog_type) %>% 
  summarise(meff = mean(effchoice)) %>% 
  melt(id.vars = c('id', 'effort_type', 'prog_type', 'meff')) %>% 
  ggplot(aes(x = value, y = meff, colour=variable)) + 
  geom_point(alpha=0.1) + 
  geom_smooth(method='lm', se=F) + 
  facet_grid(prog_type~effort_type) + 
  labs(x = 'Cum. IUS Score',
       y = '% High Effort Deck Chosen', 
       colour = 'Factor') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1)) +  
  scale_colour_manual(values = c("#D55E00", "#CC79A7", "#0072B2"), 
                      labels = c('Total Score',
                                 'Uncertainty has negative behavioural and self-referent implications',
                                 'Uncertainty is unfair and spoils everything')) + 
  theme_bw() + 
  theme(legend.position = 'bottom')
  
ius_h1_plot <- 
  fitdata_ius %>% 
  filter(prog_type!='01') %>% 
  mutate(ius_bin = factor(ntile(all, 2), labels = c('low IUS', 'high IUS')), 
         prog_type = ifelse(prog_type=='00', 
                            'Neither deck\nyields progress', 
                            'Both decks\nyields progress'), 
         effort_type = ifelse(effort_type=='12', '10%-50%', 
                              ifelse(effort_type=='13', '10%-90%', 
                                     '50%-90%'))) %>% 
  group_by(id, all, ius_bin, prog_type, effort_type) %>%
  summarise(phigh = mean(effchoice)) %>% 
  ggplot(aes(x = prog_type, y = phigh, fill = effort_type)) + 
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data= mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  geom_hline(yintercept = 0.5, linetype='dotted', size=1.2) + 
  facet_wrap(~ius_bin) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim=c(0, .8)) + 
  scale_fill_brewer(palette = 'Set1') + 
  labs(x = '', y = '% High Effort\nDeck Chosen', fill = 'Switch Rate Pairings') + 
  theme_classic()

ius_h2_plot <- 
  fitdata_ius %>% 
  filter(is.na(effchoice)) %>% 
  mutate(ius_bin = factor(ntile(all, 2), labels = c('low IUS', 'high IUS')), 
         prog_type = 'One deck yields progress,\nbut the other does not', 
         effort_type = ifelse(effort_type=='11', '10%-10%', 
                              ifelse(effort_type=='22', '50%-50%', 
                                     '90%-90%'))) %>% 
  group_by(id, all, ius_bin, prog_type, effort_type) %>%
  summarise(pprog = mean(progchoice)) %>% 
  ggplot(aes(x = prog_type, y = pprog, fill = effort_type)) + 
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data= mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  geom_hline(yintercept = 0.5, linetype='dotted', size=1.2) + 
  facet_wrap(~ius_bin) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim=c(0, .8)) + 
  scale_fill_brewer(palette = 'Dark2') + 
  labs(x = '', y = '% Progress\nDeck Chosen', fill = 'Switch Rate Pairings') + 
  theme_classic()

ius_h3_plot <- 
  fitdata_ius %>% 
  filter(prog_type=='01', !effort_type %in% c('11', '22', '33')) %>% 
  mutate(ius_bin = factor(ntile(all, 2), labels = c('low IUS', 'high IUS')), 
         prog_type = ifelse(effort_type %in% c('12', '23', '13'), 
                            'High Demand\nYields Progress', 
                            'Low Demand\nYields Progress'), 
         effort_type = ifelse(effort_type=='12' | effort_type == '21', '10%-50%', 
                              ifelse(effort_type=='23' | effort_type=='32', '50%-90%', 
                                     '10%-90%'))) %>% 
  group_by(id, all, ius_bin, prog_type, effort_type) %>%
  summarise(phigh = mean(effchoice)) %>% 
  ggplot(aes(x = effort_type, y = phigh, fill = prog_type)) + 
  stat_summary(fun.y = mean, geom = "bar", position = position_dodge(width = 0.9)) + 
  stat_summary(fun.data= mean_se, geom = "errorbar", width=0.2, position = position_dodge(width=0.9)) + 
  geom_hline(yintercept = 0.5, linetype='dotted', size=1.2) + 
  facet_wrap(~ius_bin) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  coord_cartesian(ylim = c(0, 0.8)) + 
  scale_fill_brewer(palette = 'Accent') + 
  labs(x = 'Switch Rate Pairing', y = '% High Effort\nDeck Chosen', fill = '') + 
  theme_classic()

ggarrange(ius_h1_plot, ius_h2_plot, ius_h3_plot, labels = letters[1:3], 
          nrow = 3)
