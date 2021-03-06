
# Load --------------------------------------------------------------------

load('~/Desktop/Ongoing/EffortProg/Exp2/b/effprog2b.RData')
myse <- function(x) sd(x, na.rm=T)/sqrt(length(x))

# Switch costs ------------------------------------------------------------
pdf('switchcosts.pdf')
layout(matrix(c(1, 2, 3, 4), 2, 2))
# overall
d.cor <- d[d$acc==1, ]
y  <- tapply(d.cor$switchrt, d.cor$switch, mean, na.rm=T)
pts <- tapply(d.cor$switchrt, list(d.cor$id, d.cor$switch), mean, na.rm=T)
se <- c(sd(pts[,1], na.rm = T)/sqrt(length(pts[,1])), 
        sd(pts[,2], na.rm = T)/sqrt(length(pts[,1])))

thisbar <- 
  barplot(y,
          xlab = 'Trial Type', 
          ylab  = 'Mean Correct RT (in ms.)',
          ylim = range(c(0, pretty(pts))))
arrows(thisbar, y-se, thisbar, y+se, angle = 90, code=3, length=0.1)
points(rep(thisbar[1], length(pts[,1])), pts[,1], col=rgb(red=0, blue=0, green=0, alpha = 0.3))
points(rep(thisbar[2], length(pts[,2])), pts[,2], col=rgb(red=0, blue=0, green=0, alpha = 0.3))

# acc
y  <- tapply(d$acc, d$switch, mean, na.rm=T)
pts <- tapply(d$acc, list(d$id, d$switch), mean, na.rm=T)
se <- c(sd(pts[,1], na.rm = T)/sqrt(length(pts[,1])), 
        sd(pts[,2], na.rm = T)/sqrt(length(pts[,1])))

thisbar <- 
  barplot(y,
          xlab = 'Trial Type', 
          ylab  = '% Correct',
          ylim = range(c(0, pretty(pts))))
arrows(thisbar, y-se, thisbar, y+se, angle = 90, code=3, length=0.1)
points(rep(thisbar[1], length(pts[,1])), pts[,1], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[2], length(pts[,2])), pts[,2], col=rgb(red=0, blue=0, green=0, alpha = 0.1))

# per difficulty
noprogdecks = c('deck1', 'deck2', 'deck3')
isprog <- c()
eff <- c()
for(i in 1:nrow(d)){
  if(is.na(d$deckchoice[i])) next
  if(d$deckchoice[i]==76) { 
    switch (d$deckl[i],
            'deck1' = {
              isprog[i] = 0
              eff[i] = 1
            }, 
            'deck2' = {
              isprog[i] = 0
              eff[i] = 2
            }, 
            'deck3' = {
              isprog[i] = 0
              eff[i] = 3
            }, 
            'deck4' = {
              isprog[i] = 1
              eff[i] = 1
            }, 
            'deck5' = {
              isprog[i] = 1
              eff[i] = 2
            }, 
            'deck6' = {
              isprog[i] = 1
              eff[i] = 3
            }
    )
  } else {
    switch (d$deckr[i],
            'deck1' = {
              isprog[i] = 0
              eff[i] = 1
            }, 
            'deck2' = {
              isprog[i] = 0
              eff[i] = 2
            }, 
            'deck3' = {
              isprog[i] = 0
              eff[i] = 3
            }, 
            'deck4' = {
              isprog[i] = 1
              eff[i] = 1
            }, 
            'deck5' = {
              isprog[i] = 1
              eff[i] = 2
            }, 
            'deck6' = {
              isprog[i] = 1
              eff[i] = 3
            }
    )
  }
}
# rt 
isprogcor <- isprog[d$acc==1]
effcor <- eff[d$acc==1]
y  <- tapply(d.cor$switchrt, list(isprogcor, effcor), mean, na.rm=T)
se <- tapply(d.cor$switchrt, list(isprogcor, effcor), myse)
thisbar <- 
  barplot(y, 
          beside=F, 
          ylab = 'Mean Correct RT (in ms.)', 
          xlab = 'Swich Rate', 
          col = rep('white', nrow(y)), 
          ylim = range(pretty(y)), 
          xpd = F, 
          border = F,
          names.arg = c('10%', '50%', '90%'))
lines(thisbar, y[1,], type='b', pch = 16, cex=1.5)
arrows(thisbar, y[1,]-se[1,], thisbar,y[1,]+se[1,],code=3, angle = 90, length = 0.1)
lines(thisbar, y[2,], type='b', pch=1,cex=1.5)
arrows(thisbar, y[2,]-se[2,],thisbar, y[2,]+se[2,], code=3, angle = 90, length = 0.1)

# acc
y  <- tapply(d$acc, list(isprog, eff), mean, na.rm=T)
se <- tapply(d$acc, list(isprog, eff), myse)
thisbar <- 
  barplot(y, 
          beside=F, 
          ylab = '% Correct', 
          xlab = 'Swich Rate', 
          col = rep('white', nrow(y)), 
          ylim = range(pretty(y)), 
          xpd = F, 
          border = F,
          names.arg = c('10%', '50%', '90%'))
lines(thisbar, y[1,], type='b', pch = 16, cex=1.5)
arrows(thisbar, y[1,]-se[1,], thisbar,y[1,]+se[1,],code=3, angle = 90, length = 0.1)
lines(thisbar, y[2,], type='b', pch=1,cex=1.5)
arrows(thisbar, y[2,]-se[2,],thisbar, y[2,]+se[2,], code=3, angle = 90, length = 0.1)

legend('topright', legend = c('No Progress', 'Progress'), pch=c(16, 1))

dev.off()

# tlx ---------------------------------------------------------------------
pdf('tlx.pdf')
y <- tapply(tlx$value, tlx$variable, mean)
se <- tapply(tlx$value, tlx$variable, myse)

thisbar <- 
  barplot(y, 
          xlab = 'Deck', 
          ylab = 'Rating', 
          ylim = range(pretty(c(y-se, y+se))), 
          xpd = F, 
          names.arg = LETTERS[1:length(y)],
          col = rep(c('#9cacbf', '#032e42'), each=3), 
          main='TLX: Demand Subscale')
arrows(thisbar, y-se, thisbar, y+se, code=3, angle=90, length=0.1)
legend('topright',legend = c("Doesn't yield progress", "Does yield progress"), 
       fill=c('#9cacbf', '#032e42'))
dev.off()

# Effort Preference -------------------------------------------------------
pdf('hypotheses.pdf')
# h1
h1 <- fit_data[fit_data$prog_type %in% c('11', '00') & !is.na(fit_data$effchoice), ]
y  <- tapply(h1$effchoice, list(as.character(h1$effort_type), h1$prog_type), mean)
se <- tapply(h1$effchoice, list(as.character(h1$effort_type), h1$prog_type), myse)

thisbar <- 
  barplot(y, 
          beside=T, 
          xlab = '', 
          ylab = 'p(Choose High Demand)', 
          ylim = c(0,1), 
          xpd = F, 
          names.arg = c('neither deck yields progress', 'both decks yield progress'), 
          main='Progress is held constant', 
          legend.text = T, 
          args.legend = list(title='Demand Pairs', 
                             legend=c('10%-50%', 
                                      '10%-90%', 
                                      '50%-90%'))
  )
arrows(thisbar, y-se, thisbar, y+se, code=3, angle=90, length=0.1)
abline(h=0.5, lty = 'dashed')

# h2 
# a. 
layout(matrix(c(1, 2), 1, 2))
h2 <- fit_data[fit_data$effort_type %in% c('11', '22', '33'), ]
y  <- tapply(h2$progchoice, as.character(h2$effort_type), mean)
se <- tapply(h2$progchoice, as.character(h2$effort_type), myse)

thisbar <- 
  barplot(y, 
          xlab = 'Demand Pairing', 
          ylab = 'p(Choose Progress)', 
          ylim = c(0,1), 
          xpd = F, 
          names.arg = c('10%-10%', '50%-50%', '90%-90%'))
arrows(thisbar, y-se, thisbar, y+se, code=3, angle=90, length=0.1)
abline(h=0.5, lty = 'dashed')

# b. 
subprog = tapply(h2$progchoice, h2$id, mean, na.rm=T)
hist(subprog, 
     xlab = 'p(Choose Progress)', 
     ylab = 'Number of subjects', 
     xlim =c(0, 1), 
     main='')
abline(v=0.45, lty='dashed', lwd=3)
mtext('Demand is held constant', line=-2, outer=T, font=2, cex = 1)

# h3
layout(matrix(1))
h3 <- fit_data[fit_data$prog_type %in% c('01', '10') & !is.na(fit_data$effchoice), ]
y  <- tapply(h3$effchoice, list(h3$prog_type, as.character(h3$effort_type)), mean)
se  <- tapply(h3$effchoice, list(h3$prog_type, as.character(h3$effort_type)), myse)

thisbar <- 
  barplot(y, 
          beside=T,
          xlab = 'Demand Pairing', 
          ylab = 'p(Choose High Demand)', 
          ylim = range(pretty(c(y-se, y+se))), 
          xpd = F, 
          names.arg = c('10%-50%', '10%-90%', '50%-90%'), 
          main='Demand and progress vary', 
          legend.text = T, 
          col = c('#1e2625', '#828c84'), 
          args.legend = list(legend = c('High demand yields progress', 'Low demand yields progress'))
  )
arrows(thisbar, y-se, thisbar, y+se, code=3, angle=90, length=0.1)
abline(h=0.5, lty = 'dashed')

dev.off()

# all pairs 
pdf('perdeck.pdf', width = 10, height = 10)
layout(matrix(c(3, 4, 5, 6, 7, 
                2, 8, 9, 10, 11, 
                2, 2, 12, 13, 14, 
                2, 2, 1, 15, 16, 
                1, 1, 1, 1, 17), 5, 5, byrow = T))
plot(0, axes = F, type='n', xlab='', ylab='')
plot(0, axes = F, type='n', xlab='', ylab='')
legend('center',
       legend = c('Yields progress', 'Does not yield progress', 'Switch = 10%', 'Switch = 50%', 'Switch = 90%'),
       fill = c(progcol, NA, NA, NA), 
       density = c(100, 100, 20, 20, 20),
       angle =c(0, 0, effangle), cex=1.4)

pairs <- sort(unique(fit_data$pair))
effangle <- c(90, 135, 180)
progcol = c('#9cacbf', '#032e42')
for(p in 1:length(pairs)) {
  d.pair <- fit_data[fit_data$pair==pairs[p], ]
  deckl <- as.numeric(strsplit(pairs[p], '-')[[1]][1])
  deckr <- as.numeric(strsplit(pairs[p], '-')[[1]][2])
  effl <- match(deckl %% 3, 1:6 %% 3)
  effr <- match(deckr %% 3, 1:6 %% 3)
  progl <- ifelse(deckl %in% 1:3, 1, 2)
  progr <- ifelse(deckr %in% 1:3, 1, 2)
  chosendeck <- c()
  for(i in 1:nrow(d.pair)){
    if(d.pair$deckl[i]==paste0('deck', deckl)){
      chosendeck[i] = ifelse(d.pair$deckchoice[i]==76, deckl, deckr)
    } else if(d.pair$deckl[i]==paste0('deck', deckr)) {
      chosendeck[i] = ifelse(d.pair$deckchoice[i]==76, deckr, deckl)
    } else if(d.pair$deckr[i]==paste0('deck', deckr)) {
      chosendeck[i] = ifelse(d.pair$deckchoice[i]==76, deckl, deckr)
    } else if(d.pair$deckr[i]==paste0('deck', deckl)) {
      chosendeck[i] = ifelse(d.pair$deckchoice[i]==76, deckr, deckl)
    }
  }
  y <- c(mean(chosendeck==deckl), mean(chosendeck==deckr))
  se <- c(myse(chosendeck==deckl), myse(chosendeck==deckr))
  x <- c(LETTERS[deckl], 
         LETTERS[deckr])
  thisbar <- 
    barplot(height=y, names=x, 
            xlab = 'Deck', 
            ylab = 'p(Choose)', 
            ylim = c(0.2, 0.8), 
            xpd = F, 
            col = c(progcol[progl], progcol[progr]),
            density = 20, 
            angle = c(effangle[effl], effangle[effr]))
  arrows(thisbar, y-se, thisbar, y+se, angle=90, code=3, length = 0.1)
  abline(h = '0.5', lty='dashed')
}
dev.off()
