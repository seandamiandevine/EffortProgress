
# Load --------------------------------------------------------------------
thisexp <- 'pool'
load(paste0('effprog2', thisexp,'.RData')) # change for experiment
myse <- function(x) sd(x, na.rm=T)/sqrt(length(x))
if(thisexp=='pool') d<-d.pool

# Switch costs ------------------------------------------------------------
png(paste0(thisexp, '/figures/switchcosts.png'))
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
this.x <- grconvertX(0.05, from="nfc", to="user")
this.y <- grconvertY(0.94,  from="nfc", to="user")
text(labels='a', x=this.x, y=this.y, xpd=T, cex=2, font=2)

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
this.x <- grconvertX(0.05, from="nfc", to="user")
this.y <- grconvertY(0.94,  from="nfc", to="user")
text(labels='c', x=this.x, y=this.y, xpd=T, cex=2, font=2)

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
this.x <- grconvertX(0.05, from="nfc", to="user")
this.y <- grconvertY(0.94,  from="nfc", to="user")
text(labels='b', x=this.x, y=this.y, xpd=T, cex=2, font=2)

# acc
y  <- tapply(d$acc, list(isprog, eff), mean, na.rm=T)
se <- tapply(d$acc, list(isprog, eff), myse)
thisbar <- 
  barplot(y, 
          beside=F, 
          ylab = '% Correct', 
          xlab = 'Swich Rate', 
          col = rep('white', nrow(y)), 
          ylim = range(pretty(c(y-se, y+se))), 
          xpd = F, 
          border = F,
          names.arg = c('10%', '50%', '90%'))
lines(thisbar, y[1,], type='b', pch = 16, cex=1.5)
arrows(thisbar, y[1,]-se[1,], thisbar,y[1,]+se[1,],code=3, angle = 90, length = 0.1)
lines(thisbar, y[2,], type='b', pch=1,cex=1.5)
arrows(thisbar, y[2,]-se[2,],thisbar, y[2,]+se[2,], code=3, angle = 90, length = 0.1)
this.x <- grconvertX(0.05, from="nfc", to="user")
this.y <- grconvertY(0.94,  from="nfc", to="user")
text(labels='d', x=this.x, y=this.y, xpd=T, cex=2, font=2)

legend('topright', legend = c('No Progress', 'Progress'), pch=c(16, 1))

dev.off()

# tlx ---------------------------------------------------------------------
png(paste0(thisexp, '/figures/tlx.png'))
y <- tapply(tlx$value, tlx$variable, mean)
se <- tapply(tlx$value, tlx$variable, myse)

thisbar <- 
  barplot(y, 
          xlab = 'Switch Rate', 
          ylab = 'Rating', 
          ylim = range(pretty(c(y-se, y+se))), 
          xpd = F, 
          names.arg = rep(paste0(c(10, 50, 90), '%'), 2),
          col = rep(rainbow(2, end=0.6), each=3), 
          main='TLX: Demand Subscale')
arrows(thisbar, y-se, thisbar, y+se, code=3, angle=90, length=0.1)
legend('topright',legend = c("Doesn't yield progress", "Does yield progress"), 
       fill=rainbow(2, end=0.6))
dev.off()

# Effort and Progress Preference -------------------------------------------------------
#X11()
png(paste0(thisexp, '/figures/hypotheses.png'), height = 10, width=6, res=300, units = 'in')
layout(matrix(c(1, 1, 2, 3, 4, 4),3, 2, byrow = T))
# h1
h1 <- fit_data[fit_data$prog_type %in% c('11', '00'), ]
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
          #main='Progress is held constant', 
          legend.text = T, 
          args.legend = list(title='Demand Pairs', 
                             legend=c('10%-50%', 
                                      '10%-90%', 
                                      '50%-90%'))
          )
arrows(thisbar, y-se, thisbar, y+se, code=3, angle=90, length=0.1)
abline(h=0.5, lty = 'dashed')
this.x <- grconvertX(0.05, from="nfc", to="user")
this.y <- grconvertY(0.94,  from="nfc", to="user")
text(labels='a', x=this.x, y=this.y, xpd=T, cex=2, font=2)

# h2 
# layout(matrix(c(1, 2), 1, 2))
h2 <- fit_data[fit_data$effort_type %in% c('11', '22', '33'), ]

# overall 
subprog = tapply(h2$progchoice, as.character(h2$id), mean, na.rm=T)
hist(subprog, 
     xlab = 'p(Choose Progress)', 
     ylab = 'Number of subjects', 
     xlim =c(0, 1), 
     main='')
abline(v=0.45, lty='dashed', lwd=3)

this.x <- grconvertX(0.10, from="nfc", to="user")
this.y <- grconvertY(0.94,  from="nfc", to="user")
text(labels='b', x=this.x, y=this.y, xpd=T, cex=2, font=2)

# by efflevel
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
this.x <- grconvertX(0.05, from="nfc", to="user")
this.y <- grconvertY(0.94,  from="nfc", to="user")
text(labels='c', x=this.x, y=this.y, xpd=T, cex=2, font=2)

# h3
# layout(matrix(1))
h3 <- fit_data[fit_data$prog_type %in% c('01', '10') & !is.na(fit_data$effchoice), ]
y  <- tapply(h3$effchoice, list(h3$prog_type, as.character(h3$effort_type)), mean)
colnames(y) <-  c('10%-50%', '10%-90%', '50%-90%')
# y <- matrix(y[c(1, 3, 2, 5, 4, 6)], 2, 3, 
#             dimnames = list(c('01', '10'), c('10%-50%', '10%-90%', '50%-90%')))
se  <- tapply(h3$effchoice, list(h3$prog_type, as.character(h3$effort_type)), myse)
# se  <- matrix(se[c(1, 3, 2, 5, 4, 6)], 2, 3, 
#             dimnames = list(c('01', '10'), c('10%-50%', '10%-90%', '50%-90%')))
colnames(se) <- c('10%-50%', '10%-90%', '50%-90%')

thisbar <- 
  barplot(y, 
          beside=T,
          xlab = 'Demand Pairing', 
          ylab = 'p(Choose High Demand)', 
          ylim = range(pretty(c(y-se, y+se+.1))), 
          xpd = F, 
          names.arg = c('10%-50%', '10%-90%', '50%-90%'), 
          #main='Demand and progress vary', 
          legend.text = T, 
          col = c('#1e2625', '#828c84'), 
          args.legend = list(legend = c('High demand yields progress', 'Low demand yields progress'))
          )
arrows(thisbar, y-se, thisbar, y+se, code=3, angle=90, length=0.1)
abline(h=0.5, lty = 'dashed')
this.x <- grconvertX(0.05, from="nfc", to="user")
this.y <- grconvertY(0.94,  from="nfc", to="user")
text(labels='d', x=this.x, y=this.y, xpd=T, cex=2, font=2)

dev.off()

# Hypotheses side-by-side ####
png(paste0(thisexp, '/figures/hypotheses_sidebyside.png'), height = 4, width=12, res=300, units = 'in')
# X11(height = 4, width=12)
layout(matrix(c(1, 2, 3), 1, 3, byrow = T))

# h1
h1 <- fit_data[fit_data$prog_type %in% c('11', '00'), ]
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
          #main='Progress is held constant', 
          legend.text = T, 
          args.legend = list(title='Demand Pairs', 
                             legend=c('10%-50%', 
                                      '10%-90%', 
                                      '50%-90%'))
  )
arrows(thisbar, y-se, thisbar, y+se, code=3, angle=90, length=0.1)
abline(h=0.5, lty = 'dashed')
this.x <- grconvertX(0.05, from="nfc", to="user")
this.y <- grconvertY(0.94,  from="nfc", to="user")
text(labels='a', x=this.x, y=this.y, xpd=T, cex=2, font=2)

# h2 
# by efflevel
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
this.x <- grconvertX(0.05, from="nfc", to="user")
this.y <- grconvertY(0.94,  from="nfc", to="user")
text(labels='b', x=this.x, y=this.y, xpd=T, cex=2, font=2)

# h3
h3 <- fit_data[fit_data$prog_type %in% c('01', '10') & !is.na(fit_data$effchoice), ]
y  <- tapply(h3$effchoice, list(h3$prog_type, as.character(h3$effort_type)), mean)
colnames(y) <-  c('10%-50%', '10%-90%', '50%-90%')
# y <- matrix(y[c(1, 3, 2, 5, 4, 6)], 2, 3, 
#             dimnames = list(c('01', '10'), c('10%-50%', '10%-90%', '50%-90%')))
se  <- tapply(h3$effchoice, list(h3$prog_type, as.character(h3$effort_type)), myse)
# se  <- matrix(se[c(1, 3, 2, 5, 4, 6)], 2, 3, 
#             dimnames = list(c('01', '10'), c('10%-50%', '10%-90%', '50%-90%')))
colnames(se) <- c('10%-50%', '10%-90%', '50%-90%')

thisbar <- 
  barplot(y, 
          beside=T,
          xlab = 'Demand Pairing', 
          ylab = 'p(Choose High Demand)', 
          ylim = range(pretty(c(y-se, y+se+.2))), 
          xpd = F, 
          names.arg = c('10%-50%', '10%-90%', '50%-90%'), 
          #main='Demand and progress vary', 
          legend.text = T, 
          col = c('#1e2625', '#828c84'), 
          args.legend = list(legend = c('High demand yields progress', 'Low demand yields progress'))
  )
arrows(thisbar, y-se, thisbar, y+se, code=3, angle=90, length=0.1)
abline(h=0.5, lty = 'dashed')
this.x <- grconvertX(0.05, from="nfc", to="user")
this.y <- grconvertY(0.94,  from="nfc", to="user")
text(labels='c', x=this.x, y=this.y, xpd=T, cex=2, font=2)

dev.off()


# Pairwise ####
pdf(paste0(thisexp, 'figures/perdeck.pdf', width = 10, height = 10))
layout(matrix(c(3, 4, 5, 6, 7, 
              2, 8, 9, 10, 11, 
              2, 2, 12, 13, 14, 
              2, 2, 1, 15, 16, 
              1, 1, 1, 1, 17), 5, 5, byrow = T))
plot(0, axes = F, type='n', xlab='', ylab='')
plot(0, axes = F, type='n', xlab='', ylab='')
legend('center',
       legend = c('Yields progress', 'Does not yield progress', 'Switch = 10%', 'Switch = 50%', 'Switch = 90%'),
       fill = c(progcol[2], progcol[1], NA, NA, NA), 
       density = c(100, 100, 20, 20, 20),
       angle =c(0, 0, effangle), cex=1.4)

pairs <- sort(unique(fit_data$pair))
effangle <- c(90, 135, 180)
progcol = rainbow(2, end=0.6)
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

# 2a2b2c plot ####
png('2a2b2c_hypo.png', width=10, height=10, units = 'in', res=300)
#X11(width = 7, height=8)
layout(matrix(1:9, 3, 3))
lettercount = 1

for(thisexp in c('a', 'b', 'c')) {
  
  load(paste0('effprog2', thisexp,'.RData'))
  
  # h1
  h1 <- fit_data[fit_data$prog_type %in% c('11', '00'), ]
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
            #main='Progress is held constant', 
            legend.text = ifelse(thisexp=='a', T, F), 
            args.legend = list(title='Demand Pairs', 
                               legend=c('10%-50%', 
                                        '10%-90%', 
                                        '50%-90%'))
    )
  arrows(thisbar, y-se, thisbar, y+se, code=3, angle=90, length=0.1)
  abline(h=0.5, lty = 'dashed')
  this.x <- grconvertX(0.05, from="nfc", to="user")
  this.y <- grconvertY(0.94,  from="nfc", to="user")
  text(labels=letters[lettercount], x=this.x, y=this.y, xpd=T, cex=2, font=2)
  lettercount=lettercount+1
  
  # column title 
  text(labels=paste0('Exp.2', thisexp), 
       x=grconvertX(0.5, from="nfc", to="user"), 
       y=grconvertY(0.93, from="nfc", to="user"), xpd=T, cex=3, font=2)
  
  # h2 
  # by efflevel
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
  this.x <- grconvertX(0.05, from="nfc", to="user")
  this.y <- grconvertY(0.94,  from="nfc", to="user")
  text(labels=letters[lettercount], x=this.x, y=this.y, xpd=T, cex=2, font=2)
  lettercount = lettercount+1
  
  # h3
  h3 <- fit_data[fit_data$prog_type %in% c('01', '10') & !is.na(fit_data$effchoice), ]
  y  <- tapply(h3$effchoice, list(h3$prog_type, as.character(h3$effort_type)), mean)
  colnames(y) <-  c('10%-50%', '10%-90%', '50%-90%')
  se  <- tapply(h3$effchoice, list(h3$prog_type, as.character(h3$effort_type)), myse)
  colnames(se) <- c('10%-50%', '10%-90%', '50%-90%')
  thisbar <- 
    barplot(y, 
            beside=T,
            xlab = 'Demand Pairing', 
            ylab = 'p(Choose High Demand)', 
            ylim = c(0.2, 0.8), 
            xpd = F, 
            names.arg = c('10%-50%', '10%-90%', '50%-90%'), 
            #main='Demand and progress vary', 
            legend.text = ifelse(thisexp=='a', T, F), 
            col = c('#1e2625', '#828c84'), 
            args.legend = list(legend = c('High demand yields progress', 'Low demand yields progress'))
    )
  arrows(thisbar, y-se, thisbar, y+se, code=3, angle=90, length=0.1)
  abline(h=0.5, lty = 'dashed')
  this.x <- grconvertX(0.05, from="nfc", to="user")
  this.y <- grconvertY(0.94,  from="nfc", to="user")
  text(labels=letters[lettercount], x=this.x, y=this.y, xpd=T, cex=2, font=2)
  lettercount=lettercount+1
  
}

dev.off()
