
# Load --------------------------------------------------------------------

load('/home/sdevine/Desktop/Ongoing/EffortProg/Exp1/Analysis/Exp1.RData')

# Switch costs -------------------------------------------------------------
pdf('baser_plots/switchcosts.pdf', width = 6, height = 6.5)
#X11( width = 6, height = 6)
layout(matrix(1:4, 2, 2, byrow = T))
par('mar' = c(5, 4, 3, 2))

# per block
# rt
d.cor <- d[d$acc==1, ]
y  <- tapply(d.cor$rt, list(d.cor$difficulty, d.cor$condition), mean, na.rm=T)
colnames(y) <- c('No Progress', 'Progress')
pts <- tapply(d.cor$rt, list(as.character(d.cor$id), d.cor$condition, d.cor$difficulty), mean, na.rm=T)
se <- c(sd(pts[,1,1], na.rm = T)/sqrt(length(pts[,1,1])), 
        sd(pts[,2,1], na.rm = T)/sqrt(length(pts[,2,1])), 
        sd(pts[,1,2], na.rm = T)/sqrt(length(pts[,1,2])), 
        sd(pts[,2,2], na.rm = T)/sqrt(length(pts[,2,2])))

thisbar <- 
  barplot(y,
          beside=T,
          xlab = '', 
          ylab  = 'Mean Correct RT (in ms.)',
          ylim = range(c(0, pretty(pts)+600)), 
          legend.text = T, 
          args.legend = list(x = 'topleft', legend=c('Low', 'High'), title='Demand', ncol=2))
arrows(thisbar, y-se, thisbar, y+se, angle = 90, code=3, length=0.1)
points(rep(thisbar[1,1], length(pts[,1,1])), pts[,1,1], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[2,1], length(pts[,1,2])), pts[,1,2], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[1,2], length(pts[,2,1])), pts[,2,1], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[2,2], length(pts[,2,2])), pts[,2,2], col=rgb(red=0, blue=0, green=0, alpha = 0.1))

# acc
y  <- tapply(d$acc, list(d$difficulty, d$condition), mean, na.rm=T)
colnames(y) <- c('No Progress', 'Progress')
pts <- tapply(d$acc, list(d$id, d$condition, d$difficulty), mean, na.rm=T)
se <- c(sd(pts[,1,1], na.rm = T)/sqrt(length(pts[,1,1])), 
        sd(pts[,2,1], na.rm = T)/sqrt(length(pts[,2,1])), 
        sd(pts[,1,2], na.rm = T)/sqrt(length(pts[,1,2])), 
        sd(pts[,2,2], na.rm = T)/sqrt(length(pts[,2,2])))

thisbar <- 
  barplot(y,
          beside=T,
          xlab = '', 
          ylab  = '%Correct',
          ylim = range(c(0, pretty(pts))))
arrows(thisbar, y-se, thisbar, y+se, angle = 90, code=3, length=0.1)
points(rep(thisbar[1,1], length(pts[,1,1])), pts[,1,1], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[2,1], length(pts[,1,2])), pts[,1,2], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[1,2], length(pts[,2,1])), pts[,2,1], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[2,2], length(pts[,2,2])), pts[,2,2], col=rgb(red=0, blue=0, green=0, alpha = 0.1))

mtext('Per Block', side=3, line=-2, outer=T, font=2, cex = 1)

# per trial
# rt
d.cor <- d[d$acc==1, ]
y  <- tapply(d.cor$rt, list(d.cor$switch, d.cor$condition), mean, na.rm=T)
colnames(y) <- c('No Progress', 'Progress')
pts <- tapply(d.cor$rt, list(as.character(d.cor$id), d.cor$switch, d.cor$difficulty), mean, na.rm=T)
se <- c(sd(pts[,1,1], na.rm = T)/sqrt(length(pts[,1,1])), 
        sd(pts[,2,1], na.rm = T)/sqrt(length(pts[,2,1])), 
        sd(pts[,1,2], na.rm = T)/sqrt(length(pts[,1,2])), 
        sd(pts[,2,2], na.rm = T)/sqrt(length(pts[,2,2])))

thisbar <- 
  barplot(y,
          beside=T,
          xlab = '', 
          ylab  = 'Mean Correct RT (in ms.)',
          ylim = range(c(0, pretty(pts)+600)), 
          legend.text = T, 
          args.legend = list(x = 'topleft', legend=c('repeat', 'switch'), title='Trial Type', ncol=2))
arrows(thisbar, y-se, thisbar, y+se, angle = 90, code=3, length=0.1)
points(rep(thisbar[1,1], length(pts[,1,1])), pts[,1,1], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[2,1], length(pts[,1,2])), pts[,1,2], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[1,2], length(pts[,2,1])), pts[,2,1], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[2,2], length(pts[,2,2])), pts[,2,2], col=rgb(red=0, blue=0, green=0, alpha = 0.1))

# acc
y  <- tapply(d$acc, list(d$switch, d$condition), mean, na.rm=T)
colnames(y) <- c('No Progress', 'Progress')
pts <- tapply(d$acc, list(d$id, d$condition, d$switch), mean, na.rm=T)
se <- c(sd(pts[,1,1], na.rm = T)/sqrt(length(pts[,1,1])), 
        sd(pts[,2,1], na.rm = T)/sqrt(length(pts[,2,1])), 
        sd(pts[,1,2], na.rm = T)/sqrt(length(pts[,1,2])), 
        sd(pts[,2,2], na.rm = T)/sqrt(length(pts[,2,2])))

thisbar <- 
  barplot(y,
          beside=T,
          xlab = '', 
          ylab  = '% Correct',
          ylim = range(c(0, pretty(pts))))
arrows(thisbar, y-se, thisbar, y+se, angle = 90, code=3, length=0.1)
points(rep(thisbar[1,1], length(pts[,1,1])), pts[,1,1], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[2,1], length(pts[,1,2])), pts[,1,2], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[1,2], length(pts[,2,1])), pts[,2,1], col=rgb(red=0, blue=0, green=0, alpha = 0.1))
points(rep(thisbar[2,2], length(pts[,2,2])), pts[,2,2], col=rgb(red=0, blue=0, green=0, alpha = 0.1))

mtext('Per Trial', line=-21, outer=T, font=2, cex = 1)

dev.off()

# TLX ---------------------------------------------------------------------
#pdf('tlx.pdf', width=10, height = 5)
X11(width=8, height = 5)
layout(matrix(1:5, 1, 5, byrow = T))
subscales = names(tlx)[-c(1:3)]
cond = ifelse(tlx$condition==1, 'Progress', 'No Progress')
demand = factor(ifelse(tlx$diff=='easy', 'Low', 'High'), levels = c('Low', 'High'))
for(s in 1:length(subscales)) { 
  thisscale = subscales[s]
  y  <- tapply(tlx[,thisscale], list(demand, cond), mean)
  se <- tapply(tlx[,thisscale], list(demand, cond), function(x) sd(x)/sqrt(length(x)))
  
  if(s == 1) l = T else l = F
  thisbar <- 
    barplot(y,
            xlab = '', 
            ylab = 'Subjective Rating', 
            ylim = c(5, 8), 
            xpd = F, 
            beside = T, 
            legend.text = l, 
            args.legend = list(x='topleft', title='Obj. Demand'), 
            main = thisscale)
  arrows(thisbar, y-se, thisbar, y+se, angle = 90, code=3, length=0.1)
  }

dev.off()

# Effort preference -------------------------------------------------------
pdf('demandpref.pdf', height=8, width = 5)
layout(matrix(1))
d.choices <- d[is.na(d$switch), ]
demandchoice = ifelse(d.choices$difficulty=='hard', 0, 1)
prog = ifelse(d.choices$condition==0, 'No Progress', 'Progress')
y <- tapply(demandchoice, prog, mean)
se <- tapply(demandchoice, prog, function(x) sd(x)/sqrt(length(x)))

thisbar <- 
  barplot(y*100,
          ylab = '% Low Demand Chosen', 
          ylim = c(40, 60), 
          xpd=F, 
          main='Demand Preference')
arrows(thisbar, (y-se)*100, thisbar, (y+se)*100, angle = 90, code=3, length=0.1)
abline(h = 50, lty='dashed', lwd=2)

dev.off()

