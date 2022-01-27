## ---- echo=FALSE, cache=FALSE-------------------------------------------------------------------------------
library(knitr)
options(tikzMetricsDictionary="tikzDictionary",
        tikzMetricPackages = c("\\usepackage[utf8]{inputenc}","\\usepackage[T1]{fontenc}",
                               "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
opts_chunk$set(fig.width=6.5, fig.height=8, cache=FALSE, message=FALSE,
               dev='tikz', external=FALSE,
               out.width=6.5, out.height=8)
# Fixed issue with dev='tikz' in Windows
# by setting external=FALSE and a new plot hook, as here:
# https://github.com/yihui/tikzDevice/issues/60
knit_hooks$set(crop = NULL, plot = function(x, options) {
  if ('tikz' %in% options$dev && !options$external) {
    hook_plot_tex(x, options)
  } else hook_plot_md(x, options)
})


## -----------------------------------------------------------------------------------------------------------
library(RankingProject)
data(TravelTime2011.1dec)
USdata <- TravelTime2011.1dec
head(USdata)

n = nrow(USdata)
alpha = 0.1
Z = qnorm(1-alpha/2)
Z.Indep = qnorm(1-(1-(1-alpha)^(1/n))/2) # around 3.081
USdata$IndepCiLo = with(USdata, round(Estimate.1dec - Z.Indep/Z*MOE.1dec, 1))
USdata$IndepCiHi = with(USdata, round(Estimate.1dec + Z.Indep/Z*MOE.1dec, 1))
attach(USdata)

## We could have used a Bonferroni correction instead.
## Not run:
# Z.Bonf = qnorm(1-alpha/(n*2)) # around 3.096
# USdata$BonfCiLo = with(USdata, round(Estimate.1dec - Z.Bonf/Z*MOE.1dec, 1))
# USdata$BonfCiHi = with(USdata, round(Estimate.1dec + Z.Bonf/Z*MOE.1dec, 1))


## ---- eval=FALSE--------------------------------------------------------------------------------------------
## par(xpd = TRUE, mar = c(6.3, 2.8, 0.3, 0.3) + 0.1)
## plot(c(0, n+1), c(0, n), type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='',
##      xaxs = 'i', yaxs = 'i')
## text(-3.5, n, "$r_k$", cex = 1, pos = 4)
## wd = 0.5; ht = 0.5
## for(ii in seq(1, n-2, by = 6)){
##   polygon(c(1-wd, n+wd, n+wd, 1-wd),
##           c(ii-ht, ii-ht, ii+ht+2, ii+ht+2),
##           border = NA, col = "grey90")
## }
## for(ii in 1:n){
##   SigDiffLo = sum(IndepCiHi <= IndepCiLo[ii])
##   SigDiffHi = sum(IndepCiLo >= IndepCiHi[ii])
## 
##   NotSigDiff = (SigDiffLo+1):(n-SigDiffHi)
##   mycex = 0.5
##   ## Add text
##   text(ii, (1:n)[NotSigDiff], Abbreviation[ii], cex = mycex, family = "mono", font = 2)
##   ## Draw box
##   wd = .5
##   ht = .5
##   polygon(c(ii-wd, ii+wd, ii+wd, ii-wd),
##           c(Rank[ii]-ht, Rank[ii]-ht, Rank[ii]+ht, Rank[ii]+ht),
##           border = NA, col = "grey20")
##   text(ii, Rank[ii], Abbreviation[ii], cex = mycex, family = "mono", font = 2, col = "white")
## }
## axis(1, at = 1:n, labels = FALSE)
## text(1:n + 0.5, par("usr")[3] - 2.0, labels = State, srt = 45, pos = 2, xpd = TRUE, cex = 0.7)
## axis(2, at = 1:n, las = 2, cex.axis = 0.7)


## ----joint-fig1, fig.width=8.5, fig.height=5.5, echo=FALSE--------------------------------------------------
par(xpd = TRUE, mar = c(6.3, 2.8, 0.3, 0.3) + 0.1)
plot(c(0, n+1), c(0, n), type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='',
     xaxs = 'i', yaxs = 'i')
text(-3.5, n, "$r_k$", cex = 1, pos = 4)
wd = 0.5; ht = 0.5
for(ii in seq(1, n-2, by = 6)){
  polygon(c(1-wd, n+wd, n+wd, 1-wd),
          c(ii-ht, ii-ht, ii+ht+2, ii+ht+2),
          border = NA, col = "grey90")
}
for(ii in 1:n){
  SigDiffLo = sum(IndepCiHi <= IndepCiLo[ii])
  SigDiffHi = sum(IndepCiLo >= IndepCiHi[ii])

  NotSigDiff = (SigDiffLo+1):(n-SigDiffHi)
  mycex = 0.5
  ## Add text
  text(ii, (1:n)[NotSigDiff], Abbreviation[ii], cex = mycex, family = "mono", font = 2)
  ## Draw box
  wd = .5
  ht = .5
  polygon(c(ii-wd, ii+wd, ii+wd, ii-wd),
          c(Rank[ii]-ht, Rank[ii]-ht, Rank[ii]+ht, Rank[ii]+ht),
          border = NA, col = "grey20")
  text(ii, Rank[ii], Abbreviation[ii], cex = mycex, family = "mono", font = 2, col = "white")
}
axis(1, at = 1:n, labels = FALSE)
text(1:n + 0.5, par("usr")[3] - 2.0, labels = State, srt = 45, pos = 2, xpd = TRUE, cex = 0.7)
axis(2, at = 1:n, las = 2, cex.axis = 0.7)


## ---- eval=FALSE--------------------------------------------------------------------------------------------
## stopifnot(15 <= min(IndepCiLo) & max(IndepCiHi) <= 35)
## thetamin = 15.5
## thetamax = 33
## mycex = 0.5
## tickWidth = 2/n
## 
## par(xpd = TRUE, mar = c(6.3, 2.8, 0.3, 0.3) + 0.1)
## plot(c(0, n+1), c(thetamin, thetamax),
##      type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='',
##      xaxs = 'i', yaxs = 'i')
## wd = 0.5; ht = 0.5
## for(ii in seq(1, n-2, by = 6)){
##   polygon(c(ii-ht, ii-ht, ii+ht+2, ii+ht+2),
##           c(thetamin, thetamax, thetamax, thetamin),
##           border = NA, col = "grey90")
## }
## text(-3.5, thetamax - 0.3, "$\\theta_k$", cex = 1, pos = 4)
## 
## for(ii in 1:n){
##   points(ii, Estimate.1dec[ii], pch=16, cex=mycex)
##   arrows(y0 = Estimate.1dec[ii], x0 = ii, y1 = IndepCiLo[ii],
##          angle = 90, length = tickWidth)
##   arrows(y0 = Estimate.1dec[ii], x0 = ii, y1 = IndepCiHi[ii],
##          angle = 90, length = tickWidth)
## }
## 
## axis(1, at = 1:n, labels = FALSE)
## text(1:n + 0.5, par("usr")[3] - 0.8, labels = State, srt = 45, pos = 2, xpd = TRUE, cex = 0.7)
## axis(2, at = seq(16, 32, by = 2), las = 2, cex.axis = 0.7)


## ----joint-fig2, fig.width=8.5, fig.height=5.5, echo=FALSE--------------------------------------------------
stopifnot(15 <= min(IndepCiLo) & max(IndepCiHi) <= 35)
thetamin = 15.5
thetamax = 33
mycex = 0.5
tickWidth = 2/n

par(xpd = TRUE, mar = c(6.3, 2.8, 0.3, 0.3) + 0.1)
plot(c(0, n+1), c(thetamin, thetamax),
     type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='',
     xaxs = 'i', yaxs = 'i')
wd = 0.5; ht = 0.5
for(ii in seq(1, n-2, by = 6)){
  polygon(c(ii-ht, ii-ht, ii+ht+2, ii+ht+2),
          c(thetamin, thetamax, thetamax, thetamin),
          border = NA, col = "grey90")
}
text(-3.5, thetamax - 0.3, "$\\theta_k$", cex = 1, pos = 4)

for(ii in 1:n){
  points(ii, Estimate.1dec[ii], pch=16, cex=mycex)
  arrows(y0 = Estimate.1dec[ii], x0 = ii, y1 = IndepCiLo[ii],
         angle = 90, length = tickWidth)
  arrows(y0 = Estimate.1dec[ii], x0 = ii, y1 = IndepCiHi[ii],
         angle = 90, length = tickWidth)
}

axis(1, at = 1:n, labels = FALSE)
text(1:n + 0.5, par("usr")[3] - 0.8, labels = State, srt = 45, pos = 2, xpd = TRUE, cex = 0.7)
axis(2, at = seq(16, 32, by = 2), las = 2, cex.axis = 0.7)

