SIMPLE.rankCompPlot = function(est, se, names=NULL, ref=1,
	confLevel = 0.9, xlim=NULL,
    xlab="", ylab="", xaxt = "n", yaxt = "n", cex=1, length=NULL, regions=NULL,
    rangeFactor=1.2,
	legendPos="topleft", lwdBold = 3) {

    refEst = est[which(names == ref)]

	# Re-center the data around the reference area's estimate
	# (only done for SIMPLE.rankCompPlot, not for the other variants)
	est = est - refEst

	n = length(est)
	range = max(est)-min(est)
	extrange = rangeFactor*range

	#determine units to use on axes

	exp5 = floor(log10(extrange/2.5))
	if (floor(log2(extrange/(2.5*10^exp5))) == 0) {
		exp2 = exp5 - 1
	} else if (floor(log2(extrange/(2.5*10^exp5))) == 1) {
		exp2 = exp5
	} else {
		exp2 = exp5 + 1
	}
	unit = 2^exp2*5^exp5

	#sort estimates from least to greatest (and by region, if specified)

	estSort = sort(est)
	seSort = se[order(est)]

	if (!is.null(regions)) {
		if (is.null(names)) stop ("Please provide names");
		regSort = regions[order(est)]
		estSort = estSort[order(regSort)]
		seSort = seSort[order(regSort)]
	}

	#match reference with index of sorted estimates

	if (is.null(names) & is.numeric(ref)) {
		refInd = order(est)[ref]
		namesSort = order(est)
	} else if (is.null(regions)) {
		if (is.null(names)) stop ("Please provide names");
		namesSort = names[order(est)]
		refInd = which(namesSort == ref)
	} else {
		if (is.null(names)) stop ("Please provide names");
		namesSort = names[order(est)]
		namesSort = namesSort[order(regSort)]
		refInd = which(namesSort == ref)
	}

	#Bonferroni-correction for error margins

	p = 1 - ((1 - confLevel)/2)/(n-1)
	q = qnorm(p)

	#compute error margins
# CHANGING THIS for the "simple" version to just be
# direct CIs, not Almond intervals
	moe = q*(sqrt(seSort[refInd]^2+seSort^2))
	moe[refInd] = 0
# set reference MOE to 0, to be compatible with code below
# (which ought to be changed -- right now it checks
#  whether state[i]-moe[i] > state[*]+moe[*] or vice versa,
#  but in the current version it should not be checking moe[*] itself at all,
#  and these shouldn't be called MOEs anyway)

	if (is.null(xlim)) {
		xlower = (floor(min(est)/unit)-1)*unit
		xupper = (ceiling(max(est)/unit)+1)*unit
		xlim = c(xlower, xupper)
	}

	#set width of error bars

	arlen = ifelse(is.null(length), 2/n, length)

	#create empty plot
	plot(seq(xlim[1], xlim[2], length = n+2), 0:(n+1), type = "n",
		xlab=xlab, ylab=ylab, xlim = c(xlim[1],xlim[2]), yaxt=yaxt, xaxt=xaxt,
# make y-axis go from 0 to n+1
		yaxs='i')

# add the strip/pale
# REMOVED for the "simple" version, except single vertical line
	abline(v = estSort[refInd])

	points(estSort[refInd], refInd, cex=cex, pch=16)
	text(estSort[refInd] - moe[refInd], refInd,
		labels = namesSort[refInd], pos = 2, cex=cex)
	text(estSort[refInd] + moe[refInd], refInd,
		labels = namesSort[refInd], pos = 4, cex=cex)


	#plot all other values with error bars
	# (set Line Weights for sig-diff vs not-sig-diff from the reference area)
	SigDiffLtw <- lwdBold
	NotDiffLtw <- 1

	pts = (1:n)[-refInd]
	for (i in pts) {
		ps = ifelse(estSort[i] < estSort[refInd], -1, 1)
		if (estSort[i] < estSort[refInd] & estSort[i]+moe[i] < estSort[refInd]-moe[refInd]) {
			lwd = SigDiffLtw
		} else if (estSort[i] > estSort[refInd] & estSort[i]-moe[i] > estSort[refInd]+moe[refInd]) {
			lwd = SigDiffLtw
		} else {
			lwd = NotDiffLtw
		}
		points(estSort[i], i, pch=16, cex=cex)
		arrows(x0 = estSort[i], y0 = i, x1 = estSort[i] - moe[i],
			angle = 90, length = arlen, lwd=lwd)
		arrows(x0 = estSort[i], y0 = i, x1 = estSort[i] + moe[i],
			angle = 90, length = arlen, lwd=lwd)
		text(estSort[i] + ps*moe[i], i, labels = namesSort[i], pos = 3+ps,
			cex=cex)
	}

	#add axes

	xax = seq(xlim[1],xlim[2],by=unit)
	axis(1, at = xax, cex.axis=0.7, mgp=c(3,0.3,0))

if(FALSE){ # Tommy wants to remove top axis
	toplab1 = rev((0:(floor(2*(estSort[refInd] - moe[refInd] - xlim[1])/unit)))*unit/2)
	toplab2 = (0:(floor(2*(xlim[2] - estSort[refInd] - moe[refInd])/unit)))*unit/2
	toplab = c(toplab1, toplab2)
# SIMPLIFYING for "simple" version so that strip is gone and labels meet at 0
#	topax = c(estSort[refInd] - moe[refInd] - toplab1, estSort[refInd] + moe[refInd] + toplab2)
	topax = c(estSort[refInd] - toplab1, estSort[refInd] + toplab2)
	axis(3, at = topax, labels = toplab, cex.axis=0.7)
} #end of if(FALSE)


#	# Add ranks on left axis
# CHANGING it to remove the actual rank numbers for now
# so we can add it separately using layout()
	axis(2, at = 1:n, labels=rep('',n), las=2, cex.axis=0.7)

	# add "Y" below reference area
	mtext("$\\theta_k-\\theta_{k^*}$", side=1, at=xlim[2]+.3*unit, line=1, adj=1, cex=1)

	#add region names (if necessary)
	if (!is.null(regions)) {
		y = cumsum(as.numeric(summary(regions)))
		regText = levels(regions)
		nregs = length(regText)
		for (i in 1:nregs) {
			text(xlim[2], y[i], labels = regText[i], pos = 2, cex=cex)
		}
	}

	#add legend
	legend(legendPos, inset=0.02, legend=c("Significantly Different","Not Significantly Different"),
		lwd=c(SigDiffLtw,NotDiffLtw), bg="white",
		title=paste("Compared to ", ref),
	cex=.8)
}

