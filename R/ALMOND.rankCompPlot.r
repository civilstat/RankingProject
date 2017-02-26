#############################################################
#
#	The function rankCompPlot creates a graph as
#	described by Almond, Lewis, Tukey, and Yan (2000)
#	for visualizing significant differences between
#	one reference parameter and a set of other
#	parameters, and automatically adds appropraite axis
#	labels.
#
#	Arguments:
#
#	est: 		vector of point estimates
#	se:		vector of standard errors of estimates
#	names: 	optional vector of characters giving the
#			names the estimates represent
#	ref: 		either an index number or one of the
#			values of "names" giving the
#			estimate which is being compared to
#	conf.level:	a number between 0 and 1 giving the
#			confidence level for the set of all
#			pairwise comparisons with the reference
#	length:	specifies the length of the ends of the
#			error bars. By default set to 2/n, where
#			n is the length of est
#	regions:	optional vector of regions. If used,
#			the values of est will be plotted in
#			groups by region
#
#############################################################

ALMOND.rankCompPlot = function(est, se, names=NULL, ref=1, refName="", conf.level = 0.9, xlim=NULL,
	xlab="", ylab="", yaxt = "n", xaxt = "n", cex=1, length=NULL, regions=NULL,
	legendPos = "topleft", showYlab = FALSE, thetaLine = 1.5, lwdBold = 3, ...) {

	n = length(est)
	range = max(est)-min(est)
	extrange = 1.2*range

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

	p = 1 - ((1 - conf.level)/2)/(n-1)
	q = qnorm(p)

	#compute error margins

	moe = q*(sqrt(seSort[refInd]^2+seSort^2)-seSort[refInd])
	moe[refInd] = q*seSort[refInd]

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
# make y-axis go from 0 to 52
		yaxs='i')
	mtext("Distance from the strip", side=3, line=2.2)

	#add reference

# add the strip/pale
	rect(estSort[refInd]-moe[refInd], 0, estSort[refInd]+moe[refInd], 52, border=NA, col='grey')
	abline(v = estSort[refInd] - moe[refInd], lty=2)
	abline(v = estSort[refInd] + moe[refInd], lty=2)
	abline(v = estSort[refInd])

	mtext(namesSort[refInd], side=3, at=estSort[refInd], line=0.5, cex=0.7)
	points(estSort[refInd], refInd, cex=cex, pch=16)
	text(estSort[refInd] - moe[refInd], refInd,
		labels = namesSort[refInd], pos = 2, cex=cex)
	text(estSort[refInd] + moe[refInd], refInd,
		labels = namesSort[refInd], pos = 4, cex=cex)

if(FALSE){
	abline(v = estSort[refInd] - moe[refInd])
	abline(v = estSort[refInd] + moe[refInd])
	len = round(400*moe[refInd]/range)
	xx = seq(estSort[refInd] - moe[refInd], estSort[refInd] + moe[refInd],
			length = len)
	for (k in 2:(len-1)) {
		abline(v = xx[k], lty = 3, col="grey")
	}
	abline(v = estSort[refInd])
} #end of if(FALSE)

	#plot all other values with error bars

#	# (set Line Weights for sig-diff vs not-sig-diff from the reference area)
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
		# arrows() breaks if the MOE is too small,
		# but we still want to print arrows even for tiny MOEs,
		# so set the MOE just barely large enough to print (if necessary)
		arrows(x0 = estSort[i], y0 = i, x1 = estSort[i] - max(moe[i], range(xlim)/1000),
			angle = 90, length = arlen, lwd=lwd)
		arrows(x0 = estSort[i], y0 = i, x1 = estSort[i] + max(moe[i], range(xlim)/1000),
			angle = 90, length = arlen, lwd=lwd)
		text(estSort[i] + ps*moe[i], i, labels = namesSort[i], pos = 3+ps,
			cex=cex)
	}

	#add axes

	xax = seq(xlim[1],xlim[2],by=unit)
	toplab1 = rev((0:(floor(2*(estSort[refInd] - moe[refInd] - xlim[1])/unit)))*unit/2)
	toplab2 = (0:(floor(2*(xlim[2] - estSort[refInd] - moe[refInd])/unit)))*unit/2
	toplab = c(toplab1, toplab2)
	topax = c(estSort[refInd] - moe[refInd] - toplab1, estSort[refInd] + moe[refInd] + toplab2)
#	axis(1, at = xax)
	axis(1, at = xax, cex.axis=0.7, mgp=c(3,0.3,0))
	axis(3, at = topax, labels = toplab, cex.axis=0.7)

#	# Add ranks on left axis
	if(showYlab) {
	  ylabels = 1:n
	  # Use mtext, not title, to make the y-label listen to las=2
	  mtext("$\\hat{r}_k$", side=2, line=2.5, las=2)
	} else {
	  ylabels = rep("", n)
	}
	axis(2, at = 1:n, labels=ylabels, las=2, cex.axis=0.7)
#	# add theta_star symbol below reference area
	mtext("$\\hat{\\theta}_{k^*}$", side=1, at=estSort[refInd], line=thetaLine)
#	# add "Y" below reference area
	mtext("$\\theta_k$", side=1, at=xlim[2]+.3*unit, line=thetaLine)


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

	legend(legendPos, inset=0.02,legend=c("Significantly Different","Not Significantly Different"),
		lwd=c(SigDiffLtw,NotDiffLtw), bg="white",
		title=paste("Compared to ", refName),
	cex=.8)
}

