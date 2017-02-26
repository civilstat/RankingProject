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

double.REG.CIs.rankCompPlot = function(est, se, names=NULL, ref=1,
  conf.level = 0.9, conf.level.2 = 0.9, BonfDen = NULL, xlim=NULL,
	xlab="", ylab="", yaxt = "n", xaxt = "n", cex=1, length=NULL, regions=NULL,
	rangefactor=1.2, textPad=0, ...) {

	n = length(est)
	range = max(est)-min(est)
	extrange = rangefactor*range
	
	# If no specified Bonferroni Denominator
	# (nr of tests to correct for),
	# assume we'll be comparing one area against all others
	# so use n-1 tests.
	# (Or, can set BonfDen=1 for uncorrected version.)
	if(is.null(BonfDen)) {
	  BonfDen = n-1
	}
	
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
	
	#NOT doing any Bonferroni-correction for error margins
	
	p = 1 - ((1 - conf.level)/2)
	q = qnorm(p)

	#compute error margins
	moe = q*seSort
	
	#compute error margins for *bonf-corrected* conf.level.2
	p2 = 1 - ((1 - conf.level.2) / (2*BonfDen))
	q2 = qnorm(p2)
	moe2 = q2*seSort
	
	
	
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

	points(estSort[refInd], refInd, cex=cex, pch=16)
	text(estSort[refInd] - (textPad + moe[refInd]), refInd,
		labels = namesSort[refInd], pos = 2, cex=cex)
	text(estSort[refInd] + (textPad + moe[refInd]), refInd,
		labels = namesSort[refInd], pos = 4, cex=cex)

	#plot ALL values with error bars
	pts = (1:n)
	for (i in pts) {
		ps = ifelse(estSort[i] < estSort[refInd], -1, 1)
		lwd = 1
		points(estSort[i], i, pch=16, cex=cex)
		arrows(x0 = estSort[i], y0 = i, x1 = estSort[i] - moe[i], 
			angle = 90, length = arlen, lwd=lwd)
		arrows(x0 = estSort[i], y0 = i, x1 = estSort[i] + moe[i],
			angle = 90, length = arlen, lwd=lwd)
		if(i != refInd) {
		  text(estSort[i] + ps*(moe[i]+textPad), i,
		       labels = namesSort[i], pos = 3+ps,
		       cex=cex)
		}
		
		# also draw outer CIs, with no "arrows"
		segments(x0 = estSort[i] + moe2[i], y0 = i,
		         x1 = estSort[i] - moe2[i], 
		       lwd = lwd)
	}	

	#add axes

	xax = seq(xlim[1],xlim[2],by=unit)
	axis(1, at = xax, cex.axis=0.7, mgp=c(3,0.3,0))

	# left axis
	axis(2, at = 1:n, labels=rep('',n), las=2, cex.axis=0.7)

	# add "\theta_k" below reference area
	mtext("$\\theta_k$", side=1, at=xlim[2]+.3*unit, line=1, adj=1)

	#add region names (if necessary)

	if (!is.null(regions)) {
		y = cumsum(as.numeric(summary(regions)))
		regText = levels(regions)
		nregs = length(regText)
		for (i in 1:nregs) {
			text(xlim[2], y[i], labels = regText[i], pos = 2, cex=cex)
		}
	}
	
}

