#############################################################
#
#   The function rankCompPlot creates a graph as
#   described by Almond, Lewis, Tukey, and Yan (2000)
#   for visualizing significant differences between
#   one reference parameter and a set of other
#   parameters, and automatically adds appropraite axis
#   labels.
#
#   Arguments:
#
#   est:        vector of point estimates
#   se:     vector of standard errors of estimates
#   names:  vector of characters giving the
#           names the estimates represent
#   refName: one of the values of "names" giving the
#           estimate which is being compared to
#           (optional if plotType=="individual", then defaults to median)
#   confLevel:  a number between 0 and 1 giving the
#           confidence level for the set of all
#           pairwise comparisons with the reference
#   tickWidth:  specifies the length of the ends of the
#           error bars. By default set to 2/n, where
#           n is the length of est
#   regions:    optional factor vector of regions. If used,
#           the values of est will be plotted in
#           groups by region
#
#############################################################

REG.CIs.rankCompPlot = function(est, se, names, refName=NULL,
    confLevel = 0.9, xlim=NULL,
    xlab="", ylab="", xaxt = "n", yaxt = "n", cex=1, tickWidth=NULL, regions=NULL,
    rangeFactor=1.2,
    textPad = 0,
    plotType = c("individual", "difference", "comparison"),
    tiers = 1, GH = FALSE,
    Bonferroni = ifelse(plotType == "individual", "none", "demi"),
    legendX = "topleft", legendY = NULL, legendText = NULL,
    lwdReg = 1, lwdBold = 3, showYlab = FALSE, thetaLine = 1) {

    n = length(est)
    stopifnot(length(se) == n & length(names) == n)
    stopifnot(is.numeric(est) & is.numeric(se))

    stopifnot(0 < confLevel & confLevel < 1)
    plotType = match.arg(plotType)
    stopifnot(tiers %in% 1:2)
    stopifnot(GH %in% c(TRUE, FALSE))
    Bonferroni = match.arg(Bonferroni, c("none", "demi", "full"))
    if(plotType != "individual") {
        if(is.null(refName)) {
            stop("Must provide refName")
        }
        if(Bonferroni != "demi") {
            stop("Must use Bonferroni='demi' unless plotType='individual'")
        }
        if(GH) {
            stop("Must use GH=FALSE unless plotType='individual'")
        }
        if(tiers != 1) {
            stop("Must use tiers=1 unless plotType='individual'")
        }
    }
    if(tiers == 2) { # need at least one of GH or Bonf correction
        stopifnot(GH | Bonferroni != "none")
    }


    # determine units to use on axes
    range = max(est)-min(est)
    extrange = rangeFactor*range
    unit = range2units(extrange)

    # sort estimates from least to greatest (and by region, if specified);
    # match reference with index of sorted estimates
    estSort = sort(est)
    seSort = se[order(est)]
    namesSort = names[order(est)]
    if (!is.null(regions)) {
        regSort = regions[order(est)]
        estSort = estSort[order(regSort)]
        seSort = seSort[order(regSort)]
        namesSort = namesSort[order(regSort)]
    }
    if(is.null(refName)) {
        # use median as "reference" state
        refInd = floor(n/2)
        refName = namesSort[refInd]
    } else {
        refInd = which(namesSort == refName)
    }

    if(plotType == "individual") {
        sePlot = seSort
    } else if(plotType == "difference") {
        # center at the reference state
        estSort = estSort - estSort[refInd]
        # compute SEs of differences
        sePlot = sqrt(seSort[refInd]^2 + seSort^2)
        sePlot[refInd] = 0
    } else if(plotType == "comparison") {
        # compute special "SEs" for comparison intervals
        sePlot = sqrt(seSort[refInd]^2 + seSort^2) - seSort[refInd]
        sePlot[refInd] = seSort[refInd]
    }

    denomC = 2 * switch(Bonferroni,
                        none = 1, demi = n-1, full= choose(n, 2))
    confLevelC = ifelse(GH, ConfidenceLevelGH(se, confLevel),
                        confLevel)
    if(tiers == 2) {
        # Individual CIs: no Bonferroni or Goldstein-Healy correction
        pI = 1 - ((1 - confLevel)/2)
        qI = qnorm(pI)
        # Corrected CIs
        pC = 1 - ((1 - confLevelC)/denomC)
        qC = qnorm(pC)

        legendDetails = c(paste0("tier: individual ", 100*confLevel,
                                 "\\% CIs\n"),
                          paste0("tier: ",
                                 ifelse(GH, "Goldstein-Healy", ""),
                                 ifelse(GH & Bonferroni != "none",
                                        "\nand ", ""),
                                 ifelse(Bonferroni != "none",
                                        paste0(Bonferroni, "-Bonferroni"), ""),
                                 "\nadjusted intervals"))
        # Compare and assign bigger factor to outer tier
        if(qC > qI) {
            q = qI
            qOut = qC
            legendText = paste(c("Inner", "Outer"), legendDetails)
        } else {
            q = qC
            qOut = qI
            legendText = paste(c("Inner", "Outer"), rev(legendDetails),
                               c("\n", ""))
        }
        moeOut = qOut*sePlot
    } else {
        p = 1 - ((1 - confLevelC)/denomC)
        q = qnorm(p)
    }

    #compute error margins
    moe = q*sePlot

    if (is.null(xlim)) {
        xlower = (floor(min(estSort)/unit)-1)*unit
        xupper = (ceiling(max(estSort)/unit)+1)*unit
        xlim = c(xlower, xupper)
    }

    #set width of error bars
    tickWidth = ifelse(is.null(tickWidth), 2/n, tickWidth)

    # create empty plot
    plot(seq(xlim[1], xlim[2], length = n+2), 0:(n+1), type = "n",
        xlab=xlab, ylab=ylab, xlim = c(xlim[1],xlim[2]), yaxt=yaxt, xaxt=xaxt,
        # make y-axis go from 0 to n+1
        yaxs='i')

    # add reference line/strip at 0 for difference/comparison plots
    if(plotType == "difference") {
        abline(v = estSort[refInd])
    } else if(plotType == "comparison") {
        rect(estSort[refInd] - moe[refInd], 0,
             estSort[refInd] + moe[refInd], n+1, border=NA, col='grey')
        abline(v = estSort[refInd] - moe[refInd], lty=2)
        abline(v = estSort[refInd] + moe[refInd], lty=2)
        abline(v = estSort[refInd])
        mtext(namesSort[refInd], side=3, at=estSort[refInd], line=0.5, cex=0.7)
        mtext("Distance from the strip", side=3, line=2.2)
    }

    # plot reference state
    points(estSort[refInd], refInd, cex=cex, pch=16)
    text(estSort[refInd] - (moe[refInd] + textPad), refInd,
        labels = namesSort[refInd], pos = 2, cex=cex)
    text(estSort[refInd] + (moe[refInd] + textPad), refInd,
        labels = namesSort[refInd], pos = 4, cex=cex)

    #plot values with error bars
    if(plotType == "individual") {
        pts = 1:n
    } else {
        pts = (1:n)[-refInd]
    }
    for (i in pts) {
        ps = ifelse(estSort[i] < estSort[refInd], -1, 1)
        lwd = ifelse(plotType != "individual" &
                ((ps == -1 & estSort[i]+moe[i] < estSort[refInd]-moe[refInd]) |
                 (ps == 1 & estSort[i]-moe[i] > estSort[refInd]+moe[refInd])),
                     lwdBold, lwdReg)
        points(estSort[i], i, pch=16, cex=cex)
        arrows(x0 = estSort[i], y0 = i, x1 = estSort[i] - moe[i],
               angle = 90, length = tickWidth, lwd=lwd)
        arrows(x0 = estSort[i], y0 = i, x1 = estSort[i] + moe[i],
               angle = 90, length = tickWidth, lwd=lwd)
        if(i != refInd) { # refInd was already labeled earlier
            text(estSort[i] + ps*(moe[i] + textPad), i, labels = namesSort[i],
                 pos = 3+ps, cex=cex)
        }
        if(tiers == 2) {
            # also draw outer CIs, with no "arrows"
            segments(x0 = estSort[i] + moeOut[i], y0 = i,
                     x1 = estSort[i] - moeOut[i], 
                     lwd = lwdReg)
        }
    }

    # add axes

    # bottom axis
    xax = seq(xlim[1],xlim[2],by=unit)
    axis(1, at = xax, cex.axis=0.7, mgp=c(3,0.3,0))

    # top axis
    if(plotType == "comparison") {
        toplab1 = rev((0:(floor(2*(estSort[refInd] - moe[refInd] - xlim[1])/unit)))*unit/2)
        toplab2 = (0:(floor(2*(xlim[2] - estSort[refInd] - moe[refInd])/unit)))*unit/2
        toplab = c(toplab1, toplab2)
        topax = c(estSort[refInd] - moe[refInd] - toplab1, estSort[refInd] + moe[refInd] + toplab2)
        axis(3, at = topax, labels = toplab, cex.axis=0.7)
    }

    # left axis
    if(showYlab) {
      ylabels = 1:n
      # Use mtext, not title, to make the y-label listen to las=2
      mtext("$\\hat{r}_k$", side=2, line=2.5, las=2)
    } else {
      ylabels = rep("", n)
    }
    axis(2, at = 1:n, labels=ylabels, las=2, cex.axis=0.7)

    # add "\theta_k" below reference area
    thetaText = ifelse(plotType == "difference",
                       "$\\theta_k-\\theta_{k^*}$",
                       "$\\theta_k$")
    adj = ifelse(plotType == "comparison", NA, 1)
    mtext(thetaText, side=1, at=xlim[2]+.3*unit, line=thetaLine, adj=adj)
    if(plotType == "comparison") {
        # add theta_star symbol below reference area
        mtext("$\\hat{\\theta}_{k^*}$", side=1, at=estSort[refInd], line=thetaLine)
    }

    # add region names (if necessary)
    if (!is.null(regions)) {
        y = cumsum(as.numeric(summary(regions)))
        regText = levels(regions)
        nregs = length(regText)
        for (i in 1:nregs) {
            text(xlim[2], y[i], labels = regText[i], pos = 2, cex=cex)
        }
    }

    # add legend
    if(plotType == "individual" & is.null(legendText)) {
        if(!GH & Bonferroni == "none") {
            legendText = paste0(100*confLevel, "\\% confidence intervals")
        } else {
            legendText = paste0(ifelse(GH, "Goldstein-Healy", ""),
                ifelse(GH & Bonferroni != "none", "\nand ", ""),
                ifelse(Bonferroni != "none",
                       paste0(Bonferroni, "-Bonferroni"), ""),
                "\nadjusted ", 100*confLevel, "\\% intervals")
        }
    }
    if(plotType == "individual" & !is.null(legendText)) {
        legend(x = legendX, y = legendY,
               legend = legendText, bty = "n",
               inset = 0.02, cex = 0.8)
    } else if(plotType != "individual") {
        legend(x = legendX, y = legendY,
               legend = c("Significantly Different",
                          "Not Significantly Different"),
               lwd = c(lwdBold, lwdReg), inset = 0.02, cex = 0.8, bg = "white",
               title = paste0("Compared to ", refName))
    }
}