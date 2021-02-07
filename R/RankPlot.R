# TODO: change the single Bonferroni arg into two different args:
# multcomp.scope = "none", "demi", "full"
# multcomp.type  = "none", "bonferroni", "independence"
# ???
# ...
# For now, let's keep original Bonferroni arg,
# and *add* a new arg multcomp.type = "independence", "bonferroni"
# -- this will be confusing
#    since the Bonf arg actually specifies none/demi/full,
#    not whether it's Bonf or Indep or another correction...
# but it's the fastest way to add an Independence option
# given my very limited time this week.
# We'll refactor this later!



# TODO: sanitize args when plotType = "columns", warning users that
# it won't have any effect if they tried setting non-default args
# for anything that doesn't get passed to RankColumnPlot()

# TODO: note which args do/don't apply to plotType = "columns"?

# TODO: add a function argument for whether the comparisons plot
# should show *uncorrected* reference-state CI
# or the current default demi-Bonferroni correction.
# (The actual *comparison intervals* should still keep demi-Bonf,
#  whether or not the reference state has Bonf-correction.)

#############################################################
#
#   The function RankPlot creates a graph as
#   described by Almond, Lewis, Tukey, and Yan (2000)
#   for visualizing significant differences between
#   one reference parameter and a set of other
#   parameters, and automatically adds appropriate axis
#   labels.
#
#   Arguments:
#
#   est:     vector of point estimates
#   se:      vector of standard errors of estimates
#   names:   vector of characters giving the
#            names the estimates represent
#   refName: one of the values of "names" giving the
#            estimate which is being compared to
#            (optional if plotType=="individual",
#             then defaults to median)
#   confLevel:  a number between 0 and 1 giving the
#            confidence level for the set of all
#            pairwise comparisons with the reference
#   tickWidth:  specifies the length of the ends of the
#            error bars. By default set to 2/n, where
#            n is the length of est
#
#############################################################



#' Figure containing a plot of ranking data.
#'
#' \code{RankPlot} creates a figure with a plot of ranking data,
#'   from among several options for showing uncertainty in the ranked estimates.
#'   This function is meant for use within \code{\link{RankPlotWithTable}},
#'   which draws a ranking table aligned with this plot of the data
#'   in one combined figure.
#'
#' Users may wish to modify this code and write
#'   their own plot function, which can be swapped into \code{figureFunction}
#'   within \code{\link{RankPlotWithTable}}. Be aware that
#'   \code{RankPlotWithTable} uses \code{\link{layout}} to arrange
#'   the table and plot side-by-side, so \code{layout} cannot be used within
#'   a new \code{figureFunction}.
#'
#' See Goldstein and Healy (1995) for details on the
#'   "average" confidence level procedure used when \code{GH = TRUE}.
#'   See Almond et al. (2000) for details
#'   on the "comparison intervals" procedure.
#'
#' @param est,se Vectors containing the point estimate and its standard error
#'   for each area.
#' @param names Vector containing the name of each area.
#'   Abbreviations may be preferable to full names
#'   (e.g. "CO" instead of "Colorado")
#'   since these names will be displayed directly on the plot.
#' @param refName String containing the name of the reference area;
#'   must be one of the values in \code{names}.
#'   Required for \code{plotType = c("difference", "comparison")}.
#'   Optional for \code{plotType = "individual"} (where it only determines
#'   the row above/below which the \code{names} are plotted to the right/left
#'   of the intervals; if unspecified, defaults to median rank);
#'   or for \code{plotType = "columns"} (where it selects one column
#'   to be highlighted by vertical lines, if specified).
#' @param confLevel Number between 0 and 1: confidence level for individual
#'   (uncorrected) hypothesis tests and/or confidence intervals. E.g. with
#'   \code{plotType = "individual"}, \code{confLevel = 0.9} will plot
#'   individual 90\% confidence intervals. If using \code{GH = TRUE}
#'   and/or \code{Bonferroni != "none"}, the Goldstein-Healy and/or Bonferroni/Independence
#'   corrections will be applied to the \code{confLevel} baseline.
#' @param plotType Which type of ranking plot to use. See vignettes for
#'   examples and details.
#'   \itemize{
#'   \item \code{"individual"} is used for usual individual
#'   confidence intervals, with or without Goldstein-Healy adjustment and/or
#'   (demi or full) Bonferroni/Independence corrections.
#'   \item \code{"difference"} shows confidence intervals for the differences
#'   between the reference area \code{refName} and all other areas.
#'   \item \code{"comparison"} also compares the reference area \code{refName}
#'   to all others, but using the "comparison intervals"
#'   of Almond et al. (2000).
#'   \item \code{"columns"} plots a grid of shaded columns, where each column
#'   uses shading to report demi-Bonferroni/Independence-corrected significance tests
#'   for comparing the reference area (labeled at the bottom of the column)
#'   with all other areas.
#'   }
#' @param tiers Numeric, either 1 for usual confidence intervals,
#'   or 2 for two-tiered intervals. 2 can only be used with
#'   \code{plotType = "individual"}, when either \code{GH = TRUE}
#'   or \code{Bonferroni != "none"} or both.
#'   In that case, the "inner tiers" run between each interval's cross-bars,
#'   and the "outer tiers" run past the cross-bars
#'   all the way to the ends of each interval.
#'   One of the tiers will show uncorrected
#'   \code{confLevel*100}\% confidence intervals,
#'   and the other tier will show the Goldstein-Healy and/or Bonferroni/Independence
#'   adjusted intervals. A legend will show which tier is which;
#'   usually Goldstein-Healy alone gives shorter intervals (inner tier),
#'   but Bonferroni/Independence corrections make them into longer intervals (outer tier).
#' @param GH Logical, for whether or not to plot adjusted
#'   confidence intervals at an "average" \code{confLevel*100}\%
#'   confidence level as in Goldstein and Healy (1995).
#'   Can only be used with \code{plotType = "individual"}.
#' @param Bonferroni Whether and how to correct for multiple comparisons by a
#'   Bonferroni or Independence correction to the confidence level of the tests or intervals.
#'   \code{"none"} performs no correction; \code{"demi"} corrects for
#'   comparing one reference area to all \code{n-1} other areas; and
#'   \code{"full"} corrects for comparing all possible \code{choose(n-1, 2)}
#'   pairs of areas.
#'   If \code{GH = TRUE}, the Goldstein-Healy adjustment
#'   is performed first, and any Bonferroni/Independence correction is applied afterwards.
#'   Settings \code{"none"} and \code{"full"} can only be used
#'   with \code{plotType = "individual"};
#'   all other plot types use the setting \code{"demi"}.
#'   (For now, use the \code{multcomp.type} argument to specify whether the correction
#'   should rely on Bonferroni (default) or on an assumption of Independence.
#'   In the future, this package will be refactored
#'   so that the multiple-comparisons arguments are better named!)
#' @param tikzText Logical, for whether or not to format text for tikz plotting.
#' @param cex \strong{C}haracter \strong{ex}pansion factor for
#'   the points use to plot each area's point estimate, and for the
#'   text used to plot each area's name next to its interval.
#' @param tickWidth Numeric height of the cross-bars on interval endpoints
#'   (or inner tiers, if \code{tiers = 2}). The function tries to leave
#'   a reasonable amount of space between intervals plotted in different rows,
#'   but sometimes it may help to adjust \code{tickWidth} manually.
#' @param rangeFactor Numeric multiple by which to expand the range of the data
#'   when setting the x-axis limits. The function tries to leave sufficient room
#'   for plotting margins of error and names next to each area,
#'   but sometimes it may help to adjust \code{rangeFactor} manually.
#' @param textPad Numeric amount by which to shift the text of \code{names}
#'   past the interval endpoints when plotting. Positive values shift outwards
#'   (towards the edges of the plot); negative values shift inwards.
#' @param legendX,legendY The x and y co-ordinates used to position the legend;
#'   see \code{\link{legend}} for details on specifying \code{x} by keyword.
#' @param legendText String, or string vector, with legend text. By default,
#'   each plot type adds informative legend text, but the user may override.
#'   To remove legends entirely, set \code{legendText=NA}.
#' @param lwdReg Positive number for the line width of regular lines.
#'   Used for all intervals when \code{plotType = "individual"},
#'   or for intervals not significantly different from the reference area
#'   when \code{plotType = c("difference", "comparison")}.
#' @param lwdBold Positive number for the line width of bold lines.
#'   Used for intervals significantly different from the reference area
#'   when \code{plotType = c("difference", "comparison")}.
#' @param thetaLine Number for how many lines below bottom axis to display
#'   "theta" or other default x-axis labels (which depend on \code{plotType}).
#' @param xlim Vector of 2 numbers for x-axis limits. If \code{NULL},
#'   will be automatically set using range of data
#'   expanded by \code{rangeFactor}.
#' @param multcomp.type Whether multiple comparison corrects should use a
#'   Bonferroni correction (\code{"bonferroni"})
#'   or an independence-based correction (\code{"independence"}).
#'   See Section 4 of the paper "A Joint Confidence Region..." (2020, JRSS-C)
#'   for the difference in these two corrections.
#'   (In the future, this package will be refactored
#'   so that the multiple-comparisons arguments are better named!)
#' @references Almond, R.G., Lewis, C., Tukey, J.W., and Yan, D. (2000).
#'   "Displays for Comparing a Given State to Many Others,"
#'   \emph{The American Statistician}, vol. 54, no. 2, 89-93.
#'
#'   Goldstein, H. and Healy, M.J.R. (1995). "The Graphical Presentation of a
#'   Collection of Means," \emph{JRSS A}, vol. 158, no. 1, 175-177.
#' @examples
#' # Plot of 90% confidence intervals for differences
#' # between each state and Colorado, with demi-Bonferroni correction,
#' # for US states' mean travel times to work, from the 2011 ACS
#' data(TravelTime2011)
#' with(TravelTime2011,
#'      RankPlot(est = Estimate.2dec, se = SE.2dec,
#'               names = Abbreviation, refName = "CO",
#'               confLevel = 0.90, cex = 0.6,
#'               plotType = "difference"))
#' @seealso \code{\link{RankPlotWithTable}} and \code{\link{RankTable}}.
#' @export
RankPlot = function(est, se, names, refName=NULL,
                    confLevel = 0.9,
                    plotType = c("individual", "difference", "comparison", "columns"),
                    tiers = 1, GH = FALSE,
                    Bonferroni = ifelse(plotType == "individual", "none", "demi"),
                    tikzText = FALSE,
                    cex=1,
                    tickWidth=NULL,rangeFactor=1.2,
                    textPad = 0,
                    legendX = "topleft", legendY = NULL, legendText = NULL,
                    lwdReg = 1, lwdBold = 3, thetaLine = 1,
                    xlim=NULL,
                    multcomp.type = c("bonferroni", "independence")) {
  n = length(est)
  stopifnot(length(se) == n & length(names) == n)
  stopifnot(is.numeric(est) & is.numeric(se))

  stopifnot(0 < confLevel & confLevel < 1)
  plotType = match.arg(plotType)
  stopifnot(tiers %in% 1:2)
  stopifnot(GH %in% c(TRUE, FALSE))
  Bonferroni = match.arg(Bonferroni, c("none", "demi", "full"))
  multcomp.type = match.arg(multcomp.type, c("bonferroni", "independence"))
  if(plotType != "individual") {
    if(is.null(refName) & plotType != "columns") {
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
  if(tiers == 2) { # need at least one of GH or Bonf/Indep correction
    stopifnot(GH | Bonferroni != "none")
  }

  if(plotType == "columns") {
    RankColumnPlot(est = est, se = se, names = names,
                   refName = refName, confLevel = confLevel,
                   multcomp.type = multcomp.type)
    # Don't continue with rest of plotting function below
    return()
  }

  if(tikzText) {
    textR = "$\\hat{r}_k$"
    textTheta = ifelse(plotType == "difference",
                       "$\\theta_k-\\theta_{k^*}$",
                       "$\\theta_k$")
    textThetaStar = "$\\hat{\\theta}_{k^*}$"
    textPercent = "\\%"
  } else {
    textR = expression(hat(r)[k])
    textTheta = ifelse(plotType == "difference",
                       expression(theta[k]-theta[paste(k,"*")]),
                       expression(theta[k]))
    textThetaStar = expression(hat(theta)[paste(k,"*")])
    textPercent = "%"
  }


  # determine units to use on axes
  range = max(est)-min(est)
  extrange = rangeFactor*range
  unit = range2units(extrange)

  # sort estimates from least to greatest;
  # match reference with index of sorted estimates
  estSort = sort(est)
  seSort = se[order(est)]
  namesSort = names[order(est)]
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
    # TODO: edit this into a function argument,
    # in case users want *uncorrected* CI for reference state
    # instead of default demi-mult-corrected CI
    # (also see below)
    #
    # compute special "SEs" for comparison intervals
    sePlot = sqrt(seSort[refInd]^2 + seSort^2) - seSort[refInd]
    sePlot[refInd] = seSort[refInd]
  }

  nrCorr = switch(Bonferroni,
                  none = 1, demi = n-1, full = choose(n, 2))
  confLevelC = ifelse(GH, ConfidenceLevelGH(se, confLevel),
                      confLevel)
  if(tiers == 2) {
    # Individual CIs: no Bonferroni/Indep or Goldstein-Healy correction
    pI = 1 - ((1 - confLevel)/2)
    qI = qnorm(pI)
    # Corrected CIs
    if(multcomp.type == "bonferroni") {
      pC = 1 - ((1 - confLevelC)/(2*nrCorr))
    } else if(multcomp.type == "independence") {
      pC = 1 - (1 - (confLevelC)^(1/nrCorr))/2
    }
    qC = qnorm(pC)

    legendDetails = c(paste0("tier: individual ", 100*confLevel,
                             textPercent, " CIs\n "),
                      paste0("tier: ",
                             ifelse(GH, "Goldstein-Healy", ""),
                             ifelse(GH & Bonferroni != "none",
                                    "\nand ", ""),
                             ifelse(Bonferroni != "none",
                                    paste0(Bonferroni, ifelse(multcomp.type == "bonferroni",
                                                              "-Bonferroni", "-Independence")), ""),
                             "\nadjusted intervals"))

    # Compare and assign bigger factor to outer tier
    if(qC > qI) {
      q = qI
      qOut = qC
      if(is.null(legendText)) {
        legendText = paste0(c("Inner ", "Outer "), legendDetails)
      }
    } else {
      q = qC
      qOut = qI
      if(is.null(legendText)) {
        legendText = paste0(c("Inner ", "Outer "), rev(legendDetails),
                            c("\n ", ""))
      }
    }
    moeOut = qOut*sePlot
  } else {
    if(multcomp.type == "bonferroni") {
      p = 1 - ((1 - confLevelC)/(2*nrCorr))
    } else if(multcomp.type == "independence") {
      p = 1 - (1 - (confLevelC)^(1/nrCorr))/2
    }
    q = qnorm(p)
  }

  #compute error margins
  moe = q*sePlot

  # TODO: edit this into a function argument,
  # in case users want *uncorrected* CI for reference state
  # instead of default demi-Bonf.-corrected CI
  # (also see above)
  #
  # #compute error margins
  # if(plotType != "comparison") {
  #   moe = q*sePlot
  # } else {
  #   # compute special "SEs and MOEs" for comparison intervals
  #   pRef = 1 - ((1 - confLevelC)/2)
  #   pOther = 1 - ((1 - confLevelC)/(2*nrCorr))
  #   qRef = qnorm(pRef)
  #   qOther = qnorm(pOther)
  #   moe = qOther*sqrt(seSort[refInd]^2 + seSort^2) - qRef*seSort[refInd]
  #   moe[refInd] = qRef*seSort[refInd]
  # }

  if (is.null(xlim)) {
    xlower = (floor(min(estSort)/unit)-1)*unit
    xupper = (ceiling(max(estSort)/unit)+1)*unit
    xlim = c(xlower, xupper)
  }

  #set width of error bars
  tickWidth = ifelse(is.null(tickWidth), 2/n, tickWidth)

  # create empty plot
  plot(seq(xlim[1], xlim[2], length = n+2), 0:(n+1), type = "n",
       xlab="", ylab="", xlim = c(xlim[1],xlim[2]), yaxt="n", xaxt="n",
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
    ps = ifelse(estSort[i] <= estSort[refInd], -1, 1)
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

  axis(2, at = 1:n, labels=rep("", n), las=2, cex.axis=0.7)

  # add theta_k below reference area
  adj = ifelse(plotType == "comparison", NA, 1)
  mtext(textTheta, side=1, at=xlim[2]+.3*unit, line=thetaLine, adj=adj)
  if(plotType == "comparison") {
    # add theta_star symbol below reference area
    mtext(textThetaStar, side=1, at=estSort[refInd], line=thetaLine)
  }

  # add legend
  if(plotType == "individual" & is.null(legendText)) {
    if(!GH & Bonferroni == "none") {
      legendText = paste0(100*confLevel, textPercent, " confidence intervals")
    } else {
      legendText = paste0(ifelse(GH, "Goldstein-Healy", ""),
                          ifelse(GH & Bonferroni != "none", "\nand ", ""),
                          ifelse(Bonferroni != "none",
                                 paste0(Bonferroni, ifelse(multcomp.type == "bonferroni",
                                                           "-Bonferroni", "-Independence")), ""),
                          "\nadjusted intervals")
    }
  }
  if(plotType == "individual" & !is.null(legendText)) {
    legend(x = legendX, y = legendY,
           legend = legendText, bty = "n",
           inset = 0.02, cex = 0.8)
  } else if(plotType != "individual") {
    if(is.null(legendText)) {
      legendText = c("Significantly Different",
                     "Not Significantly Different")
    }
    if(!all(is.na(legendText))) {
      legend(x = legendX, y = legendY,
             legend = legendText,
             lwd = c(lwdBold, lwdReg), inset = 0.02, cex = 0.8, bg = "white",
             title = paste0("Compared to ", refName))
    }
  }
}
