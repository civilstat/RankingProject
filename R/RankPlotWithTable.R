# TODO: allow moving table from left to right?
# but that would also require adjusting plots,
# so that the y-axis ticks are on right side,
# and adjusting all the text.


#' Figure containing aligned table and plot of ranking data.
#'
#' \code{RankPlotWithTable} aligns a table of ranking data with a plot of the
#'   data, in one combined figure. See \code{\link{RankTable}} and
#'   \code{\link{RankPlot}} for details about the default table and plot
#'   functions, including arguments that can be passed to those functions.
#'
#' Users may write their own table and plot functions to swap into
#'   \code{tableFunction} and \code{plotFunction}. Be aware that
#'   \code{RankPlotWithTable} uses \code{\link{layout}} to arrange
#'   the table and plot side-by-side, so \code{layout} cannot be used within
#'   either \code{tableFunction} or \code{plotFunction}. This can also cause
#'   trouble for using the \code{lattice} package within \code{plotFunction}.
#'
#' @param tableParList A required named list of arguments that will be passed
#'   to \code{tableFunction} using \code{do.call()}. The default
#'   \code{tableFunction} is \code{\link{RankTable}}, which
#'   requires at least these four arguments:
#'   \code{ranks}, \code{names}, \code{est}, \code{se}.
#' @param plotParList A required named list of arguments that will be passed
#'   to \code{plotFunction} using \code{do.call()}. The default
#'   \code{plotFunction} is \code{\link{RankPlot}}, which
#'   requires at least these three arguments:
#'   \code{est}, \code{se}, \code{names}.
#' @param tableFunction The function to use for plotting a table of the data
#'   on the left-hand side of the layout. Default is \code{\link{RankTable}}.
#' @param plotFunction The function to use for plotting a figure of the data
#'   on the right-hand side of the layout. Default is \code{\link{RankPlot}}.
#' @param tableWidthProp A number between 0 and 1, for what proportion of the
#'   layout's width should be used to plot the table. The remaining proportion
#'   \code{1-tableWidthProp} is used to plot the figure.
#' @param tikzText Logical, formats text for tikz plotting if \code{TRUE}.
#' @param annotRefName,annotRefRank Optional rank and name of the reference
#'   area, for adding an extra
#'   annotation below the figure created by \code{plotFunction}.
#'   Currently centered at 0 on x-axis,
#'   so only useful when \code{plotType = "difference"}.
#'   If provided, the list must contain two required named elements
#'   (\code{refFullName} and \code{refRank}, the reference area's name and rank)
#' @param annotX A number, showing where on the x-axis to center the annotation
#'   if \code{annotRefName} and \code{annotRefRank} are not \code{NULL}.
#' @examples
#' # Table with plot of individual 90% confidence intervals
#' # for US states' mean travel times to work, from the 2011 ACS
#' data(TravelTime2011)
#' tableParList <- with(TravelTime2011,
#'   list(ranks = Rank, names = State,
#'        est = Estimate.2dec, se = SE.2dec,
#'        placeType = "State"))
#' plotParList <- with(TravelTime2011,
#'   list(est = Estimate.2dec, se = SE.2dec,
#'        names = Abbreviation,
#'        confLevel = .90, plotType = "individual", cex = 0.6))
#' RankPlotWithTable(tableParList = tableParList,
#'   plotParList = plotParList)
#'
#' # Illustrating the use of annotRefName and annotRefRank:
#' # Table with plot of 90% confidence intervals for differences
#' # between each state and Colorado, with demi-Bonferroni correction
#' plotParList$plotType <- "difference"
#' plotParList$refName <- "CO"
#' RankPlotWithTable(tableParList = tableParList,
#'   plotParList = plotParList, annotRefName = "Colorado",
#'   annotRefRank = USdata$Rank[which(USdata$Abbreviation == "CO")])
#' @seealso \code{\link{RankPlot}} and \code{\link{RankTable}}.
#' @export
RankPlotWithTable = function(tableParList, plotParList,
                             tableFunction = RankTable,
                             plotFunction = RankPlot,
                             tableWidthProp = 3/8, tikzText = FALSE,
                             annotRefName = NULL, annotRefRank = NULL,
                             annotX = 0) {

  if(tikzText) {
    if(is.null(tableParList$tikzText) | is.null(plotParList$tikzText)){
      warning("Setting tikzText to TRUE in both tableParList and plotParList")
    } else if (!tableParList$tikzText | !plotParList$tikzText) {
      warning("Setting tikzText to TRUE in both tableParList and plotParList")
    }
    tableParList$tikzText <- TRUE
    plotParList$tikzText <- TRUE
  }

  oldpar <- par(no.readonly = TRUE)
  oldmar <- par('mar')

  # Set up the layout: table on left, figure on right
  layout(matrix(c(1, 2), 1, 2),
         widths = c(tableWidthProp, 1 - tableWidthProp))

  # Create a table on the left
  par(xpd=TRUE, mar=c(oldmar[1],0,oldmar[3],0))
  do.call(tableFunction, tableParList)

  # Create a figure on the right
  par(xpd = FALSE, mar = c(oldmar[1], 1.1, oldmar[3], oldmar[4]))
  do.call(plotFunction, plotParList)

  if(!is.null(annotRefName) & !is.null(annotRefRank)) {
    if(tikzText) {
      textK = "($k^*$):~"
      textRank = "Rank:~ "
    } else {
      textK = "(k*):"
      textRank = "Rank: "
    }
    # Create annotations on the bottom of the figure
    mtext(paste0("Reference ", tableParList$placeType, " ", textK, " "),
          side=1, at=annotX, line=2, adj=1)
    mtext(annotRefName, side=1, at=annotX, line=2, adj=0)
    mtext(textRank, side=1, at=annotX, line=3, adj=1)
    mtext(annotRefRank, side=1, at=annotX, line=3, adj=0)
  }

  par(oldpar)
}

