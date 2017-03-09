# TODO: Be consistent about sorting:
# either sort here too, or don't sort in RankPlot?

#' Figure containing a table of ranking data.
#'
#' \code{RankTable} creates a figure with a table of ranking data.
#'   This may not look very good plotted on its own.
#'   Rather, it is meant for use within \code{\link{RankPlotWithTable}},
#'   which draws this table aligned with a plot of the data
#'   in one combined figure.
#'
#' This function is currently hardcoded to give a table with four columns,
#'   with given column names. Users may wish to modify this code and write
#'   their own table function, which can be swapped into \code{tableFunction}
#'   within \code{\link{RankPlotWithTable}}. Be aware that
#'   \code{RankPlotWithTable} uses \code{\link{layout}} to arrange
#'   the table and plot side-by-side, so \code{layout} cannot be used within
#'   a new \code{tableFunction}.
#'
#' @param ranks Vector containing the rank of each area.
#' @param names Vector containing the name of each area.
#' @param est,se Vectors containing the point estimate and its standard error
#'   for each area.
#'   See vignettes for examples of using \code{\link{formatC}}
#'   to turn the numeric estimates or SEs into strings,
#'   for printing with a consistent number of decimal places.
#' @param placeType String, naming the type of places or units being ranked.
#' @param col1,col2,col3,col4 Numeric values between 0 and 1,
#'   showing where each column's right-hand-side endpoint is
#'   along the table's width. In other words, \code{colJ} should be the fraction
#'   of the table's total width at which the Jth column should end,
#'   if using default of right-aligned columns (unless \code{textPos != 2}).
#'   Use \code{col4 = 1} unless you want the table to be narrower
#'   than the space available, or unless you switch to
#'   centered or left-aligned columns.
#' @param textPos Passed to \code{pos} argument of \code{\link{text}}.
#'   Default of 2 ensures each column of text is right-justified.
#' @param titleCex \strong{C}haracter \strong{ex}pansion factor for
#'   column titles.
#' @param titleLift Numeric value for how many row-heights
#'   to raise column titles above top row of column contents.
#' @param contentCex \strong{C}haracter \strong{ex}pansion factor for
#'   column contents (all column text except the titles).
#' @param columnsPlotRefLine Optional numeric value. If not NULL,
#'   how many row-heights below bottom row of column contents to print
#'   the phrase "Reference State:" (or "Reference <placeType>:")
#'   as a label for bottom row of columns plot.
#' @param tikzText Logical, for whether or not to format text for tikz plotting.
#' @examples
#' # Table of US states' mean travel times to work, from the 2011 ACS
#' data(TravelTime2011)
#' # Just as inside RankPlotWithTable(),
#' # we have to set par(xpd=TRUE)
#' # and adjust the plotting margins
#' oldpar <- par(no.readonly = TRUE)
#' oldmar <- par('mar')
#' par(xpd=TRUE, mar=c(oldmar[1],0,oldmar[3],0))
#' with(TravelTime2011,
#'      RankTable(ranks = Rank, names = State,
#'                est = Estimate.2dec, se = SE.2dec,
#'                placeType = "State"))
#' par(oldpar)
#' @seealso \code{\link{RankPlotWithTable}} and \code{\link{RankPlot}}.
#' @export
RankTable <- function(ranks, names, est, se, placeType = "State",
                     col1 = .15, col2 = .6, col3 = .85, col4 = 1,
                     textPos = 2, titleCex = 0.9,
                     titleLift = 1.5, contentCex = 0.7,
                     columnsPlotRefLine = NULL, tikzText = FALSE) {

  n <- length(ranks)

  if(tikzText) {
    textR = "$\\hat{r}_k$"
    textPlace = paste0("$", placeType, "\\ (k)$")
    textTheta = "$\\hat{\\theta}_k$"
    textSE = "$SE_k$"
  } else {
    textR = expression(hat(r)[k])
    textPlace = paste(placeType, "(k)")
    textTheta = expression(hat(theta)[k])
    textSE = expression(SE[k])
  }

  if(!is.null(columnsPlotRefLine)) {
    plot(c(0,rep(1,n)), 1:(n+1)-.5,
         yaxs='i', type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
  } else {
    plot(c(0, rep(1, n + 1)), 0:(n + 1),
         yaxs='i', type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
  }
  text(rep(col1, n), 1:n, ranks, cex = contentCex, pos = textPos)
  text(col1, n + titleLift, textR, cex = titleCex, pos = textPos, font = 2)
  text(rep(col2, n), 1:n, names, cex = contentCex, pos = textPos)
  text(col2, n + titleLift, textPlace, cex = titleCex, pos = textPos, font = 2)
  text(rep(col3, n), 1:n, est, cex = contentCex, pos = textPos)
  text(col3, n + titleLift, textTheta, cex = titleCex, pos = textPos, font = 2)
  text(rep(col4, n), 1:n, se, cex = contentCex, pos = textPos)
  text(col4, n + titleLift, textSE, cex = titleCex, pos = textPos, font = 2)

  if(!is.null(columnsPlotRefLine)) {
    text(col4, -columnsPlotRefLine, paste0("Reference ", placeType, ":"),
         cex=contentCex, pos=textPos, font=2)
  }
}

