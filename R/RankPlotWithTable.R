# TODO: allow moving table from left to right?
# but that would also require adjusting plots,
# so that the y-axis ticks are on right side,
# and adjusting all the text...
# Let's skip this feature for now!

#' @export
RankPlotWithTable = function(tableParList, figureParList, annotParList = NULL,
                             tableFunction = RankTable,
                             figureFunction = RankPlot,
                             tablewidthProp = 3/8) {

  oldpar <- par(no.readonly = TRUE)
  oldmar <- par('mar')

  # Set up the layout: table on left, figure on right
  layout(matrix(c(1, 2), 1, 2),
         widths = c(tablewidthProp, 1 - tablewidthProp))

  # Create a table on the left
  par(xpd=TRUE, mar=c(oldmar[1],0,oldmar[3],0))
  do.call(tableFunction, tableParList)

  # Create a figure on the right
  par(xpd = FALSE, mar = c(oldmar[1], 1.1, oldmar[3], oldmar[4]))
  do.call(figureFunction, figureParList)

  if(!is.null(annotParList)) {
    if(!is.null(annotParList$tikzText) & isTRUE(annotParList$tikzText)) {
      textK = "($k^*$):~"
      textRank = "Rank:~ "
    } else {
      textK = "(k*):"
      textRank = "Rank: "
    }
    # Create annotations on the bottom of the figure
    mtext(paste0("Reference ", tableParList$PlaceType, " ", textK, " "),
          side=1, at=0, line=2, adj=1)
    mtext(annotParList$refFullName, side=1, at=0, line=2, adj=0)
    mtext(textRank, side=1, at=0, line=3, adj=1)
    mtext(annotParList$refRank, side=1, at=0, line=3, adj=0)
  }

  par(oldpar)
}

