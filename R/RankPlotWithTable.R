# Do something about the fact that ___.rankCompPlot funcions ask for refName
# (which I'd rather use in the first line of inputs instead of refFullName)

RankPlotWithTable = function(file, figwidth, figheight,
                             tablewidthProp = 3/8,
                             tableParList, figureParList, annotParList = NULL,
                             figureFunction = SIMPLE.rankCompPlot,
                             tableFunction = RankTable) {

  oldpar <- par(no.readonly = TRUE)
  oldmar <- par('mar')

  # We will be "plotting" to a .TEX file using tikz
  tikz(file, standAlone = TRUE, width = figwidth, height = figheight)

  # Set up the layout: table on left, figure on right
  layout(matrix(c(1, 2), 1, 2), widths = c(tablewidthProp, 1 - tablewidthProp))

  # Create a table on the left
  par(xpd=TRUE, mar=c(oldmar[1],0,oldmar[3],0))
  do.call(tableFunction, tableParList)

  # Create a figure on the right
  par(xpd = FALSE, mar = c(oldmar[1], 1.1, oldmar[3], oldmar[4]))
  do.call(figureFunction, figureParList)

  # Create annotations on the bottom
  if(!is.null(annotParList)) {
    mtext(paste0("Reference ", tableParList$PlaceType, " ($k^*$):~ "), side=1, at=0, line=2, adj=1)
    mtext(annotParList$refFullName, side=1, at=0, line=2, adj=0)
    mtext("Rank:~ ", side=1, at=0, line=3, adj=1)
    mtext(annotParList$refRank, side=1, at=0, line=3, adj=0)
  }

  dev.off()

  par(oldpar)
}

