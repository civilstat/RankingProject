
RankTable <- function(Rank, PlaceName, Est, SE, PlaceType = "State",
                     col1 = .15, col2 = .6, col3 = .85, col4 = 1,
                     textPos = 2, titleCex = 0.9, titleLift = 1.5, contentCex = 0.7,
                     pantyhoseRefLine = NULL, ...) {
  # TODO: can/should we remove ... from function defintion?

  n <- length(Rank)
  # "textPos = 2" is right-aligned;
  # col1, ..., col4 should be the fractions of the table's width
  # at which each column should end;
  # col4 = 1 unless you want the table to be narrower than the space available,
  # or unless you switch to centered or left-aligned columns.

  if(!is.null(pantyhoseRefLine)) {
    plot(c(0,rep(1,n)), 1:(n+1)-.5, yaxs='i', type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
  } else {
    plot(c(0, rep(1, n + 1)), 0:(n + 1), yaxs='i', type='n', bty='n', xaxt='n', yaxt='n', xlab='', ylab='')
  }
  text(rep(col1, n), 1:n, Rank, cex = contentCex, pos = textPos)
  text(col1, n + titleLift, "$\\hat{r}_k$", cex = titleCex, pos = textPos, font = 2)
  text(rep(col2, n), 1:n, PlaceName, cex = contentCex, pos = textPos)
  text(col2, n + titleLift, paste0("$", PlaceType, "\\ (k)$"), cex = titleCex, pos = textPos, font = 2)
  text(rep(col3, n), 1:n, Est, cex = contentCex, pos = textPos)
  text(col3, n + titleLift, "$\\hat{\\theta}_k$", cex = titleCex, pos = textPos, font = 2)
  text(rep(col4, n), 1:n, SE, cex = contentCex, pos = textPos)
  text(col4, n + titleLift, "$SE_k$", cex = titleCex, pos = textPos, font = 2)

  if(!is.null(pantyhoseRefLine)) {
    text(col4, -pantyhoseRefLine, paste0("Reference ", PlaceType, ":"), cex=contentCex, pos=textPos, font=2)
  }
}

