
RankTable <- function(ranks, names, est, se, placeType = "State",
                     col1 = .15, col2 = .6, col3 = .85, col4 = 1,
                     textPos = 2, titleCex = 0.9, titleLift = 1.5, contentCex = 0.7,
                     pantyhoseRefLine = NULL, tikzText = NULL) {

  n <- length(ranks)

  if(!is.null(tikzText) & isTRUE(tikzText)) {
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
  text(rep(col1, n), 1:n, ranks, cex = contentCex, pos = textPos)
  text(col1, n + titleLift, textR, cex = titleCex, pos = textPos, font = 2)
  text(rep(col2, n), 1:n, names, cex = contentCex, pos = textPos)
  text(col2, n + titleLift, textPlace, cex = titleCex, pos = textPos, font = 2)
  text(rep(col3, n), 1:n, est, cex = contentCex, pos = textPos)
  text(col3, n + titleLift, textTheta, cex = titleCex, pos = textPos, font = 2)
  text(rep(col4, n), 1:n, se, cex = contentCex, pos = textPos)
  text(col4, n + titleLift, textSE, cex = titleCex, pos = textPos, font = 2)

  if(!is.null(pantyhoseRefLine)) {
    text(col4, -pantyhoseRefLine, paste0("Reference ", placeType, ":"), cex=contentCex, pos=textPos, font=2)
  }
}

