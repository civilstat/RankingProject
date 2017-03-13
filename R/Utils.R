ConfidenceLevelGH <- function(se, confLevel = .90, tol = 0.01) {
  # Based on Goldstein & Healy, using Tommy Wright's notation for z_alpha_a

  n <- length(se)
  k <- matrix(NA, n, n)
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      k[i,j] <- (se[i]+se[j])/sqrt(se[i]^2+se[j]^2)
    }
  }
  k <- c(k[-which(is.na(k))])
  mean_k <- mean(k)

  z_alpha <- qnorm(1 - (1 - confLevel) / 2)
  z_alpha_a <- z_alpha / mean_k
  alpha_a <- 2 * (1 - pnorm(z_alpha_a))

  alpha <- 1 - confLevel
  achieved_alphas <- 2 * (1 - pnorm(z_alpha_a * k))
  avg_alpha <- mean(achieved_alphas)
  message(paste0("Achieved 'alphas' range from ",
                 round(min(achieved_alphas), 4), " to ",
                 round(max(achieved_alphas), 4),
                 ", with mean ", round(avg_alpha, 4)))
  if(abs(alpha - avg_alpha) > tol) {
    warning(paste0("Achieved 'average alpha' = ", round(avg_alpha, 4),
                   " is farther than tol = ", tol,
                   " from target alpha = ", alpha))
  }

  conf.level_a <- 1 - alpha_a
  return(conf.level_a)
}



range2units = function(extrange) {
  exp5 = floor(log10(extrange/2.5))
  if (floor(log2(extrange/(2.5*10^exp5))) == 0) {
    exp2 = exp5 - 1
  } else if (floor(log2(extrange/(2.5*10^exp5))) == 1) {
    exp2 = exp5
  } else {
    exp2 = exp5 + 1
  }
  unit = 2^exp2*5^exp5
  return(unit)
}



#### Pantyhose plot ####

RankColumnPlot <- function(est, se, names, refName = NULL,
                           confLevel = 0.9) {
  signifMatrix <- apply(cbind(est, se), 1, FindSignifInColumn,
                        alldata = cbind(est, se), confLevel = confLevel)
  colnames(signifMatrix) <- names

  n <- length(est)
  refInd <- ifelse(is.null(refName), NA, which(names[order(est)] == refName))

  RankHeatmap(signifMatrix, Rowv=NA, Colv=NA, scale='none',
              col = c('grey80','white','grey50'), cexCol = 0.6,
              add.expr={abline(h=(.5+1:(n-1)),col="grey90"); abline(v=refInd+c(-.5,.5),col="grey90")})
}



FindSignifInColumn <- function(x, alldata, confLevel = 0.9){
  # x[1] and x[2] are the current area's est and se;
  # alldata[, 1] and alldata[, 2] are all the ests and ses.

  # Demi-Bonferroni-correction for error margins, for (n-1) comparisons
  n=nrow(alldata)
  p.Bonf = 1 - ((1 - confLevel)/2)/(n-1)
  q.Bonf = qnorm(p.Bonf)

  x <- as.matrix(x)
  x <- cbind(x[1],x[2])
  (abs(alldata[,1]-x[1]) > q.Bonf*sqrt(alldata[,2]^2+x[2]^2)) * sign(alldata[,1]-x[1])
}


# TODO: clean up and remove anything not strictly necessary.
# Cite that this is based on the internals of stats::heatmap()
# basically just commenting out the lines that use layout()
# in order to get a layout-free heatmap (with no dendograms)
# so we can use the heatmap inside a larger layout
#
# The source is within src/library/stats/R/dendrogram.R
# which states it is licensed under GPL-2,
# and the heatmap() function is prefaced by this comment:
# "original Andy Liaw; modified RG, MM :"
# See for instance here:
# https://github.com/wch/r-source/blob/trunk/src/library/stats/R/dendrogram.R
RankHeatmap <- function (x, Rowv = NULL, Colv = if (symm) "Rowv" else NULL,
                         distfun = dist, hclustfun = hclust, reorderfun = function(d,
                                                                                   w) reorder(d, w), add.expr, symm = FALSE, revC = identical(Colv,
                                                                                                                                              "Rowv"), scale = c("row", "column", "none"), na.rm = TRUE,
                         margins = c(5, 5), ColSideColors, RowSideColors, cexRow = 0.2 +
                           1/log10(nr), cexCol = 0.2 + 1/log10(nc), labRow = NULL,
                         labCol = NULL, oneColumn = FALSE,
                         main = NULL, xlab = NULL, ylab = NULL, keep.dendro = FALSE,
                         verbose = getOption("verbose"), ...)
{
  scale <- if (symm && missing(scale))
    "none"
  else match.arg(scale)
  if (length(di <- dim(x)) != 2 || !is.numeric(x))
    stop("'x' must be a numeric matrix")
  nr <- di[1L]
  nc <- di[2L]
  if (nr <= 1 || nc <= 1)
    stop("'x' must have at least 2 rows and 2 columns")
  if (!is.numeric(margins) || length(margins) != 2L)
    stop("'margins' must be a numeric vector of length 2")
  doRdend <- !identical(Rowv, NA)
  doCdend <- !identical(Colv, NA)
  if (!doRdend && identical(Colv, "Rowv"))
    doCdend <- FALSE
  if (is.null(Rowv))
    Rowv <- rowMeans(x, na.rm = na.rm)
  if (is.null(Colv))
    Colv <- colMeans(x, na.rm = na.rm)
  if (doRdend) {
    if (inherits(Rowv, "dendrogram"))
      ddr <- Rowv
    else {
      hcr <- hclustfun(distfun(x))
      ddr <- as.dendrogram(hcr)
      if (!is.logical(Rowv) || Rowv)
        ddr <- reorderfun(ddr, Rowv)
    }
    if (nr != length(rowInd <- order.dendrogram(ddr)))
      stop("row dendrogram ordering gave index of wrong length")
  }
  else rowInd <- 1L:nr
  if (doCdend) {
    if (inherits(Colv, "dendrogram"))
      ddc <- Colv
    else if (identical(Colv, "Rowv")) {
      if (nr != nc)
        stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
      ddc <- ddr
    }
    else {
      hcc <- hclustfun(distfun(if (symm)
        x
        else t(x)))
      ddc <- as.dendrogram(hcc)
      if (!is.logical(Colv) || Colv)
        ddc <- reorderfun(ddc, Colv)
    }
    if (nc != length(colInd <- order.dendrogram(ddc)))
      stop("column dendrogram ordering gave index of wrong length")
  }
  else colInd <- 1L:nc
  x <- x[rowInd, colInd]
  labRow <- if (is.null(labRow))
    if (is.null(rownames(x)))
      (1L:nr)[rowInd]
  else rownames(x)
  else labRow[rowInd]
  labCol <- if (is.null(labCol))
    if (is.null(colnames(x)))
      (1L:nc)[colInd]
  else colnames(x)
  else labCol[colInd]
  if (scale == "row") {
    x <- sweep(x, 1L, rowMeans(x, na.rm = na.rm), check.margin = FALSE)
    sx <- apply(x, 1L, sd, na.rm = na.rm)
    x <- sweep(x, 1L, sx, "/", check.margin = FALSE)
  }
  else if (scale == "column") {
    x <- sweep(x, 2L, colMeans(x, na.rm = na.rm), check.margin = FALSE)
    sx <- apply(x, 2L, sd, na.rm = na.rm)
    x <- sweep(x, 2L, sx, "/", check.margin = FALSE)
  }
  lmat <- rbind(c(NA, 3), 2:1)
  lwid <- c(if (doRdend) 1 else 0.05, 4)
  lhei <- c((if (doCdend) 1 else 0.05) + if (!is.null(main)) 0.2 else 0,
            4)
  if (!missing(ColSideColors)) {
    if (!is.character(ColSideColors) || length(ColSideColors) !=
        nc)
      stop("'ColSideColors' must be a character vector of length ncol(x)")
    lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 1)
    lhei <- c(lhei[1L], 0.2, lhei[2L])
  }
  if (!missing(RowSideColors)) {
    if (!is.character(RowSideColors) || length(RowSideColors) !=
        nr)
      stop("'RowSideColors' must be a character vector of length nrow(x)")
    lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 1),
                                   1), lmat[, 2] + 1)
    lwid <- c(lwid[1L], 0.2, lwid[2L])
  }
  lmat[is.na(lmat)] <- 0
  if (verbose) {
    cat("layout: widths = ", lwid, ", heights = ", lhei,
        "; lmat=\n")
    print(lmat)
  }
  dev.hold()
  on.exit(dev.flush())
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  #    layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
  if (!missing(RowSideColors)) {
    par(mar = c(margins[1L], 0, 0, 0.5))
    image(rbind(1L:nr), col = RowSideColors[rowInd], axes = FALSE)
  }
  if (!missing(ColSideColors)) {
    par(mar = c(0.5, 0, 0, margins[2L]))
    image(cbind(1L:nc), col = ColSideColors[colInd], axes = FALSE)
  }
  #    par(mar = c(margins[1L], 0, 0, margins[2L]))
  # Find another way to do the margins so it doesn't take up entire space
  # Just using the default values for now;
  #  if doesn't work, we'll have to find out what Joel's code does & match that
  # Ok, works. Now I'd like to cut off the RHS axis labels,
  #  which I'll do by commenting them out further below
  par(mar = c(5.1, 1.1, 4.1, 2.1))
  if (!symm || scale != "none")
    x <- t(x)
  if (revC) {
    iy <- nr:1
    if (doRdend)
      ddr <- rev(ddr)
    x <- x[, iy]
  }
  else iy <- 1L:nr
  ###
  # Here's where it actually plots the heatmap with image()
  image(1L:nc, 1L:nr, x, xlim = 0.5 + c(0, nc), ylim = 0.5 +
          c(0, nr), axes = FALSE, xlab = "", ylab = "", ...)
  ###
  if(!oneColumn)
    axis(1, 1L:nc, labels = labCol, las = 2, line = -0.5, tick = 0,
         cex.axis = cexCol)
  else
    axis(1, 2, labels = labCol[2], las = 1, line = -1, tick = 0,
         cex.axis = cexCol)
  if (!is.null(xlab))
    mtext(xlab, side = 1, line = margins[1L] - 1.25)
  #    axis(4, iy, labels = labRow, las = 2, line = -0.5, tick = 0,
  #        cex.axis = cexRow)
  if (!is.null(ylab))
    mtext(ylab, side = 4, line = margins[2L] - 1.25)
  if (!missing(add.expr))
    # CHANGED to use parent.frame() as the environment,
    # so we can use the values of n etc. from the calling function
    # for the sake of drawing all the horizontal and vertical lines
    eval(substitute(add.expr, parent.frame()))
  par(mar = c(margins[1L], 0, 0, 0))
  #    if (doRdend)
  #        plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
  #    else frame()
  par(mar = c(0, 0, if (!is.null(main)) 1 else 0, margins[2L]))
  #    if (doCdend)
  #        plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none")
  #    else if (!is.null(main))
  #        frame()
  if (!is.null(main)) {
    par(xpd = NA)
    title(main, cex.main = 1.5 * op[["cex.main"]])
  }
  invisible(list(rowInd = rowInd, colInd = colInd, Rowv = if (keep.dendro &&
                                                              doRdend) ddr, Colv = if (keep.dendro && doCdend) ddc))
}

