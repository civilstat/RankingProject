#' The Ranking Project: Visualizations for Comparing Populations
#'
#' Functions to generate plots and tables for comparing independently-sampled
#' populations. Companion package to "A Primer on Visualizations for Comparing
#' Populations, Including the Issue of Overlapping Confidence Intervals"
#' (Wright, Klein, and Wieczorek, 2017,
#' \emph{The American Statistician}, in press).
#' See the Intro vignette (html) for an overview and examples:
#' \code{vignette("intro", package = "RankingProject")}.
#' See the Primer vignette (pdf)
#' for code which replicates the main figures from the article:
#' \code{vignette("primer", package = "RankingProject")}.
#'
#' The "comparison" plots are based on figures and S code from
#' Almond et al. (2000).
#' The present package does not contain a direct modification of their S code,
#' but draws inspiration from it. Their script was originally hosted at
#' Statlib at \url{http://stat.cmu.edu/S/comprB} and may still be found at
#' Statlib mirrors such as
#' \url{http://ftp.uni-bayreuth.de/math/statlib/S/comprB}.
#'
#' The code for the "columns" plots is directly based on R's
#' \code{stats::heatmap()}
#' function, with minor modifications to remove dendograms and allow the heatmap
#' to be placed inside a larger \code{layout()}.
#'
#' @references Almond, R.G., Lewis, C., Tukey, J.W., and Yan, D. (2000).
#'   "Displays for Comparing a Given State to Many Others,"
#'   \emph{The American Statistician}, vol. 54, no. 2, 89-93.
#'
#'   Wright, T., Klein, M., and Wieczorek, J. (2017).
#'   "A Primer on Visualizations for Comparing Populations,
#'   Including the Issue of Overlapping Confidence Intervals,"
#'   \emph{The American Statistician}, in press.
#'
#' @importFrom graphics abline arrows axis image layout legend mtext par plot
#'   points rect segments text title
#' @importFrom grDevices dev.flush dev.hold
#' @importFrom stats as.dendrogram dist hclust order.dendrogram pnorm qnorm
#'   reorder sd
#'
#' @docType package
#' @name RankingProject
NULL
