#' The Ranking Project: Visualizations for Comparing Populations
#'
#' Functions to generate plots and tables for comparing independently-sampled
#' populations. Companion package to "A Primer on Visualizations for Comparing
#' Populations, Including the Issue of Overlapping Confidence Intervals"
#' (Wright, Klein, and Wieczorek, 2017, The American Statistician). See the
#' \link[=../doc/intro.html]{Intro vignette (html)} for an overview and examples,
#' or the \link[=../doc/primer.pdf]{Primer vignette (pdf)}
#' for code which replicates the main figures from the article.
#'
#' The "comparison" plots are based on figures and S code from
#' "Displays for Comparing a Given State to Many Others"
#' (Almond, Lewis, Tukey, and Yan, 2000, The American Statistician).
#' The present package does not rely directly on Almond et al.'s S code,
#' but draws inspiration from it. Their script was originally hosted at
#' Statlib at \url{http://stat.cmu.edu/S/comprB} and may still be found at
#' various Statlib mirrors, such as
#' \url{http://ftp.uni-bayreuth.de/math/statlib/S/comprB}.
#'
#' The code for the "columns" plots is directly based on R's stats::heatmap()
#' function, with minor modifications to remove dendograms and allow the heatmap
#' to be placed inside a larger layout().
#'
#' @docType package
#' @name RankingProject
NULL
