# RankingProject

The package `RankingProject` is a companion for the article "A Primer on Visualizations for Comparing Populations, Including the Issue of Overlapping Confidence Intervals" (Wright, Klein, and Wieczorek, 2017, *The American Statistician*, in press).

The package provides functions for plotting ranked tables of data side-by-side with their plots. The available visualizations include shaded columns plots, adjusted confidence intervals, and related plots intended for making correct inferences about one-to-many or many-to-many comparisons.

## Installation

```{r}
# install.packages("devtools")
# library("devtools")
install_github("civilstat/RankingProject")
```

To install the vignettes and exactly replicate figures from the paper,
you will also need the `tikzDevice` package:

```{r}
# install.packages("tikzDevice")
install_github("civilstat/RankingProject", build_vignettes = TRUE)
```

## Examples

```{r}
library(RankingProject)

# Load dataset of mean travel time (in minutes) to work
# of workers 16 years and over who did not work at home,
# from the 2011 American Community Survey (ACS)
data(TravelTime2011)
USdata  <- TravelTime2011
head(USdata)

# Format estimates and SEs into strings with 2 digits past the decimal
USdata$Estimate.Print = formatC(USdata$Estimate.2dec,
                                format = 'f', digits = 2)
# For SEs, also drop the leading 0
USdata$SE.Print = substring(formatC(USdata$SE.2dec,
                                    format = 'f', digits = 2),
                            first = 2)

# Set USA as the reference area
refAbbr <- "USA"
refRow  <- which(USdata$Abbreviation==refAbbr)

# Set up parameter lists for table function and plot function
tableParList <- with(USdata,
                     list(ranks = Rank, names = State,
                          est = Estimate.Print, se = SE.Print,
                          placeType = "State"))
plotParList <- with(USdata,
                      list(est = Estimate.2dec, se = SE.2dec,
                           names = Abbreviation, refName = refAbbr,
                           confLevel = .9, cex = 0.6,
                           plotType = "individual"))

# Plot individual 90% CIs alongside a ranking table
RankPlotWithTable(tableParList = tableParList, plotParList = plotParList)
```

## References

Almond, R.G., Lewis, C., Tukey, J.W., and Yan, D. (2000).
"Displays for Comparing a Given State to Many Others,"
*The American Statistician*, vol. 54, no. 2, 89-93.

Wright, T., Klein, M., and Wieczorek, J. (2017).
"A Primer on Visualizations for Comparing Populations,
Including the Issue of Overlapping Confidence Intervals,"
*The American Statistician*, in press.
