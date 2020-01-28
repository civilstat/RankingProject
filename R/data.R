#' Mean travel times to work, from 2011 ACS.
#'
#' A dataset containing the estimated mean travel time (in minutes) to work
#' of workers 16 years and over who did not work at home
#' (henceforth "mean travel time to work"), and its estimated standard error,
#' for each of the 51 states (including Washington, D.C.),
#' from the 2011 American Community Survey.
#'
#' @format A data frame with 51 rows and 7 variables:
#' \describe{
#'   \item{Rank}{state rank, by estimated mean travel time, where 1 is lowest travel time and 51 is highest}
#'   \item{State}{full name of the state}
#'   \item{Estimate.2dec}{estimated mean travel time, in minutes}
#'   \item{SE.2dec}{estimated standard error of the estimated mean travel time, in minutes}
#'   \item{Abbreviation}{postal abbreviation of the state}
#'   \item{Region}{factor variable for geographic region of the state: Northeast, South, Midwest, West, Pacific}
#'   \item{FIPS}{Federal Information Processing Standard (FIPS) code of the state; may be useful for linking with other datasets}
#' }
#' @source \url{https://www.census.gov/}
"TravelTime2011"


#' Mean travel times to work, from 2011 ACS, rounded to 1 decimal place.
#'
#' A dataset containing the estimated mean travel time (in minutes) to work
#' of workers 16 years and over who did not work at home
#' (henceforth "mean travel time to work"), and its estimated Margin of Error
#' at the 90\% confidence level,
#' for each of the 51 states (including Washington, D.C.),
#' from the 2011 American Community Survey.
#'
#' Due to rounding, some ranks are tied in this version of the data.
#' Also note that this dataset reports Margins of Error (MoEs)
#' instead of standard errors.
#'
#' @format A data frame with 51 rows and 7 variables:
#' \describe{
#'   \item{Rank}{state rank, by estimated mean travel time, where 1 is lowest travel time and 51 is highest}
#'   \item{State}{full name of the state}
#'   \item{Estimate.1dec}{estimated mean travel time, in minutes}
#'   \item{MOE.1dec}{estimated Margin of Error (at the 90\% confidence level) of the estimated mean travel time, in minutes}
#'   \item{Abbreviation}{postal abbreviation of the state}
#'   \item{Region}{factor variable for geographic region of the state: Northeast, South, Midwest, West, Pacific}
#'   \item{FIPS}{Federal Information Processing Standard (FIPS) code of the state; may be useful for linking with other datasets}
#' }
#' @source \url{https://www.census.gov/}
"TravelTime2011.1dec"
