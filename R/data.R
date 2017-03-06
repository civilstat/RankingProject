#' Mean travel times to work, from 2011 ACS.
#'
#' A dataset containing the estimated mean travel time (in minutes) to work
#' of workers 16 years and over who did not work at home
#' (henceforth "mean travel time to work"), and its estimated standard error,
#' for each of the 51 states (including Washington, D.C.)
#' as well as for the whole USA,
#' from the 2011 American Community Survey.
#'
#' @format A data frame with 52 rows and 7 variables:
#' \describe{
#'   \item{Rank}{state rank, by estimated mean travel time, where 1 is lowest travel time and 51 is highest (USA row has NA Rank)}
#'   \item{State}{full name of the state or area}
#'   \item{Estimate.2dec}{estimated mean travel time, in minutes}
#'   \item{SE.2dec}{estimated standard error of the estimated mean travel time, in minutes}
#'   \item{Abbreviation}{postal abbreviation of the state or area}
#'   \item{Region}{factor variable for geographic region of the state: Northeast, South, Midwest, West, Pacific (USA row has blank Region)}
#'   \item{FIPS}{Federal Information Processing Standard (FIPS) code of the state or area; may be useful for linking with other datasets}
#' }
#' @source \url{http://www.census.gov/}
"TravelTime2011"
