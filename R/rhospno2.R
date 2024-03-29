#' Respiratory hospital admissions and air pollution.
#'
#' @description Simulated data for daily measures of hospital admissions for respiratory causes
#'   count and ambient air NO\eqn{_2} concentrations during 10 years.
#'
#' @format A data frame with 3,652 rows and 7 variables:
#' \describe{
#' \item{date}{date of the observation}
#' \item{t}{numerical indicator of date}
#' \item{year}{year indicator}
#' \item{dow}{day of week}
#' \item{temp}{ambient temperature in Celsius degrees}
#' \item{no2}{NO\eqn{_2} concentration in \eqn{\mu}g/m\eqn{^3}}
#' \item{hresp}{number of hospital admissions for respiratory causes}
#' }
#'
#' @details This is a synthetic dataset generated with the \code{synthpop} package, based on
#'   true data on daily number of hospital admissions for respiratory causes and
#'   ambient air NO\eqn{_2} concentrations in the city of Barcelona (Spain) for years
#'   2006-2015.
#'
#' @examples
#'  # time series:
#'  par(las = 1, mfrow = c(3, 1))
#'  with(rhospno2, plot(date, hresp, type = "l", lwd = 0.5))
#'  with(rhospno2, plot(date, no2, type = "l", lwd = 0.5))
#'  with(rhospno2, plot(date, temp, type = "l", lwd = 0.5))
"rhospno2"
