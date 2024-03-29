% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rhospno2.R
\docType{data}
\name{rhospno2}
\alias{rhospno2}
\title{Respiratory hospital admissions and air pollution.}
\format{
A data frame with 3,652 rows and 7 variables:
\describe{
\item{date}{date of the observation}
\item{t}{numerical indicator of date}
\item{year}{year indicator}
\item{dow}{day of week}
\item{temp}{ambient temperature in Celsius degrees}
\item{no2}{NO\eqn{_2} concentration in \eqn{\mu}g/m\eqn{^3}}
\item{hresp}{number of hospital admissions for respiratory causes}
}
}
\usage{
rhospno2
}
\description{
Simulated data for daily measures of hospital admissions for respiratory causes
count and ambient air NO\eqn{_2} concentrations during 10 years.
}
\details{
This is a synthetic dataset generated with the \code{synthpop} package, based on
true data on daily number of hospital admissions for respiratory causes and
ambient air NO\eqn{_2} concentrations in the city of Barcelona (Spain) for years
2006-2015.
}
\examples{
 # time series:
 par(las = 1, mfrow = c(3, 1))
 with(rhospno2, plot(date, hresp, type = "l", lwd = 0.5))
 with(rhospno2, plot(date, no2, type = "l", lwd = 0.5))
 with(rhospno2, plot(date, temp, type = "l", lwd = 0.5))
}
\keyword{datasets}
