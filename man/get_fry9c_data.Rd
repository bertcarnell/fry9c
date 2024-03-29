% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_fry9c_data.R
\name{get_fry9c_data}
\alias{get_fry9c_data}
\title{Get FR Y-9c data}
\usage{
get_fry9c_data(year, quarter, verbose = TRUE)
}
\arguments{
\item{year}{the year requested starting in 2017 (for prior years, use \code{\link{get_fry9c_historical_data}})}

\item{quarter}{the quarter requested [1,2,3,4]}

\item{verbose}{should messages be printed? (Default: TRUE)}
}
\value{
a \code{data.frame} containing the results
}
\description{
Query the FFIEC (Federal Financial Institutions Examination Council)
NIC (National Information Center) to obtain the requested year and quarter FR Y-9c
}
\details{
\url{https://www.ffiec.gov/npw/FinancialReport/FinancialDataDownload}
}
\examples{
\dontrun{X <- get_fry9c_data(2021, 1)}
}
\seealso{
\code{\link{get_fry9c_historical_data}}
}
