\name{getMeta.googleFinance}
\alias{getMeta.googleFinance}
\title{Get Feed Meta Data from Google Finance...}
\usage{getMeta.googleFinance(query, n=20, verbose=TRUE, ...)
}
\description{Get Feed Meta Data from Google Finance}
\author{Mario Annau}
\value{list}
\seealso{\code{\link{getFeed}} \code{\link{getCorpus}}}
\arguments{\item{query}{ticker symbols of companies to be searched for, see \url{http://finance.yahoo.com/lookup}.
Please note that Google ticker symbols need to be prefixed with the exchange name, e.g. NASDAQ:MSFT}
\item{n}{number of results (curr. max is ?), defaults to 20}
\item{verbose}{print status output to console, defaults to TRUE}
\item{...}{additional query parameters}
}
\examples{\dontrun{
metalist <- getMeta.googleFinance("NASDAQ:MSFT", n = 20)
corpus <- getCorpus("NASDAQ:MSFT", src = "googleFinance", n = 20)
}}

