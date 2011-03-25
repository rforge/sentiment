\name{getMeta.yahooFinance}
\alias{getMeta.yahooFinance}
\title{Get Feed Meta Data from Yahoo! Finance...}
\usage{getMeta.yahooFinance(query, n=20, verbose=TRUE, ...)
}
\description{Get Feed Meta Data from Yahoo! Finance}
\author{Mario Annau}
\value{list}
\seealso{\code{\link{getFeed}} \code{\link{getCorpus}}}
\arguments{\item{query}{ticker symbols of companies to be searched for, see \url{http://finance.yahoo.com/lookup}}
\item{n}{number of results (curr. max is 20)}
\item{verbose}{print status output to console, defaults to TRUE}
\item{...}{additional query parameters, see \url{http://developer.yahoo.com/finance/company.html}}
}
\examples{\dontrun{
metalist <- getMeta.yahooFinance("MSFT", n = 20)
corpus <- getCorpus("MSFT", src = "yahooFinance", n = 20)
}}

