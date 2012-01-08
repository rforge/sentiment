\name{YahooFinanceSource}
\alias{readYahoo}
\alias{YahooFinanceSource}
\title{Get Feed Meta Data from Yahoo Finance}
\usage{
  YahooFinanceSource(query,
    params = list(s = query, n = 20), ...)
}
\arguments{
  \item{query}{ticker symbols of companies to be searched
  for, see \url{http://finance.yahoo.com/lookup}.}

  \item{params,}{additional query parameters, see
  \url{http://developer.yahoo.com/rss/}}

  \item{...}{additional parameters to
  \code{\link{WebSource}}}
}
\value{
  WebXMLSource
}
\description{
  Get Feed Meta Data from Yahoo Finance
}
\examples{
\dontrun{
corpus <- Corpus(YahooFinanceSource("MSFT"))
}
}
\author{
  Mario Annau
}
\seealso{
  \code{\link{WebSource}}
}

