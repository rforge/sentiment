\name{GoogleFinanceSource}
\alias{GoogleFinanceSource}
\title{Get Feed Meta Data from Google Finance...}
\usage{GoogleFinanceSource(query, params=list(hl = "en", q = query, ie = "utf-8", start = 0, num =
    20, output = "rss"), ...)
}
\description{Get Feed Meta Data from Google Finance}
\author{Mario Annau}
\value{WebXMLSource}
\seealso{\code{\link{WebSource}}, \code{\link{readGoogleFinance}}}
\arguments{\item{query}{ticker symbols of companies to be searched for, see \url{http://www.google.com/finance}.
Please note that Google ticker symbols need to be prefixed with the exchange name, e.g. NASDAQ:MSFT}
\item{params,}{additional query parameters}
\item{...}{additional parameters to \code{\link{WebSource}}}
}
\examples{\dontrun{
corpus <- Corpus(GoogleFinanceSource("NASDAQ:MSFT"))
}}

