\name{GoogleFinanceSource}
\alias{GoogleFinanceSource}
\title{Get Feed Meta Data from Google Finance...}
\usage{GoogleFinanceSource(query, n=20, ...)
}
\description{Get Feed Meta Data from Google Finance}
\author{Mario Annau}
\value{list}
\arguments{\item{query}{ticker symbols of companies to be searched for, see \url{http://finance.yahoo.com/lookup}.
Please note that Google ticker symbols need to be prefixed with the exchange name, e.g. NASDAQ:MSFT}
\item{n}{number of results (curr. max is ?), defaults to 20}
\item{...}{additional query parameters}
}

