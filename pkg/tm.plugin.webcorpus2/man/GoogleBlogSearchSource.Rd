\name{GoogleBlogSearchSource}
\alias{GoogleBlogSearchSource}
\title{Get Feed Meta Data from GoogleBlogSearch...}
\usage{GoogleBlogSearchSource(query, n=100, ...)
}
\description{Get Feed Meta Data from GoogleBlogSearch}
\author{Mario Annau}
\value{WebXMLSource}
\arguments{\item{query}{ticker symbols of companies to be searched for, see \url{http://finance.yahoo.com/lookup}.
Please note that Google ticker symbols need to be prefixed with the exchange name, e.g. NASDAQ:MSFT}
\item{n}{number of results (curr. max is ?), defaults to 20}
\item{...}{additional query parameters}
}

