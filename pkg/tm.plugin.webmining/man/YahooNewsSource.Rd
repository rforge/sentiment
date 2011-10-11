\name{YahooNewsSource}
\alias{YahooNewsSource}
\title{Get Feed Meta Data from Yahoo News...}
\usage{YahooNewsSource(query, params=list(p = query, n = 20, ei = "UTF-8"), ...)
}
\description{Get Feed Meta Data from Yahoo News}
\author{Mario Annau}
\value{WebXMLSource}
\seealso{\code{\link{WebSource}}, \code{\link{readYahoo}}}
\arguments{\item{query}{words to be searched in Yahoo News, multiple words must be separated by '+'}
\item{params,}{additional query parameters, see \url{http://developer.yahoo.com/rss/}}
\item{...}{additional parameters to \code{\link{WebSource}}}
}
\examples{\dontrun{
corpus <- Corpus(YahooNewsSource("Microsoft"))
}}
