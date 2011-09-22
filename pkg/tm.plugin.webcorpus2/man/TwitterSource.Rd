\name{TwitterSource}
\alias{TwitterSource}
\title{Get Feed Meta Data from Twitter...}
\usage{TwitterSource(query, n=1500, params=list(q = query, rpp = 100, page = seq(1,
    ceiling(n/100), by = 1)), ...)
}
\description{Get Feed Meta Data from Twitter}
\author{Mario Annau}
\value{WebXMLSource}
\seealso{\code{\link{WebSource}}, \code{\link{readTwitter}}}
\arguments{\item{query}{Google Blog Search query}
\item{n}{number of results, defaults to 1500}
\item{params,}{additional query parameters, see \url{http://search.twitter.com/api/}}
\item{...}{additional parameters to \code{\link{WebSource}}}
}
\examples{\dontrun{
corpus <- Corpus(TwitterSource("Microsoft"))
}}

