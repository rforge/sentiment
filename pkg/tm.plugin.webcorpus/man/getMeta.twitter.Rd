\name{getMeta.twitter}
\alias{getMeta.twitter}
\title{Get Feed Meta Data from Twitter...}
\usage{getMeta.twitter(query, count=100, n=100, verbose=TRUE, ...)
}
\description{Get Feed Meta Data from Twitter}
\author{Mario Annau}
\value{list}
\seealso{\code{\link{getFeed}} \code{\link{getCorpus}}}
\arguments{\item{query}{character specifying query to be used to search tweets}
\item{count}{number of results per page, defaults to 100}
\item{n}{number of results, defaults to 100}
\item{verbose}{print status output to console, defaults to TRUE}
\item{...}{additional query parameters, see \url{http://search.twitter.com/api/}}
}
\examples{\dontrun{
query <- c("MSFT","Microsoft")
metalist <- getMeta.twitter(query, n = 100)
corpus <- getCorpus(query, src = "twitter", n = 100)
}}

