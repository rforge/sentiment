\name{NYTimesSource}
\alias{NYTimesSource}
\title{Get Feed Meta Data from NYTimes Article Search...}
\usage{NYTimesSource(query, n=100, count=10, appid, params=list(format = "json", query =
    query, offset = seq(0, n - count, by = count), `api-key` = appid),
    ...)
}
\description{Get Feed Meta Data from NYTimes Article Search}
\author{Mario Annau}
\seealso{\code{\link{WebSource}}, \code{\link{readNYTimes}}}
\arguments{\item{query}{character specifying query to be used to search NYTimes articles}
\item{n}{number of results defaults to 100}
\item{count}{number of results per page, defaults to 10}
\item{appid}{Developer App id to be used, obtained from \url{http://developer.nytimes.com/}}
\item{params}{additional query parameters, specified as list, see \url{http://developer.nytimes.com/docs/read/article_search_api}}
\item{...}{additional parameters to \code{\link{WebSource}}}
}
\examples{\dontrun{
#nytimes_appid needs to be specified
corpus <- Corpus(NYTimesSource("Microsoft", appid = nytimes_appid))
}}

