\name{getMeta.nytimes.articlesearch}
\alias{getMeta.nytimes.articlesearch}
\title{Get Feed Meta Data from NYTimes Article Search...}
\usage{getMeta.nytimes.articlesearch(query, n=100, count=10, appid, verbose=TRUE, ...)
}
\description{Get Feed Meta Data from NYTimes Article Search}
\author{Mario Annau}
\value{list}
\seealso{\code{\link{getFeed}} \code{\link{getCorpus}}}
\arguments{\item{query}{character specifying query to be used to search NYTimes articles}
\item{n}{number of results defaults to 100}
\item{count}{number of results per page, defaults to 10}
\item{appid}{Developer App id to be used, obtained from \url{http://developer.nytimes.com/}}
\item{verbose}{print status output to console, defaults to TRUE}
\item{...}{additional query parameters, see \url{http://developer.nytimes.com/docs/read/article_search_api}}
}
\examples{\dontrun{
query <- c("MSFT","Microsoft")
metalist <- getMeta.nytimes.articlesearch(query, n = 100)
corpus <- getCorpus(query, src = "nytimes.articlesearch", n = 100, appid = #Your App-ID#)
}}

