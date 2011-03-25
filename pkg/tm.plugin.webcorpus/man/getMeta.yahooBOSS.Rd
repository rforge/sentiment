\name{getMeta.yahooBOSS}
\alias{getMeta.yahooBOSS}
\title{Get Feed Meta Data from Yahoo BOSS...}
\usage{getMeta.yahooBOSS(query, type="news", n=100, count=min(50, n), appid, lang="en",
    verbose=TRUE, age="30d", ...)
}
\description{Get Feed Meta Data from Yahoo BOSS}
\author{Mario Annau}
\value{list}
\seealso{\code{\link{getFeed}} \code{\link{getCorpus}}}
\arguments{\item{query}{character vector of keywords to be searched for}
\item{type}{Specifies source type, defaults to "news", see \url{http://developer.yahoo.com/search/boss/boss_guide/index.html} for other}
\item{n}{number of results (curr. max is ?), defaults to 100, (max is ~1000)}
\item{count}{number of results per page, defaults to 50}
\item{appid}{Developer App id to be used obtained from \url{http://developer.yahoo.com/search/boss/}}
\item{lang}{Language of search results, deraults to "en"}
\item{verbose}{print status output to console, defaults to TRUE}
\item{age,}{maximum age of search results, defaults to "30d" (maximum is 30 days)}
\item{...}{additional query parameters, see \url{http://developer.yahoo.com/search/boss/boss_guide/index.html}}
}
\examples{\dontrun{
query <- c("MSFT","Microsoft")
metalist <- getMeta.yahooBOSS(query, n = 1000)
corpus <- getCorpus(query, src = "yahooBOSS", n = 20, appid = #Your App-ID#)
}}

