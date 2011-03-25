\name{getMeta.bing}
\alias{getMeta.bing}
\title{Get Feed Meta Data from MS Bing Live Search API...}
\usage{getMeta.bing(query, n=100, count=10, appid, sources="news", market="en-US",
    verbose=TRUE, ...)
}
\description{Get Feed Meta Data from MS Bing Live Search API}
\author{Mario Annau}
\value{list}
\seealso{\code{\link{getFeed}} \code{\link{getCorpus}}}
\arguments{\item{query}{character vector of keywords to be searched for}
\item{n}{number of results (curr. max is ?), defaults to 100}
\item{count}{number of results per page, defaults to 10}
\item{appid}{Developer App id to be used obtained from \url{http://www.bing.com/developers}}
\item{sources}{Specifies source type like "news", "web" or "video", see \url{http://msdn.microsoft.com/en-us/library/bb266167.aspx}}
\item{market}{Market for which search should be done, defaults to "en-US" (typically offers most results)}
\item{verbose}{print status output to console, defaults to TRUE}
\item{...}{additional query parameters, see \url{http://msdn.microsoft.com/en-us/library/bb266180.aspx}}
}
\examples{\dontrun{
query <- c("MSFT","Microsoft")
metalist <- getMeta.bing(query, n = 20)
corpus <- getCorpus(query, src = "bing", n = 20, appid = #Your App-ID#)
}}

