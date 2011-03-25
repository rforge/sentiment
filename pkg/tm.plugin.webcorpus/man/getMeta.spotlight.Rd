\name{getMeta.spotlight}
\alias{getMeta.spotlight}
\title{Get Feed Meta Data and Content from Reuters Spotlight...}
\usage{getMeta.spotlight(query="newsOne", edition="us", content="channelarticles", n=100,
    type="json", appid, verbose=TRUE, ...)
}
\description{Get Feed Meta Data and Content from Reuters Spotlight}
\author{Mario Annau}
\value{list}
\seealso{\code{\link{getFeed}} \code{\link{getCorpus}}}
\arguments{\item{query}{character specifying news channel to be used, defaults to "newsOne", see \url{http://spotlight.reuters.com/page/2007/07/10/feeds} for all available channels}
\item{edition}{specifies edition to be used, like "us", "uk", "ru", see \url{http://spotlight.reuters.com/page/2007/07/10/feeds} for all available editions}
\item{content}{Content type to be used, like "channelarticles", "channelnews", "channelphotos" or "channelvideos"}
\item{n}{number of results defaults to 100, (max is 100)}
\item{type}{\code{character} specifying content type to be used, can either be "json", "atom" or "rss", defaults to "json". Please note that no content is integrated in RSS feed.}
\item{appid}{Developer App id to be used, obtained from \url{http://spotlight.reuters.com/}}
\item{verbose}{print status output to console, defaults to TRUE}
\item{...}{additional query parameters, see \url{http://spotlight.reuters.com/page/2007/07/10/feeds}}
}
\examples{\dontrun{
#Retrieve Gold Market Report from Reuters Spotlight
query <- "goldMktRpt"
metalist <- getMeta.spotlight(query, n = 100)
corpus <- getCorpus(query, src = "spotlight", n = 100, appid = #Your App-ID#)
}}

