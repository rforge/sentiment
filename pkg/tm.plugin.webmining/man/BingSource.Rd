\name{BingSource}
\alias{BingSource}
\alias{readBing}
\title{Get Feed Meta Data from Bing Live Search API}
\usage{
  BingSource(query, n = 100, count = 10, appid,
    sources = "news",
    params = list(Appid = appid, query = query, sources = sources, market = "en-US"),
    ...)
}
\arguments{
  \item{query}{character specifying query to be used to
  search tweets}

  \item{n}{number of results (curr. max is ?), defaults to
  100}

  \item{count}{number of results per page, defaults to 10}

  \item{appid}{Developer App id to be used obtained from
  \url{http://www.bing.com/developers}}

  \item{sources,}{source type, defaults to "news", see
  \url{http://msdn.microsoft.com/en-us/library/dd250847.aspx}
  for additional source types}

  \item{params}{additional query parameters, see
  \url{http://msdn.microsoft.com/en-us/library/dd251056.aspx}}

  \item{...}{additional parameters to
  \code{\link{WebSource}}}
}
\value{
  WebXMLSource
}
\description{
  Get Feed Meta Data from Bing Live Search API
}
\examples{
\dontrun{
#set appid, obtained from http://www.bing.com/developers
corpus <- Corpus(BingSource("Microsoft", appid = appid))
}
}
\author{
  Mario Annau
}
\seealso{
  \code{\link{WebSource}}, \code{\link{readBing}}
}

