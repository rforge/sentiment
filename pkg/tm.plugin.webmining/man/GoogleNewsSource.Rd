\name{GoogleNewsSource}
\alias{GoogleNewsSource}
\title{Get Feed Meta Data from Google News Search http://news...}
\usage{GoogleNewsSource(query, params=list(hl = "en", q = query, ie = "utf-8", num = 100,
    output = "rss"), ...)
}
\description{Get Feed Meta Data from Google News Search \url{http://news.google.com/}}
\author{Mario Annau}
\value{WebXMLSource}
\seealso{\code{\link{WebSource}}, \code{\link{readGoogle}}}
\arguments{\item{query}{Google News Search query}
\item{params,}{additional query parameters}
\item{...}{additional parameters to \code{\link{WebSource}}}
}
\examples{\dontrun{
corpus <- Corpus(GoogleNewsSource("Microsoft"))
}}

