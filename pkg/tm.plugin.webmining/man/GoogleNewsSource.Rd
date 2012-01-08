\name{GoogleNewsSource}
\alias{GoogleNewsSource}
\title{Get Feed Meta Data from Google News Search \url{http://news.google.com/}}
\usage{
  GoogleNewsSource(query,
    params = list(hl = "en", q = query, ie = "utf-8", num = 100, output = "rss"),
    ...)
}
\arguments{
  \item{query}{Google News Search query}

  \item{params,}{additional query parameters}

  \item{...}{additional parameters to
  \code{\link{WebSource}}}
}
\value{
  WebXMLSource
}
\description{
  Get Feed Meta Data from Google News Search
  \url{http://news.google.com/}
}
\examples{
\dontrun{
corpus <- Corpus(GoogleNewsSource("Microsoft"))
}
}
\author{
  Mario Annau
}
\seealso{
  \code{\link{WebSource}}
}

