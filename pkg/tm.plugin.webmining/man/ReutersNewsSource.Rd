\name{ReutersNewsSource}
\alias{readReutersNews}
\alias{ReutersNewsSource}
\title{Get Feed Meta Data from Reuters News RSS Feeds}
\usage{
  ReutersNewsSource(query = "businessNews", ...)
}
\arguments{
  \item{query}{Reuters News RSS Feed, see
  \url{http://www.reuters.com/tools/rss} for a list of all
  feeds provided. Note that only string after
  'http://feeds.reuters.com/reuters/' must be given.
  Defaults to 'businessNews'.}

  \item{...}{additional parameters to
  \code{\link{WebSource}}}
}
\value{
  WebXMLSource
}
\description{
  Get Feed Meta Data from Reuters News RSS Feeds
}
\examples{
\dontrun{
corpus <- Corpus(ReutersNewsSource("businessNews"))
}
}
\author{
  Mario Annau
}
\seealso{
  \code{\link{WebSource}}
}

