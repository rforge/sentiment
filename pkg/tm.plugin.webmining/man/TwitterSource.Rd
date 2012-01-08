\name{TwitterSource}
\alias{readTwitter}
\alias{TwitterSource}
\title{Get Feed Meta Data from Twitter}
\usage{
  TwitterSource(query, n = 1500,
    params = list(lang = "en"), ...)
}
\arguments{
  \item{query}{Google Blog Search query}

  \item{n}{number of results, defaults to 1500}

  \item{params,}{additional query parameters, see
  \url{http://search.twitter.com/api/}}

  \item{...}{additional parameters to
  \code{\link{WebSource}}}
}
\value{
  WebXMLSource
}
\description{
  Get Feed Meta Data from Twitter
}
\examples{
\dontrun{
corpus <- Corpus(TwitterSource("Microsoft"))
}
}
\author{
  Mario Annau
}
\seealso{
  \code{\link{WebSource}}
}

