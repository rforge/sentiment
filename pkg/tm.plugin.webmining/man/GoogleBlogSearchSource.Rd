\name{GoogleBlogSearchSource}
\alias{GoogleBlogSearchSource}
\alias{readGoogleBlogSearch}
\title{Get Feed Meta Data from Google Blog Search \url{http://www.google.com/blogsearch}}
\usage{
  GoogleBlogSearchSource(query,
    params = list(hl = "en", q = query, ie = "utf-8", num = 100, output = "rss"),
    ...)
}
\arguments{
  \item{query}{Google Blog Search query}

  \item{params,}{additional query parameters}

  \item{...}{additional parameters to
  \code{\link{WebSource}}}
}
\value{
  WebXMLSource
}
\description{
  Get Feed Meta Data from Google Blog Search
  \url{http://www.google.com/blogsearch}
}
\examples{
\dontrun{
corpus <- Corpus(GoogleBlogSearchSource("Microsoft"))
}
}
\author{
  Mario Annau
}
\seealso{
  \code{\link{WebSource}}
}

