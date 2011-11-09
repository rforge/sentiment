\name{GoogleBlogSearchSource}
\alias{GoogleBlogSearchSource}
\title{Get Feed Meta Data from Google Blog Search http://www...}
\usage{GoogleBlogSearchSource(query, params=list(hl = "en", q = query, ie = "utf-8", num = 100,
    output = "rss"), ...)
}
\description{Get Feed Meta Data from Google Blog Search \url{http://www.google.com/blogsearch}}
\author{Mario Annau}
\value{WebXMLSource}
\seealso{\code{\link{WebSource}}, \code{\link{readGoogle}}}
\arguments{\item{query}{Google Blog Search query}
\item{params,}{additional query parameters}
\item{...}{additional parameters to \code{\link{WebSource}}}
}
\examples{\dontrun{
corpus <- Corpus(GoogleBlogSearchSource("Microsoft"))
}}

