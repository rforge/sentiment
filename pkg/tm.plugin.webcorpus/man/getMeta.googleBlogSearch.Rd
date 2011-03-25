\name{getMeta.googleBlogSearch}
\alias{getMeta.googleBlogSearch}
\title{Get Feed Meta Data from Google BlogSearch...}
\usage{getMeta.googleBlogSearch(query, n=100, verbose=TRUE, ...)
}
\description{Get Feed Meta Data from Google BlogSearch}
\author{Mario Annau}
\value{list}
\seealso{\code{\link{getFeed}} \code{\link{getCorpus}}}
\arguments{\item{query}{character vector of keywords to be searched for}
\item{n}{number of results (curr. max is ?), defaults to 100}
\item{verbose}{print status output to console, defaults to TRUE}
\item{...}{additional query parameters}
}
\examples{\dontrun{
query <- c("MSFT","Microsoft")
metalist <- getMeta.googleBlogSearch(query, n = 20)
corpus <- getCorpus(query, src = "googleBlogSearch", n = 20)
}}

