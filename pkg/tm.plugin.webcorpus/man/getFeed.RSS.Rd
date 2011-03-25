\name{getFeed.RSS}
\alias{getFeed.RSS}
\title{Get (Meta-)Content list from RSS Feed.}
\usage{getFeed.RSS(feed, fieldnames, ...)
}
\description{Get (Meta-)Content list from RSS Feed.
A Simple wrapper for \code{\link{getFeed.XML}}}
\author{Mario Annau}
\seealso{\code{\link{getFeed.XML}} \code{\link{getFeed}}}
\arguments{\item{feed}{character of feed to retrieve, feed can either be a URL or file (see \code{\link{xmlTreeParse}} for more info)}
\item{fieldnames}{list of fieldnames to extract from feed}
\item{...}{additional parameters for \code{\link{getFeed.XML}} function}
}

