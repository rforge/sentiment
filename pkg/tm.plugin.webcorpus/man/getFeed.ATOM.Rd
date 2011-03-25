\name{getFeed.ATOM}
\alias{getFeed.ATOM}
\title{Get (Meta-)Content list from ATOM Feed.}
\usage{getFeed.ATOM(feed, fieldnames, parent, ...)
}
\description{Get (Meta-)Content list from ATOM Feed.
A Simple wrapper for \code{\link{getFeed.XML}}}
\author{Mario Annau}
\seealso{\code{\link{getFeed.XML}} \code{\link{getFeed}}}
\arguments{\item{feed}{character of feed to retrieve, feed can either be a URL or file (see \code{\link{xmlTreeParse}} for more info)}
\item{fieldnames}{list of fieldnames to extract from feed}
\item{parent}{character which specifies xpath expression of `parent' node.}
\item{...}{additional parameters for getFeed.XML}
}

