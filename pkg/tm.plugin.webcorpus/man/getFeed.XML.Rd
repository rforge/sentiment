\name{getFeed.XML}
\alias{getFeed.XML}
\title{Get (Meta-)Content list from XML...}
\usage{getFeed.XML(feed, fieldnames, parent, ...)
}
\description{Get (Meta-)Content list from XML
After feed is retrieved Conversion of Date and HTML Content fields is done}
\author{Mario Annau}
\seealso{\code{\link{getFeed}}}
\arguments{\item{feed}{character of feed to retrieve, feed can either be a URL or file (see \code{\link{xmlTreeParse}} for more info)}
\item{fieldnames}{list of fieldnames to extract from feed}
\item{parent}{character which specifies xpath expression of `parent' node.}
\item{...}{additional parameters to xml value extraction FUNction}
}

