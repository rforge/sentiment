\name{getFeed.JSON}
\alias{getFeed.JSON}
\title{Get (Meta-)Content list from JSON Feed.}
\usage{getFeed.JSON(feed, fieldnames, parent, textPreprocess=TRUE)
}
\description{Get (Meta-)Content list from JSON Feed.}
\author{Mario Annau}
\seealso{\code{\link{getFeed}}}
\arguments{\item{feed}{character of feed to retrieve, feed can either be a URL or file (see \code{\link{xmlTreeParse}} for more info)}
\item{fieldnames}{list of fieldnames to extract from feed}
\item{textPreprocess}{preprocess text for RJSONIO parser - is at least necessary for reutersSpotligth content}
\item{parent}{Specifies parent node in JSON structure}
}

