\name{getFeed.CSV}
\alias{getFeed.CSV}
\title{Get (Meta-)Content list from CSV...}
\usage{getFeed.CSV(feed, fieldnames, stringsAsFactors=FALSE, ...)
}
\description{Get (Meta-)Content list from CSV
After feed is retrieved Conversion of Date and HTML Content fields is done}
\author{Mario Annau}
\seealso{\code{\link{getFeed}}}
\arguments{\item{feed}{character of feed to retrieve, feed should be a csv file name)}
\item{fieldnames}{list of fieldnames to extract from feed}
\item{stringsAsFactors}{strings in csv are treated as factors, defaults to FALSE, see \code{\link{read.csv}}}
\item{...}{addtional parameters to \code{\link{read.csv}}}
}

