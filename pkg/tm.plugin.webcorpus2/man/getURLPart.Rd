\name{getURLPart}
\alias{getURLPart}
\title{Retrieve the content of multiple URLs asynchronously in Chunks.}
\usage{getURLPart(url, maxChunkSize=10, verbose=TRUE, ...)
}
\description{Retrieve the content of multiple URLs asynchronously in Chunks.
Function makes use of RCurls excellent getURL function to retrieve 
content.}
\author{Mario Annau}
\value{list of html content from urls}
\arguments{\item{url}{character vector of urls which should be retrieved}
\item{maxChunkSize}{integer which specifies chunk size of links which
should be retrieved at once asynchronously}
\item{verbose}{print status information}
\item{...}{Additional parameters to \code{\link{getURL}}}
}

