\name{extractHTMLStrip}
\alias{extractHTMLStrip}
\title{Simply strip HTML Tags from Document}
\usage{
  extractHTMLStrip(url, asText = TRUE, encoding, ...)
}
\arguments{
  \item{url}{character, url or filename}

  \item{asText}{specifies if url parameter is a
  \code{character}, defaults to TRUE}

  \item{encoding}{specifies local encoding to be used,
  depending on platform}

  \item{...}{Additional parameters for
  \code{\link{htmlTreeParse}}}
}
\description{
  Simply strip HTML Tags from Document
}
\author{
  Mario Annau
}
\seealso{
  \code{\link{xmlNode}}
}

