\name{extractContentDOM}
\alias{extractContentDOM}
\title{Extract Main HTML Content from DOM
Function extracts main HTML Content using its Document Object Model.
Idea comes basically from the fact, that main content of an HTML Document
is in a subnode of the HTML DOM Tree with a high text to tag - ratio.}
\usage{
  extractContentDOM(url, threshold = 0.5, asText = TRUE,
    ...)
}
\arguments{
  \item{url}{character, url or filename}

  \item{threshold}{threshold for extraction, defaults to
  0.5}

  \item{asText}{boolean, specifies if url should be
  interpreted as character}

  \item{...}{Additional Parameters to
  \code{\link{htmlTreeParse}}}
}
\description{
  Extract Main HTML Content from DOM Function extracts main
  HTML Content using its Document Object Model. Idea comes
  basically from the fact, that main content of an HTML
  Document is in a subnode of the HTML DOM Tree with a high
  text to tag - ratio.
}
\author{
  Mario Annau
}
\references{
  \url{http://www.elias.cn/En/ExtMainText},
  \url{http://ai-depot.com/articles/the-easy-way-to-extract-useful-text-from-arbitrary-html/}
  \cite{Gupta et al., DOM-based Content Extraction of HTML
  Documents},\url{http://www2003.org/cdrom/papers/refereed/p583/p583-gupta.html}
}
\seealso{
  \code{\link{xmlNode}}
}

