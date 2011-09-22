\name{extractHTMLStrip}
\alias{extractHTMLStrip}
\title{Simply strip HTML Tags from Document...}
\usage{extractHTMLStrip(url, asText=TRUE, ...)
}
\description{Simply strip HTML Tags from Document}
\author{Mario Annau}
\seealso{\code{\link{xmlNode}}}
\references{\url{http://www.elias.cn/En/ExtMainText}, 
\url{http://ai-depot.com/articles/the-easy-way-to-extract-useful-text-from-arbitrary-html/}
\cite{Gupta et al., DOM-based Content Extraction of HTML Documents},\url{http://www2003.org/cdrom/papers/refereed/p583/p583-gupta.html}}
\arguments{\item{url}{character, url or filename}
\item{asText}{specifies if url parameter is a \code{character}, defaults to TRUE}
\item{...}{Additional parameters for \code{\link{htmlTreeParse}}}
}

