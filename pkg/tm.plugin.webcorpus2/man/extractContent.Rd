\name{extractContent}
\alias{extractContent}
\title{Extract Content from HTML Documents...}
\usage{extractContent(contentsrc, extractor="extractContentDOM", verbose=TRUE, ...)
}
\description{Extract Content from HTML Documents}
\author{Mario Annau}
\seealso{\code{\link{extractContentDOM}} \code{\link{extractHTMLStrip}}}
\references{\url{http://www.elias.cn/En/ExtMainText}, 
\url{http://ai-depot.com/articles/the-easy-way-to-extract-useful-text-from-arbitrary-html/}
\cite{Gupta et al., DOM-based Content Extraction of HTML Documents},\url{http://www2003.org/cdrom/papers/refereed/p583/p583-gupta.html}}
\arguments{\item{contentsrc}{list containing source text content to be extracted}
\item{extractor}{Extractor function to be used, defaults to \code{\link{extractContentDOM}}}
\item{verbose}{Print messages from extraction process, defaults to TRUE}
\item{...}{additional parameters forwarded to extraction function}
}

