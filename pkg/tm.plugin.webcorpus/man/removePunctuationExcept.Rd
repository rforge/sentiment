\name{removePunctuationExcept}
\alias{removePunctuationExcept}
\title{Remove punctuations except specified one.}
\usage{removePunctuationExcept(td, except="/")
}
\description{Remove punctuations except specified one. 
Helper function for preprocessing of POS tagged items}
\author{Mario Annau}
\seealso{\code{\link{removePunctuation}} \code{\link{tm_map}}}
\note{Perl syntax is used.}
\arguments{\item{td}{TextDocument}
\item{except}{punctuation patterns which should not be removed}
}

