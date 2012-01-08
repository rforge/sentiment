\name{WebCorpus}
\alias{WebCorpus}
\title{Customized WebCorpus function
Constructs a derived class of \code{\link{Corpus}}}
\usage{
  WebCorpus(x,
    readerControl = list(reader = x$DefaultReader, language = "en"),
    postFUN = x$PostFUN, retryEmpty = T, ...)
}
\arguments{
  \item{x}{object of type Source, see also
  \code{\link{Corpus}}}

  \item{readerControl}{specifies reader to be used for
  \code{Source}, defaults to list(reader = x$DefaultReader,
  language = "en"}

  \item{postFUN}{function to be applied to WebCorpus after
  web retrieval has been completed, defaults to x$PostFUN}

  \item{retryEmpty}{specifies if retrieval for empty
  content elements should be repeated, defaults to TRUE}

  \item{...}{additional parameters for Corpus function
  (actually Corpus reader)}
}
\description{
  Customized WebCorpus function Constructs a derived class
  of \code{\link{Corpus}}
}

