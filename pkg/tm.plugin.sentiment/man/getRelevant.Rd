\name{getRelevant}
\alias{getRelevant}
\title{Get Relevant Part from Text Document.
Search for the first and last occurence of items in Text Document,
get sentence boundaries and extract text.
Function can be used in tm_map wrapper}
\usage{
  getRelevant(td, items, boundaries, matches.only = FALSE,
    fieldname = "matches")
}
\arguments{
  \item{td}{TextDocument}

  \item{items}{character vector of items to be searched,
  eg. c("Microsoft", "MSFT")}

  \item{boundaries}{defined sentence boundaries in
  perl-regex syntax}

  \item{matches.only}{Return number of matches only}

  \item{matches}{only should only matches be annotated,
  defaults to FALSE}

  \item{fieldname}{name which should be used for meta field
  in Text Document to be annotated}
}
\description{
  Get Relevant Part from Text Document. Search for the
  first and last occurence of items in Text Document, get
  sentence boundaries and extract text. Function can be
  used in tm_map wrapper
}
\author{
  Mario Annau
}
\seealso{
  \code{\link{tm_map}}
}

