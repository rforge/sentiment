\name{readWeb}
\alias{readWeb}
\title{Read content from Web...Source
Generic function to read content from \code{\link{WebSource}}}
\usage{
  readWeb(spec, doc, parser, contentparser, freeFUN = NULL)
}
\arguments{
  \item{spec}{specification of content reader}

  \item{doc}{document to be parsed}

  \item{parser}{parser function to be used}

  \item{contentparser}{content parser function to be used,
  see also \code{tm:::xml_content} or
  \code{\link{json_content}}}

  \item{freeFUN}{function to free memory from parsed object
  (actually only relevant for XML and HTML trees)}
}
\value{
  FunctionGenerator
}
\description{
  Read content from Web...Source Generic function to read
  content from \code{\link{WebSource}}
}

