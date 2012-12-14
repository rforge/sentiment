\name{removeNonASCII}
\alias{removeNonASCII}
\alias{removeNonASCII.PlainTextDocument}
\title{Remove non-ASCII characters from Text}
\usage{
  removeNonASCII(x,
    fields = c("Content", "Heading", "Description"),
    from = "UTF-8", to = "ASCII//TRANSLIT")
}
\arguments{
  \item{x}{object of PlainTextDocument class}

  \item{fields}{specifies fields to be converted, defaults
  to fields = c("Content", "Heading", "Description")}

  \item{from}{specifies encoding from which conversion
  should be done, defaults to "UTF-8"}

  \item{to}{speciefies target encoding, defaults to
  "ASCII//TRANSLIT"}
}
\description{
  Remove non-ASCII characters from Text
}

