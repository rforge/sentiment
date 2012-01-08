\name{getLinkContent}
\alias{getLinkContent}
\title{Get Corpus link content}
\usage{
  getLinkContent(corpus,
    links = sapply(corpus, meta, "Origin"),
    timeout.request = 30,
    curlOpts = curlOptions(followlocation = TRUE, maxconnects = 20, maxredirs = 10, timeout = timeout.request, connecttimeout = timeout.request, ssl.verifypeer = FALSE, useragent = "R"),
    ...)
}
\arguments{
  \item{corpus}{corpus for which link content should be
  downloaded}

  \item{links}{character vector specifyinig links to be
  used for download, defaults to sapply(corpus, meta,
  "Origin")}

  \item{timeout.request}{timeout (in seconds) to be used
  for connections/requests, defaults to 30}

  \item{curlOpts}{curl options to be passed to
  \code{\link{getURL}}}

  \item{...}{additional parameters to \code{\link{getURL}}}
}
\description{
  Get Corpus link content
}

