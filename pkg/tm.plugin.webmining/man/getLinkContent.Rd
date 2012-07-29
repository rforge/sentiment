\name{getLinkContent}
\alias{getLinkContent}
\title{Get Corpus link content}
\usage{
  getLinkContent(corpus,
    links = sapply(corpus, meta, "Origin"),
    timeout.request = 30, chunksize = 20,
    verbose = getOption("verbose"),
    curlOpts = curlOptions(verbose = FALSE, followlocation = TRUE, maxconnects = 5, maxredirs = 10, timeout = timeout.request, connecttimeout = timeout.request, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE, useragent = "R"),
    retry.empty = 3, sleep.time = 3,
    extractor = extractContentDOM, ...)
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

  \item{chunksize}{Size of download chunks to be used for
  parallel retrieval, defaults to 20}

  \item{verbose}{Specifies if retrieval info should be
  printed, defaults to getOption("verbose")}

  \item{retry.empty}{Specifies number of times empty
  content sites should be retried, defaults to 3}

  \item{sleep.time}{Sleep time to be used between chunked
  download, defaults to 3 (seconds)}

  \item{extractor}{Extractor to be used for content
  extraction, defaults to extractContentDOM}

  \item{...}{additional parameters to \code{\link{getURL}}}
}
\value{
  corpus including downloaded link content
}
\description{
  Get Corpus link content
}

