\name{KeepEverythingWighMinKWordsExtractor}
\alias{KeepEverythingWighMinKWordsExtractor}
\title{A full-text extractor which extracts the largest text component of a page.
For news articles, it may perform better than the \code{\link{DefaultExtractor}},
but usually worse than \code{\link{ArticleExtractor}}.}
\usage{
  KeepEverythingWighMinKWordsExtractor(content, kMin = 20,
    ...)
}
\arguments{
  \item{content}{Text content as character}

  \item{kMin}{minimum number of words to be included in the
  text fragment}

  \item{...}{additional parameters}
}
\description{
  A full-text extractor which extracts the largest text
  component of a page. For news articles, it may perform
  better than the \code{\link{DefaultExtractor}}, but
  usually worse than \code{\link{ArticleExtractor}}.
}

