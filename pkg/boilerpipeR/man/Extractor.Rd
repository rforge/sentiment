\name{Extractor}
\alias{Extractor}
\title{Generic extraction function to call boilerpipe extractors}
\usage{
  Extractor(exname, content, asText = TRUE, ...)
}
\arguments{
  \item{exname}{character specifying the extractor to be
  used. It can take one of the following values: \describe{
  \item{ArticleExtractor}{A full-text extractor which is
  tuned towards news articles.}
  \item{ArticleSentencesExtractor}{A full-text extractor
  which is tuned towards extracting sentences from news
  articles.} \item{CanolaExtractor}{A full-text extractor
  trained on a \href{http://krdwrd.org/}{krdwrd}.}
  \item{DefaultExtractor}{A quite generic full-text
  extractor.} \item{KeepEverythingExtractor}{Marks
  everything as content.}
  \item{KeepEverythingWighMinKWordsExtractor}{A full-text
  extractor which extracts the largest text component of a
  page.} \item{LargestContentExtractor}{A full-text
  extractor which extracts the largest text component of a
  page.} \item{NumWordsRulesExtractor}{A quite generic
  full-text extractor solely based upon the number of words
  per block.} }}

  \item{content}{Text content or URL as character}

  \item{asText}{should content specifed be treated as
  actual text to be extracted or url (from which HTML
  document is first downloaded and extracted afterwards),
  defaults to TRUE}

  \item{...}{additional parameters}
}
\description{
  Generic extraction function to call boilerpipe extractors
}
\references{
  \url{http://code.google.com/p/boilerpipe/}
}

