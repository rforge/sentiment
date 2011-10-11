
#'Generic extraction function to call boilerpipe extractors
#' @param exname character specifying the extractor to be used. 
#' It can take one of the following values:
#' \describe{
#' \item{ArticleExtractor}{A full-text extractor which is tuned towards news articles.}
#' \item{ArticleSentencesExtractor}{A full-text extractor which is tuned towards extracting sentences from news articles.}
#' \item{CanolaExtractor}{A full-text extractor trained on a \href{http://krdwrd.org/}{krdwrd}.}
#' \item{DefaultExtractor}{A quite generic full-text extractor.}
#' \item{KeepEverythingExtractor}{Marks everything as content.}
#' \item{KeepEverythingWighMinKWordsExtractor}{A full-text extractor which extracts the largest text component of a page.}
#' \item{LargestContentExtractor}{A full-text extractor which extracts the largest text component of a page.}
#' \item{NumWordsRulesExtractor}{A quite generic full-text extractor solely based upon the number of words per block.}
#' }
#' @param content Text content or URL as character
#' @param asText should content specifed be treated as actual text to be extracted or url (from which HTML document is first downloaded and extracted afterwards), defaults to TRUE
#' @param ... additional parameters
#' @references \url{http://code.google.com/p/boilerpipe/}
#' @export 
Extractor <- function(exname, content, asText = TRUE, ...){
	
	excontent <- character(0)
	if(asText){
		excontent <- .jnew("java/lang/String", content)
	}else{ #assume that content is url
		excontent <- .jnew("java/net/URL", content)
	}
	
	expath <- paste("de/l3s/boilerpipe/extractors", exname, sep = "/")
	
	
	ex <- .jnew(expath)
	.jcall(ex, returnSig = "S", method = "getText", excontent, ...)
}

#' A full-text extractor which is tuned towards news articles. In this scenario
#' it achieves higher accuracy than \code{\link{DefaultExtractor}}.
#' @param content Text content as character
#' @param ... additional parameters
#' @export 
ArticleExtractor <- function(content, ...){
	Extractor("ArticleExtractor", content, ...)
}

#' A full-text extractor which is tuned towards extracting sentences from news articles.
#' @param content Text content as character
#' @param ... additional parameters
#' @export 
ArticleSentencesExtractor <- function(content, ...){
	Extractor("ArticleSentencesExtractor", content, ...)
}

#' A full-text extractor trained on a
#' \href{http://krdwrd.org/}{krdwrd} \href{https://krdwrd.org/trac/attachment/wiki/Corpora/Canola/CANOLA.pdf}{Canola}.
#' @param content Text content as character
#' @param ... additional parameters
#' @export 
CanolaExtractor <- function(content, ...){
	Extractor("CanolaExtractor", content, ...)
}

#'A quite generic full-text extractor.
#' @param content Text content as character
#' @param ... additional parameters
#' @export 
DefaultExtractor <- function(content, ...){
	Extractor("DefaultExtractor", content, ...)
}

#'Marks everything as content.
#' @param content Text content as character
#' @param ... additional parameters
#' @export 
KeepEverythingExtractor <- function(content, ...){
	Extractor("KeepEverythingExtractor", content, ...)
}

#' A full-text extractor which extracts the largest text component of a page.
#' For news articles, it may perform better than the \code{\link{DefaultExtractor}},
#' but usually worse than \code{\link{ArticleExtractor}}.
#' @param content Text content as character
#' @param kMin minimum number of words to be included in the text fragment
#' @param ... additional parameters
#' @export 
KeepEverythingWighMinKWordsExtractor <- function(content, kMin = 20, ...){
	Extractor("KeepEverythingWighMinKWordsExtractor", content, ...)
}

#' A full-text extractor which extracts the largest text component of a page.
#' For news articles, it may perform better than the \code{\link{DefaultExtractor}},
#' but usually worse than \code{\link{ArticleExtractor}}.
#' @param content Text content as character
#' @param ... additional parameters
#' @export 
LargestContentExtractor <- function(content, ...){
	Extractor("LargestContentExtractor", content, ...)
}

#' A quite generic full-text extractor solely based upon the number of words per
#' block (the current, the previous and the next block).
#' @param content Text content as character
#' @param ... additional parameters
#' @export 
NumWordsRulesExtractor <- function(content, ...){
	Extractor("NumWordsRulesExtractor", content, ...)
}