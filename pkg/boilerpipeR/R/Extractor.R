
#'Extractor
#'@export 
Extractor <- function(exname, content, asText = TRUE){
	
	excontent <- character(0)
	if(asText){
		excontent <- .jnew("java/lang/String", content)
	}else{ #assume that content is url
		excontent <- .jnew("java/net/URL", content)
	}
	
	expath <- paste("de/l3s/boilerpipe/extractors", exname, sep = "/")
	
	
	ex <- .jnew(expath)
	.jcall(ex, returnSig = "S", method = "getText", excontent)
}

#'ArticleExtractor
#'@export 
ArticleExtractor <- function(content, ...){
	Extractor("ArticleExtractor", content, ...)
}

#'ArticleSentencesExtractor
#'@export 
ArticleSentencesExtractor <- function(content, ...){
	Extractor("ArticleSentencesExtractor", content, ...)
}

#'CanolaExtractor
#'@export 
CanolaExtractor <- function(content, ...){
	Extractor("CanolaExtractor", content, ...)
}

#'DefaultExtractor
#'@export 
DefaultExtractor <- function(content, ...){
	Extractor("DefaultExtractor", content, ...)
}

#'KeepEverythingExtractor
#'@export 
KeepEverythingExtractor <- function(content, ...){
	Extractor("KeepEverythingExtractor", content, ...)
}

#'KeepEverythingWighMinKWordsExtractor
#'@export 
KeepEverythingWighMinKWordsExtractor <- function(content, ...){
	Extractor("KeepEverythingWighMinKWordsExtractor", content, ...)
}

#'LargestContentExtractor
#'@export
LargestContentExtractor <- function(content, ...){
	Extractor("LargestContentExtractor", content, ...)
}

#'NumWordsRulesExtractor
#'@export
NumWordsRulesExtractor <- function(content, ...){
	Extractor("NumWordsRulesExtractor", content, ...)
}