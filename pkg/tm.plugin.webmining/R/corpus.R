# TODO: Add comment
# 
# Author: mario
###############################################################################

#' Customized WebCorpus function
#' Constructs a derived class of \code{\link{Corpus}} 
#' @param x object of type Source, see also \code{\link{Corpus}}
#' @param readerControl specifies reader to be used for \code{Source}, defaults to
#' list(reader = x$DefaultReader, language = "en"
#' @param postFUN function to be applied to WebCorpus after web retrieval has been completed,
#' defaults to x$PostFUN
#' @param retryEmpty specifies if retrieval for empty content elements should be repeated, 
#' defaults to TRUE
#' @param ... additional parameters for Corpus function (actually Corpus reader)
#' @export
WebCorpus <- function(x, readerControl = list(reader = x$DefaultReader, language = "en"), 
		postFUN = x$PostFUN, retryEmpty = T, ...){
	corpus <- Corpus(x, readerControl, ...)
	if(!is.null(postFUN)){
		corpus <- postFUN(corpus)
	}
	
	cm <- CMetaData(corpus)
	
	cm$MetaData$Source <- x
	cm$MetaData$ReaderControl <- readerControl
	cm$MetaData$PostFUN <- postFUN
	
	attr(corpus, "CMetaData") <- cm
	class(corpus) <- c("WebCorpus", class(corpus))
	if(retryEmpty){
		corpus <- getEmpty(corpus)
	}
	corpus
	
}

#'@S3method [ WebCorpus
#' @noRd
`[.WebCorpus` <- function(x, i) {
	if (missing(i)) return(x)
	corpus <- tm:::.VCorpus(NextMethod("["), CMetaData(x), DMetaData(x)[i, , drop = FALSE])
	class(corpus) <- c("WebCorpus", class(corpus))
	corpus
}

#' Update Corpus
#' Generic function to update a text \code{\link{Corpus}}
#' @param x object of type \code{\link{WebCorpus}}
#' @param ... additional parameters to \code{\link{Corpus}} function
#' @export corpus.update
#' @aliases corpus.update.WebCorpus
corpus.update <- function(x, ...){
	UseMethod("corpus.update", x)	
}

#' Update WebCorpus
#' @S3method corpus.update WebCorpus
#' @param x \code{\link{WebCorpus}}
#' @param fieldname name of \code{\link{Corpus}} field name to be used as ID, defaults to "ID"
#' @param retryempty specifies if empty corpus elements should be downloaded again, defaults to TRUE
#' @param ... additional parameters to \code{\link{Corpus}} function
#' @noRd
corpus.update.WebCorpus <- 
function(x, fieldname = "ID", retryempty = T, verbose = F, ...) {
	cm <- CMetaData(x)
	
	newsource <- source.update(cm$MetaData$Source)
	
	newcorpus <- Corpus(newsource, readerControl = cm$MetaData$ReaderControl, postFUN = NULL, ...)
	#intersect on ID
	id_old <- sapply(x, meta, fieldname)
	if(any(sapply(id_old, length) == 0))
		stop(paste("Not all elements in corpus to update have field '", fieldname, "' defined", sep = ""))

	id_new <- sapply(newcorpus, meta, fieldname)
	if(any(sapply(id_new, length) == 0))
		stop(paste("Not all elements in corpus to update have field '", fieldname, "' defined", sep = ""))
	
	newcorpus <- newcorpus[!id_new %in% id_old]
	
	if(length(newcorpus) > 0){
		if(!is.null(cm$MetaData$PostFUN)){
			newcorpus <- cm$MetaData$PostFUN(newcorpus)
		}
		corpus <- c(x, newcorpus)
		attr(corpus, "CMetaData") <- CMetaData(x)
		class(corpus) <- c("WebCorpus", class(corpus))
	}else{
		corpus <- x
	}
	
	if(retryempty){
		corpus <- getEmpty(corpus)
	}
	
	if(verbose){
		cat(length(newcorpus), " corpus items added.\n")
	}
		
	corpus
}


#' Get Empty Corpus Elements
#' Retrieve content of all empty Corpus elements
#' @param x object of type \code{\link{WebCorpus}}
#' @param ... additional parameters to PostFUN
#' @export getEmpty
#' @aliases getEmpty.WebCorpus
getEmpty <- function(x, ...){
	UseMethod("getEmpty", x)	
}
	

#' @S3method getEmpty WebCorpus
#' @noRd
getEmpty.WebCorpus <- 
function(x, nChar = 0, ...){
	cm <- CMetaData(x)
	noContent <- which(sapply(x, nchar) <= nChar)
	if(length(noContent) > 0){
		corp_nocontent <- x[noContent]
		if(!is.null(cm$MetaData$PostFUN)){
			corp_nocontent <- cm$MetaData$PostFUN(corp_nocontent, ...)
		}
		x[noContent] <- corp_nocontent
	}
	x
}


