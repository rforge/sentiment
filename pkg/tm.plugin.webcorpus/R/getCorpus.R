#' Get Corpus from a web source.
#' @author Mario Annau
#' @param query user defined (search) query. If query is given as a character vector it will be collapsed with + as separator
#' @param src source from which (Meta-) Data should be retrieved from. Internally the function getMeta.#src# will be called.
#' Already implemented sources are:
#' \itemize{
#' 	\item{\code{yahooFinance}} Yahoo Finance, see \code{\link{getMeta.yahooFinance}}
#'  \item{\code{googleFinance}} Yahoo Finance, see \code{\link{getMeta.googleFinance}}
#' 	\item{\code{googleBlogSearch}} Google Blog Search, see \code{\link{getMeta.googleBlogSearch}}
#' 	\item{\code{bing}} Bing News Search, see \code{\link{getMeta.bing}} (api-key required)
#' 	\item{\code{yahooBOSS}} Yahoo BOSS Search, see \code{\link{getMeta.yahooBOSS}} (api-key required)
#'  \item{\code{spotlight}} Reuters Spotlight, see \code{\link{getMeta.spotlight}} (api-key required)
#'  \item{\code{nytimes.articlesearch}} NY Times Article Search, see \code{\link{getMeta.nytimes.articlesearch}} (api-key required)
#'  \item{\code{twitter}} Twitter Search, see \code{\link{getMeta.twitter}}
#' }
#' @param fastmode Instead of downloading data (defined by "Origin" in meta list) "Description" or "Heading" field is used as Content, defaults to FALSE
#' @param extractor Extractor function to be used for downloaded data, defaults to \code{extractContentDOM}
#' @param extractorOpts Specify further parameters for the extraction function as list, defaults to list()
#' @param verbose print status output to console, defaults to FALSE
#' @param getURLPartOpts set default parameters for url download function \code{\link{getURLPart}} as \code{list}
#' @param useSource Integrate Source in Corpus in the field \code{sourcefield}, defaults to FALSE
#' @param sourceField If useSource is TRUE, websource will be integrated in defined meta data field 
#' @param writeCorpus, specifies if the corpus and its sources should be written to disk
#' Please note, that this feature is still in an experimental stage
#' @param sourcePath character specifying full path where corpus should be written, will be created if not existent, defaults to \code{getwd()}
#' @param ... additional parameters for respective getMeta.#src# function
#' @return corpus
#' @seealso \code{\link{getURLPart}}
#' @examples
#' \dontrun{
#' corpus <- getCorpus("MSFT", src = "yahooFinance", n = 20)
#' }
#' @export
`getCorpus` <- function(
				query, 
				src = "yahooFinance", 
				fastmode = FALSE, 
				extractor = "extractContentDOM", 
				extractorOpts = list(), 
				verbose = FALSE, 
				getURLPartOpts, 
				useSource = FALSE, 
				sourceField = "Source", 
				writeCorpus = FALSE,
				sourcePath = getwd(),
				...){

	query <- paste(query, collapse = "+")
	
	corplst <- do.call(paste("getMeta.", src, sep = ""), list(query, verbose=verbose,...))

	if(fastmode){
		if("Description" %in% names(corplst)){
			corplst[["Content"]] <- corplst[["Description"]]
		}else if("Heading" %in% names(corplst)){
			corplst[["Content"]] <- corplst[["Heading"]]
		}else{
			stop("Cannot apply fastmode: Neither Description nor Heading field name found")
		}
	}
	
	if(!("Content" %in% names(corplst))){
		if(missing(getURLPartOpts)){
			getURLPartOpts <- list(
				maxChunkSize = 20,
				verbose = verbose, 
				followlocation = TRUE, 
				maxredirs = as.integer(20), 
				timeout.ms = as.integer(30000), 
				connecttimeout.ms = as.integer(30000)
			)
		}
		getURLPartOpts[["url"]] <- corplst[["Origin"]]
		
		#contentsrc <- getURLPart(corplst[["Origin"]], maxChunkSize = maxChunkSize, verbose = verbose, followlocation = TRUE, maxredirs = as.integer(20), timeout.ms = as.integer(30000), connecttimeout.ms = as.integer(30000))
		contentsrc <- do.call("getURLPart", getURLPartOpts)
		
		allExtractorOpts <- c(list(contentsrc = contentsrc, extractor = extractor, verbose = verbose),
				extractorOpts)
		
		corplst[["Content"]] <- do.call("extractContent", allExtractorOpts)
		#corplst[["Content"]] <- extractContent(contentsrc, extractor = extractor, verbose = verbose)
		#corplst[["Content"]] <- getContent(corplst[["Origin"]], extractor = extractFUN, verbose = verbose)
		if(useSource){
			corplst[[sourceField]] <- contentsrc
		}
		
		
	}
	if(sum(sapply(corplst, length)) <= 0){
		stop("No Results found for entered Query")
	}#else
	
	if(writeCorpus){
		writeWebCorpus(corplst, contentsrc, sourceField, sourcePath, extractdir = extractor)
	}
	
	Corpus(ListSource(corplst), readerControl = list(reader = readList))
}





