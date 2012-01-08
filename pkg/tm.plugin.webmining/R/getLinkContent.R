# TODO: Add comment
# 
# Author: mario
###############################################################################

#getURLTimelimit <- function(urls, timeout, curlOpts = 
#				curlOptions(followlocation = TRUE, 
#				maxconnects = 20,
#				maxredirs = 10,
#				timeout = timeout,
#				connecttimeout = timeout),...){
#	
#	setTimeLimit(cpu=timeout, elapsed=timeout, transient=TRUE);
#	getURL(urls, timelimit, curlOpts = curlOpts, ...)
#}

#evalWithTimeout <- 
#function(..., envir=parent.frame(), timeout, cpu=timeout, elapsed=timeout, onTimeout=c("error", "warning", "silent")) {
#	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#	# Validate arguments
#	# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#	# Argument 'cpu' and 'elapsed':
#	cpu <- Arguments$getNumeric(cpu, range=c(0,Inf));
#	elapsed <- Arguments$getNumeric(elapsed, range=c(0,Inf));
#	
#	# Argument 'onTimeout':
#	onTimeout <- match.arg(onTimeout);
#	
#	
#	# Default result value
#	res <- invisible();
#	
#	setTimeLimit(cpu=cpu, elapsed=elapsed, transient=TRUE);
#	tryCatch({
#				res <- eval(..., envir=envir);
#			}, error = function(ex) {
#				msg <- ex$message;
#				# Was it a timeout?
#				pattern <- gettext("reached elapsed time limit");
#				if (regexpr(pattern, msg) != -1) {
#					ex <- TimeoutException(msg, cpu=cpu, elapsed=elapsed);
#					if (onTimeout == "error") {
#						throw(ex);
#					} else if (onTimeout == "warning") {
#						warning(getMessage(ex));
#					} else if (onTimeout == "silent") {
#					}
#				} else {
#					# Rethrow error
#					throw(ex);
#				}
#			})
#	
#	res;
#}

#' Get Corpus link content
#' @param corpus corpus for which link content should be downloaded
#' @param links character vector specifyinig links to be used for download, defaults to 
#' sapply(corpus, meta, "Origin")
#' @param timeout.request timeout (in seconds) to be used for connections/requests, defaults to 30
#' @param curlOpts curl options to be passed to \code{\link{getURL}}
#' @param ... additional parameters to \code{\link{getURL}}
#' @export
getLinkContent <- function(corpus, links = sapply(corpus, meta, "Origin"),timeout.request = 30,
		curlOpts = curlOptions(	followlocation = TRUE, 
				maxconnects = 20,
				maxredirs = 10,
				timeout = timeout.request,
				connecttimeout = timeout.request,
				ssl.verifypeer = FALSE,
				useragent = "R"),...){
	
	if(length(corpus) != length(links))
		stop("Corpus length not equal to links length\n")

	#content_urls <- unlist(sapply(content_parsed, linkreader))
	content <- tryCatch({
				#cat("Get ", length(links), "Links...")
				#timeout = timeout.request * length(links)
				#setTimeLimit(cpu=timeout, elapsed=timeout, transient=TRUE)
				getURL(links, .opts = curlOpts, ...)
				},
				error=function(e){
					print(e)
					cat("\nError on retrieval, single retrieval fallback... \n")
					content <- list()
					for(i in 1:length(links)){
						content[[i]] <- tryCatch({
								#timeout = timeout.request * length(links)
								#setTimeLimit(cpu=timeout, elapsed=timeout, transient=TRUE)
								getURL(links, .opts = curlOpts, ...)
							},error = function(f) {
												  cat(f,"\n")
												  ""})
					}
					#cat("Done\n")
					do.call(c, content)})
	#assign web content to 
	for(i in 1:length(corpus)){
		Content(corpus[[i]]) <- content[i]
	}
	
	corpus
}

#Define as Method for Corpus
## retrieve Content from empty corpus items
## @export
#retrieveEmpty <- function(corpus, ...){
#	noContent <- sapply(corpus, nchar) == 0
#	corp_nocontent <- getLinkContent(corpus[noContent], ...)
#	c(corpus[!noContent],corp_nocontent)
#}
