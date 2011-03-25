#' Retrieve the content of multiple URLs asynchronously in Chunks.
#' Function makes use of RCurls excellent getURL function to retrieve 
#' content.
#' @author Mario Annau
#' @param url character vector of urls which should be retrieved
#' @param maxChunkSize integer which specifies chunk size of links which
#' should be retrieved at once asynchronously
#' @param verbose print status information
#' @param ... Additional parameters to \code{\link{getURL}}
#' @seealso \code{\link{getCorpus}}
#' @return list of html content from urls
#' @export
#' @importFrom RCurl getURL

# TODO implement retries 
getURLPart <- function(url, maxChunkSize = 10, verbose = TRUE, ...){
	
	source <- c()
	start <- seq(1, to = length(url), by = maxChunkSize)

	if(verbose)
		cat("Starting URL Retrieval...\n")
	
	for(i in start){
		end <- min(i + maxChunkSize -1,length(url))
		urlpart <- url[i:end]
	
		response <- tryCatch({getURL(urlpart, ...)}
				, error = function(e){
					response <- c()
					for(j in 1:length(urlpart)){
						u <- urlpart[j]
						src <- tryCatch(getURL(u, ...),
								error = function(err){	if(verbose){
															print(paste("Error at URL:",u))
													  	}
														return("")
													 })
						response <- c(response, src)
						return(response)
					}
					return(response)
				})
		source <- c(source,response)
		
		if(verbose){
			progress <- floor(end/length(url)*100)
			cat(progress, "%\r")
		}
	}
	if(verbose)
		cat("Done !\n")
	
	return(source)
}
