
#' Get Corpus link content
#' @param corpus corpus for which link content should be downloaded
#' @param links character vector specifyinig links to be used for download, defaults to 
#' sapply(corpus, meta, "Origin")
#' @param timeout.request timeout (in seconds) to be used for connections/requests, defaults to 30
#' @param curlOpts curl options to be passed to \code{\link{getURL}}
#' @param ... additional parameters to \code{\link{getURL}}
#' @export
getLinkContent <- function(corpus, links = meta(corpus, "Origin"),
		timeout.request = 30, chunksize = 20, verbose = getOption("verbose"),
		curlOpts = curlOptions(	verbose = FALSE,
				followlocation = TRUE, 
				maxconnects = 5,
				maxredirs = 10,
				timeout = timeout.request,
				connecttimeout = timeout.request,
				ssl.verifyhost=FALSE,
				ssl.verifypeer = FALSE,
				useragent = "R"),  
		#write.disk = F, 
		tdir = tempdir(), 
		retry.empty = 3, 
		delete.tempdir = F, 
		sleep.time = 3, 
		extractor = ArticleExtractor, ...){
	
	if(nrow(corpus) != length(links))
		stop("Corpus length not equal to links length\n")
	
	#content_urls <- unlist(sapply(content_parsed, linkreader))
	if(verbose){
		cat("Starting URL Download ...\n")
	}
	
	retries <- 0
	#allcontent <- rep("", length(links))
	
	while(any((tlength <- textlength(corpus))[,1] < 1) & (retries <= retry.empty)){
		# Avoid memory leakages through parallel processing
		p <- parallel({
			retries <- retries + 1
			emptycontent.ids <- as.numeric(row.names(tlength)[tlength[,1] < 1])
			
			if(verbose){
				cat("Run ", retries, ", retrieving ", length(emptycontent.ids), " content items\n")
			}
			
			#for(cstart in seq(from = 1, to =  length(links), by = chunksize)){
			for(cstart in seq(from = 1, to =  length(emptycontent.ids), by = chunksize)){
				if(sleep.time > 0){
					if(verbose){
						cat("Sleeping ", sleep.time, " seconds...\n")
					}
					Sys.sleep(sleep.time)
				}
				
				cend <- min(cstart[1] + chunksize-1, length(emptycontent.ids))
				chunk.ids <- emptycontent.ids[cstart:cend]
				chunk <- links[chunk.ids]
				
				# TODO Enable chunk download
				content <- tryCatch({
							#cat("Get ", length(links), "Links...")
							#timeout = timeout.request * length(links)
							#setTimeLimit(cpu=timeout, elapsed=timeout, transient=TRUE)
							getURL(chunk, .opts = curlOpts, ...)
						},
						error=function(e){
							print(e)
							# TODO: Check if really necessary
							cat("\nError on retrieval, single retrieval fallback... \n")
							content <- list()
							for(i in 1:length(chunk)){
								content[[i]] <- tryCatch({
											#timeout = timeout.request * length(links)
											#setTimeLimit(cpu=timeout, elapsed=timeout, transient=TRUE)
											getURL(chunk[i], .opts = curlOpts, ...)
										},error = function(f) {
											print(f)
											""})
							}
							#cat("Done\n")
							do.call(c, content)})
				
				
				# Extract Content
				extract <- sapply(content, extractor, ...)
				
				# Escape '
				#extract <- gsub("'", "", extract)	
				
				# Put Content Into Database
				Content(corpus[chunk.ids,]) <- extract
				
				
				#allcontent[chunk.ids] <- content
				
				if(verbose){
					progress <- floor(cend/length(links)*100)
					cat(paste(progress, "% (",cend,"/",length(emptycontent.ids), ") ", Sys.time(), "\n",sep = ""))
				}
				closeAllConnections()
				
			}
			TRUE
		})
		success <- collect(p)
	}
#	if(length(corpus) != length(allcontent)){
#		stop("Length mismatch: length(Corpus) != length(allcontent)")
#	}
	
	corpus
}