#' Buildup string for feedquery. Function has partly been taken from getForm function in
#' \code{XML} package (Duncan Temple Lang)
#' @author Mario Annau
#' @param url character
#' @param params list which contains feed parameters
#' @seealso \code{\link{xmlNode}}
#' @export 
#' @importFrom RCurl curlEscape
feedquery <-
function(url, params){
	els <- lapply(names(params), function(n) {		
		paste(n, curlEscape(params[[n]]), sep = "=")
	})
	names(els) <- names(params)
	
	feeds <- ""
	for(i in names(els)){
		if(feeds[1] == ""){
			sep = ""
		}
		else{
			sep = "&"
		}
		feeds <- paste(feeds, els[[i]], sep = sep)
	}

	feeds <- paste(url, feeds, sep = "?")
	return(feeds)
}