#' Get (Meta-)Content list from Feed .
#' After feed is retrieved Conversion of Date and HTML Content fields is done
#' @author Mario Annau
#' @aliases getFeed
#' @param feed character (vector) of feeds to retrieve, feed can either be a URL or file (see \code{\link{xmlTreeParse}} for more info)
#' @param params named list of feed parameters
#' @param fieldnames list of fieldnames to extract from feed
#' @param dateConvertFields fields to convert to POSIXt, defaults to "pubDate"
#' @param htmlConvertFields fields which contain html syntax and should be converted to text (with tags removed), 
#' defaults to "description"
#' @param timeFormat character defining time format by ISO C99 / POSIX standard for \code{\link{strptime}},
#' defaults to "\%a, \%d \%b \%Y \%H:\%M:\%S"
#' @param timeZone character defining time zone, defaults to GMT
#' @param type Specify feed type.
#' #' Already implemented types are:
#' \itemize{
#' 	\item{\code{XML}} Get (Meta-)Content list from XML, see \code{\link{getFeed.XML}}
#'  \item{\code{RSS}} Get (Meta-)Content list from RSS Feed, see \code{\link{getFeed.RSS}}
#' 	\item{\code{ATOM}} Get (Meta-)Content list from ATOM Feed, see \code{\link{getFeed.ATOM}}
#' 	\item{\code{JSON}} Get (Meta-)Content list from JSON, see \code{\link{getFeed.JSON}} 
#' 	\item{\code{CSV}} Get (Meta-)Content list from CSV, see \code{\link{getFeed.CSV}}
#' }
#' @param extractor Text extraction function used for \code{htmlConvertFields}
#' @param verbose Print feed retrieval status to console, defaults to TRUE
#' @param ... Addtional parameters for \code{getFeed.#type#} function
#' @return named list
#' @seealso \code{\link{xmlTreeParse}} \code{\link{strptime}} \code{\link{getCorpus}}
#' @note for examples see \code{\link{getCorpus}}
#' @export


`getFeed` <- 
function(feed, params, fieldnames, dateConvertFields = c("DateTimeStamp"), 
		htmlConvertFields = c("Description"), timeFormat, timeZone = "GMT", 
		type = "XML", extractor = extractHTMLStrip, verbose = TRUE, ...){
	
	#cause roxygen does not like '%'
	if(missing(timeFormat)){
		timeFormat = "%a, %d %b %Y %H:%M:%S"
	}

	
	if(!missing(params)){
		feeds <- feedquery(feed, params)
	}else{
		feeds <- feed
	}

	if(verbose)
		cat("Starting Feed Retrieval...\n")
	
	out <- list()
	for(f in feeds){
		if(verbose){
			cat("Retrieve from ", f, "...")
		}
		args <- list(feed = f, fieldnames = fieldnames, ...)
		l <- tryCatch(do.call(paste("getFeed.", type, sep = ""), args = args),
				error = function(e){
					warning(e)
					return(NULL)
				}
		)
		if(is.null(l))
			break
		
		for(n in names(l)){
			out[[n]] <- c(out[[n]], l[[n]])
		}
		if(verbose)
			cat("Done!\n")
		
	}

	for(dt in dateConvertFields){
		if(dt %in% names(out)){
			loc <- Sys.getlocale("LC_TIME")
			dummy <- Sys.setlocale("LC_TIME", "C")
			time <- strptime(out[[dt]], format = timeFormat, tz= timeZone)
			dummy <- Sys.setlocale("LC_TIME", loc)
			out[[dt]] <- as.POSIXct(time)
		}else{
			warning(paste("Date Conversion Field '",dt, "' could not be found in meta data frame"))
		}
	}
	
	for(h in htmlConvertFields){
		if(h %in% names(out)){
			out[[h]] <- tryCatch(sapply(out[[h]], extractor),
					error = function(e){
								warning(e)
								out[[h]]
					})
			
		}else{
			warning(paste("HTML Conversion Field '",h, "' could not be found in meta data frame"))
		}
	}
	
	return(out)
}

#' Get (Meta-)Content list from XML
#' After feed is retrieved Conversion of Date and HTML Content fields is done
#' @author Mario Annau
#' @param feed character of feed to retrieve, feed can either be a URL or file (see \code{\link{xmlTreeParse}} for more info)
#' @param fieldnames list of fieldnames to extract from feed
#' @param parent character which specifies xpath expression of `parent' node. 
#' @param ... additional parameters to xml value extraction FUNction
#' @seealso \code{\link{getFeed}}
#' @export
#' @importFrom XML xmlTreeParse
#' @importFrom XML getNodeSet
#' @importFrom XML xpathSApply
#' @importFrom XML xmlDoc
#' @importFrom XML xmlValue
`getFeed.XML` <- 
function(feed, fieldnames, parent, ...){
	out <- list()
	tree <- xmlTreeParse(feed, useInternal = TRUE, fullNamespaceInfo=TRUE);
	nodeset <- NULL
	if(!missing(parent)){
		nodeset <- getNodeSet(tree, path = parent, ...)
	}	
		
	for(n in names(fieldnames)){
		
		path <- fieldnames[[n]][[1]]
		FUN <- "xmlValue"
		args <- list()
		if(length(fieldnames[[n]]) > 1){
			FUN <- fieldnames[[n]][[2]]
		}
		if(length(fieldnames[[n]]) > 2){
			args <- fieldnames[[n]][[3]]
		}

		if(missing(parent)){
			out[[n]] <- xpathSApply(tree, path, function(x) do.call(FUN, args = list(x, args)), ...)
		}else{
			out[[n]] <- sapply(nodeset, function(x) xpathSApply(xmlDoc(x), path, function(y) do.call(FUN, args = list(y, args)),...))
		}
	}
	return(out)
}

#' Get (Meta-)Content list from ATOM Feed.
#' A Simple wrapper for \code{\link{getFeed.XML}}
#' @author Mario Annau
#' @param feed character of feed to retrieve, feed can either be a URL or file (see \code{\link{xmlTreeParse}} for more info)
#' @param fieldnames list of fieldnames to extract from feed
#' @param parent character which specifies xpath expression of `parent' node. 
#' @param ... additional parameters for getFeed.XML
#' @seealso \code{\link{getFeed.XML}} \code{\link{getFeed}}
#' @export
`getFeed.ATOM` <-
function(feed, fieldnames, parent, ...){
	return(getFeed.XML(feed, fieldnames, parent, ...))
}

#' Get (Meta-)Content list from RSS Feed.
#' A Simple wrapper for \code{\link{getFeed.XML}}
#' @author Mario Annau
#' @param feed character of feed to retrieve, feed can either be a URL or file (see \code{\link{xmlTreeParse}} for more info)
#' @param fieldnames list of fieldnames to extract from feed
#' @param ... additional parameters for \code{\link{getFeed.XML}} function
#' @seealso \code{\link{getFeed.XML}} \code{\link{getFeed}}
#' @export
`getFeed.RSS` <-
function(feed, fieldnames, ...){

	if(missing(fieldnames)){
		fieldnames <- 	list(
				title = "//item/title",
				link = "//item/link",
				description = "//item/description",
				guid = "//item/guid",
				pubDate = "//item/pubDate"
		)
	}
	return(getFeed.XML(feed, fieldnames, ...))
}

#' Get (Meta-)Content list from JSON Feed.
#' @author Mario Annau
#' @param feed character of feed to retrieve, feed can either be a URL or file (see \code{\link{xmlTreeParse}} for more info)
#' @param fieldnames list of fieldnames to extract from feed
#' @param textPreprocess preprocess text for RJSONIO parser - is at least necessary for reutersSpotligth content
#' @param parent Specifies parent node in JSON structure
#' @seealso \code{\link{getFeed}}
#' @export
#' @importFrom RCurl getURL
`getFeed.JSON` <- function(feed, fieldnames, parent, textPreprocess = TRUE){
	if(missing(parent)){
		stop("Please specify parent node")
	}
	
	require(RJSONIO)
	news <- getURL(feed)
	if(textPreprocess){
		news <- sub("^.*?\\(", "", news, perl = TRUE)
		news <- sub("\\).*?$", "", news, perl = TRUE)
		news <- gsub("\\\\'", "'", news, perl = TRUE)
		news <- gsub("\\\\\\n", " ", news, perl = TRUE)
		news <- gsub("\\\\(?=\\d)", "", news, perl = TRUE)
	}
	
	json <- fromJSON(news)
	out <- list()
	for(f in names(fieldnames)){
		out[[f]] <- sapply(json[[parent]], function(x) return(x[[fieldnames[[f]]]]))
	}
	
	return(out)
	
}

#' Get (Meta-)Content list from CSV
#' After feed is retrieved Conversion of Date and HTML Content fields is done
#' @author Mario Annau
#' @param feed character of feed to retrieve, feed should be a csv file name)
#' @param fieldnames list of fieldnames to extract from feed
#' @param stringsAsFactors strings in csv are treated as factors, defaults to FALSE, see \code{\link{read.csv}}
#' @param ... addtional parameters to \code{\link{read.csv}}
#' @seealso \code{\link{getFeed}}
#' @export
`getFeed.CSV` <- 
function(feed, fieldnames, stringsAsFactors = FALSE, ...){
	
	df <- read.csv(feed, stringsAsFactors = stringsAsFactors, ...)
	out <- list()
	
	for(f in names(fieldnames)){
		out[[f]] <- df[,fieldnames[[f]]]
	}
	return(out)
}


