# TODO: Add comment
# 
# Author: mario
###############################################################################

#' Read Web Content and respective Link Content from feedurls
#' @param feedurls urls from feeds to be retrieved
#' @param parser function to be used to split feed content into chunks, returns list of content elements
#' @param linkreader function to be applied to content elements from parser, returns character vector of urls
#' @param getLinkContent specifies whether urls from linkreader should be retrieved
#' @param encoding specifies default encoding, defaults to 'UTF-8'
#' @export
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
WebSource <- function(feedurls, parser, linkreader, getLinkContent = !missing(linkreader), encoding = "UTF-8"){
	#TODO: do something "better"
	
	
	#cat("Retrieving ", feedurls, "\n")
	# retrieve feeds using RCurl
	content_raw <- getURL(feedurls)
	
	content_parsed <- unlist(lapply(content_raw, parser))
	
	# extract content urls from feeds
	s <- tm:::.Source(NULL, encoding, length(content_parsed), FALSE, NULL, 0, FALSE)
	
	#TODO: do parsing and concat stuff together
	#like lapply(content_raw, parse)
	#function(tree) XML:: 
	s$Content <- content_parsed
	
	if(getLinkContent){ # if contentpath is se
		content_urls <- unlist(sapply(content_parsed, linkreader))
		
		#cat("Retrieving Content...\n")
		content <- getURL(content_urls)
		s$LinkContent = content
	}
	
	class(s) <- c("WebSource", "Source")
	s
}

#' Get Feed Meta Data from Google Finance
#' @author Mario Annau
#' @param query ticker symbols of companies to be searched for, see \url{http://finance.yahoo.com/lookup}.
#' Please note that Google ticker symbols need to be prefixed with the exchange name, e.g. NASDAQ:MSFT
#' @param n number of results (curr. max is ?), defaults to 20
#' @param ... additional query parameters
#' @return list
#' @export
#' @importFrom XML xmlInternalTreeParse
#' @importFrom XML xpathSApply
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
GoogleFinanceSource <- function(query, n = 20, ...){
	feed <- "http://www.google.com/finance/company_news"
	params = list(hl= 'en', q=query, ie='utf-8', start = 0, num = n, output='rss', ...)
	fq <- feedquery(feed, params)
	
	parser <- function(cr){
		tree <- xmlInternalTreeParse(cr, asText = TRUE)
		xpathSApply(tree, path = "//item")
	}
	
	linkreader <- function(tree) getNodeSet(tree, ".//link", fun = xmlValue)
	
	ws <- WebSource(feedurls = fq, parser = parser, linkreader = linkreader)
	ws$DefaultReader = readGoogleFinance
	class(ws) <- c("WebXMLSource", "Source")
	ws
}

#' Get Feed Meta Data from Twitter
#' @author Mario Annau
#' @param query character specifying query to be used to search tweets
#' @param count number of results per page, defaults to 100
#' @param n number of results, defaults to 100
#' @param ... additional query parameters, see \url{http://search.twitter.com/api/}
#' @return list
#' @export
#' @importFrom XML xmlInternalTreeParse
#' @importFrom XML xpathSApply
#' @importFrom XML newXMLNamespace
TwitterSource <- function(query, count = 100, n = 1500, ...){
	page <- seq(1,ceiling(n/count), by = 1)
	params <- list( q = query,
			rpp = count,
			page=page,
			...
	)
	
	feed <- "http://search.twitter.com/search.atom"
	
	parser <- function(cr){
		namespaces = c(	"google" = "http://base.google.com/ns/1.0", 
				"openSearch" = "http://a9.com/-/spec/opensearch/1.1/",  
				"georss"="http://www.georss.org/georss", 
				"a" = "http://www.w3.org/2005/Atom", 
				"twitter"="http://api.twitter.com/")
		
		tree <- xmlInternalTreeParse(cr, asText = TRUE)
		nodes <- xpathSApply(tree, path = "///a:entry", namespaces = namespaces)
		#to surpress namespace warnings while parsing
		xmlns1 <- lapply(nodes, newXMLNamespace, "http://api.twitter.com/", "twitter")
		xmlns2 <- lapply(nodes, newXMLNamespace, "http://www.georss.org/georss", "georss")
		nodes
	}
	
	fq <- feedquery(feed, params, ...)
	ws <- WebSource(feedurls = fq, parser = parser)
	ws$DefaultReader = readTwitter
	class(ws) <- c("WebXMLSource", "Source")
	ws
}


#' @S3method getElem WebXMLSource
#' @nord
#' ...
getElem.WebXMLSource <- function(x) {
	# Construct a character representation from the XMLNode
	virtual.file <- character(0)
	con <- textConnection("virtual.file", "w", local = TRUE)
	XML:::saveXML(x$Content[[x$Position]], file = con)
	close(con)

	list(content = virtual.file, linkcontent = x$LinkContent[x$Position], uri = x$URI)
}

#' @S3method eoi WebXMLSource
#' @nord
#' ...
eoi.WebXMLSource <- function(x) length(x$Content) <= x$Position