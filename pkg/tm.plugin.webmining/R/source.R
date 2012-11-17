# TODO: Add comment
# 
# Author: mario
###############################################################################

#' Read Web Content and respective Link Content from feedurls
#' @author Mario Annau
#' @param feedurls urls from feeds to be retrieved
#' @param class class label to be assigned to \code{Source} object, defaults to "WebXMLSource"
#' @param parser function to be used to split feed content into chunks, returns list of content elements
#' @param encoding specifies default encoding, defaults to 'UTF-8'
#' @param vectorized specifies if source is vectorized, defaults to FALSE
#' @param curlOpts a named list or CURLOptions object identifying the curl options for the handle. Type \code{listCurlOptions()} for all Curl options available.
#' @return WebSource
#' @export
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
#' @importFrom RCurl curlOptions
WebSource <- function(feedurls, class = "WebXMLSource", parser, encoding = "UTF-8", vectorized = FALSE,
		curlOpts = curlOptions(	followlocation = TRUE, 
				maxconnects = 20,
				maxredirs = 10,
				timeout = 30,
				connecttimeout = 30)){
	#TODO: implement "save url retrieval with retries"
	#cat("Retrieving ", feedurls, "\n")
	# retrieve feeds using RCurl
	content_raw <- getURL(feedurls, .opts = curlOpts)
	content_parsed <- unlist(lapply(content_raw, parser), recursive = FALSE)
	
	# generate source object
	s <- tm:::.Source(NULL, encoding, length(content_parsed), FALSE, NULL, 0, vectorized, class = class)
	
	s$Content <- content_parsed
	s$Feedurls <- feedurls
	s$Parser <- parser
	s$CurlOpts <- curlOpts
	
	s
}

#' Update Source
#' @param x Source object to be updated
#' @export source.update
#' @aliases source.update.WebXMLSource source.update.WebHTMLSource source.update.WebJSONSource
source.update <- function(x){
	UseMethod("source.update", x)	
}

#'update WebSource
#' @S3method source.update WebXMLSource
#' @S3method source.update WebHTMLSource
#' @S3method source.update WebJSONSource
#' @noRd
#update <- function(x) UseMethod("update", x)
source.update.WebXMLSource <- 
source.update.WebHTMLSource <- 
source.update.WebJSONSource <- 
function(x) {
	content_raw <- getURL(x$Feedurls, .opts = x$CurlOpts)
	content_parsed <- unlist(lapply(content_raw, x$Parser), recursive = FALSE)
	x$Content <- content_parsed
	x$Position <- 0
	x
}

#' Get Feed Meta Data from Google Finance
#' @author Mario Annau
#' @param query ticker symbols of companies to be searched for, see \url{http://www.google.com/finance}.
#' Please note that Google ticker symbols need to be prefixed with the exchange name, e.g. NASDAQ:MSFT
#' @param params, additional query parameters
#' @param ... additional parameters to \code{\link{WebSource}}
#' @return WebXMLSource
#' @seealso \code{\link{WebSource}}
#' @export
#' @examples
#' \dontrun{
#' corpus <- Corpus(GoogleFinanceSource("NASDAQ:MSFT"))
#' }
#' @importFrom XML xmlInternalTreeParse
#' @importFrom XML xpathSApply
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
#' @aliases readGoogle
GoogleFinanceSource <- function(query, params = 
				list( 	hl= 'en', 
						q=query, 
						ie='utf-8', 
						start = 0, 
						num = 20, 
						output='rss'),...){
	feed <- "http://www.google.com/finance/company_news"
	parser <- function(cr){
		tree <- xmlInternalTreeParse(cr, asText = TRUE)
		xpathSApply(tree, path = "//item")
	}
	
	fq <- feedquery(feed, params)
	ws <- WebSource(feedurls = fq, class = "WebXMLSource", parser = parser, ...)
	ws$DefaultReader <- readGoogle
	ws$PostFUN <- function(x){
		x <- getLinkContent(x)
		#tm_map(x, extract, extractor = ArticleExtractor)
	}
	ws
}

#' Get Feed Meta Data from Yahoo Finance
#' @author Mario Annau
#' @param query ticker symbols of companies to be searched for, see \url{http://finance.yahoo.com/lookup}.
#' @param params, additional query parameters, see \url{http://developer.yahoo.com/rss/}
#' @param ... additional parameters to \code{\link{WebSource}}
#' @return WebXMLSource
#' @export
#' @examples
#' \dontrun{
#' corpus <- Corpus(YahooFinanceSource("MSFT"))
#' }
#' @seealso \code{\link{WebSource}}
#' @importFrom XML xmlInternalTreeParse
#' @importFrom XML xpathSApply
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
#' @aliases readYahoo
YahooFinanceSource <- function(query, params = 
				list(	s= query, 
						n = 20), ...){
	feed <- "http://finance.yahoo.com/rss/headline"
	
	fq <- feedquery(feed, params)
	parser <- function(cr){
		tree <- xmlInternalTreeParse(cr, asText = TRUE)
		xpathSApply(tree, path = "//item")
	}
	
	#linkreader <- function(tree) getNodeSet(tree, ".//link", fun = xmlValue)
	
	ws <- WebSource(feedurls = fq, class = "WebXMLSource", parser = parser, ...)
	ws$DefaultReader <- readYahoo
	ws$PostFUN <- function(x){
		x <- getLinkContent(x)
		#tm_map(x, extract, extractor = ArticleExtractor)
	}
	ws
}

#' Get Feed Meta Data from Google Blog Search \url{http://www.google.com/blogsearch}
#' @author Mario Annau
#' @param query Google Blog Search query
#' @param params, additional query parameters
#' @param ... additional parameters to \code{\link{WebSource}}
#' @return WebXMLSource
#' @seealso \code{\link{WebSource}}
#' @export
#' @examples
#' \dontrun{
#' corpus <- Corpus(GoogleBlogSearchSource("Microsoft"))
#' }
#' @importFrom XML xmlInternalTreeParse
#' @importFrom XML xpathSApply
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
#' @aliases readGoogleBlogSearch
GoogleBlogSearchSource <- function(query, params = 
				list(	hl= 'en', 
						q = query, 
						ie='utf-8', 
						num = 100, 
						output='rss'), ...){
	feed <- "http://blogsearch.google.com/blogsearch_feeds"

	fq <- feedquery(feed, params)
	parser <- function(cr){
		tree <- xmlInternalTreeParse(cr, asText = TRUE)
		nodes <- xpathSApply(tree, path = "//item")
		xmlns1 <- lapply(nodes, newXMLNamespace, "http://purl.org/dc/elements/1.1/", "dc")
		nodes
	}
	
	ws <- WebSource(feedurls = fq, class = "WebXMLSource", parser = parser, ...)
	ws$DefaultReader <- readGoogleBlogSearch
	ws$PostFUN = function(x){
		x <- getLinkContent(x, extractor = DefaultExtractor)
		#tm_map(x, extract, extractor = ArticleExtractor)
	}
	ws
}


#' Get Feed Meta Data from Google News Search \url{http://news.google.com/}
#' @author Mario Annau
#' @param query Google News Search query
#' @param params, additional query parameters
#' @param ... additional parameters to \code{\link{WebSource}}
#' @return WebXMLSource
#' @seealso \code{\link{WebSource}}
#' @export
#' @examples
#' \dontrun{
#' corpus <- Corpus(GoogleNewsSource("Microsoft"))
#' }
#' @importFrom XML xmlInternalTreeParse
#' @importFrom XML xpathSApply
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
GoogleNewsSource <- function(query, params = 
				list(	hl= 'en', 
						q = query, 
						ie='utf-8', 
						num = 100, 
						output='rss'), ...){
	feed <- "http://news.google.com/news"
	
	fq <- feedquery(feed, params)
	parser <- function(cr){
		tree <- xmlInternalTreeParse(cr, asText = TRUE)
		nodes <- xpathSApply(tree, path = "//item")
		xmlns1 <- lapply(nodes, newXMLNamespace, "http://purl.org/dc/elements/1.1/", "dc")
		nodes
	}
	
	ws <- WebSource(feedurls = fq, class = "WebXMLSource", parser = parser, ...)
	ws$DefaultReader <- readGoogle
	ws$PostFUN = function(x){
		x <- getLinkContent(x)
		#tm_map(x, extract, extractor = ArticleExtractor)
	}
	ws
}

#' Get Feed Meta Data from Reuters News RSS Feeds
#' @author Mario Annau
#' @param query Reuters News RSS Feed, see \url{http://www.reuters.com/tools/rss} for a list of all feeds provided. Note that only string after 'http://feeds.reuters.com/reuters/' must be given. Defaults to 'businessNews'.
#' @param ... additional parameters to \code{\link{WebSource}}
#' @return WebXMLSource
#' @seealso \code{\link{WebSource}}
#' @export
#' @examples
#' \dontrun{
#' corpus <- Corpus(ReutersNewsSource("businessNews"))
#' }
#' @importFrom XML xmlInternalTreeParse
#' @importFrom XML xpathSApply
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
#' @aliases readReutersNews
ReutersNewsSource <- function(query = 'businessNews', ...){
	feed <- "http://feeds.reuters.com/reuters"
	
	fq <- paste(feed, query, sep = "/")
	#fq <- feedquery(feed, params)
	parser <- function(cr){
		tree <- xmlInternalTreeParse(cr, asText = TRUE)
		nodes <- xpathSApply(tree, path = "//item")
		xmlns1 <- lapply(nodes, newXMLNamespace, "http://rssnamespace.org/feedburner/ext/1.0", "feedburner")
		nodes
	}

	ws <- WebSource(feedurls = fq, class = "WebXMLSource", parser = parser, ...)
	ws$DefaultReader <- readReutersNews
	ws$PostFUN = function(x){
		x <- getLinkContent(x)
		#tm_map(x, extract, extractor = ArticleExtractor)
	}
	
	ws
}



#' Get Feed Meta Data from Twitter
#' @author Mario Annau
#' @param query Google Blog Search query
#' @param n number of results, defaults to 1500
#' @param params, additional query parameters, see \url{http://search.twitter.com/api/}
#' @param ... additional parameters to \code{\link{WebSource}}
#' @return WebXMLSource
#' @seealso \code{\link{WebSource}}
#' @export
#' @examples
#' \dontrun{
#' corpus <- Corpus(TwitterSource("Microsoft"))
#' }
#' @importFrom XML xmlInternalTreeParse
#' @importFrom XML xpathSApply
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
#' @aliases readTwitter
TwitterSource <- function(query, n = 1500, params = 
				list(lang = 'en'),...){
	feed <- "http://search.twitter.com/search.atom"

	if(is.null(params[["q"]])) params[["q"]] <- query
	if(is.null(params[["rpp"]])) params[["rpp"]] <- 100
	if(is.null(params[["page"]])) params[["page"]] <- seq(1,ceiling(n/params[["rpp"]]), by = 1)

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
	
	fq <- feedquery(feed, params)
	ws <- WebSource(feedurls = fq, class = "WebXMLSource", parser = parser, ...)
	ws$DefaultReader <- readTwitter
	#TODO: error with extractHTMLStrip, need tryCatch or whatever
	ws$PostFUN <- function(x){
		tm_map(x, extract, extractor = extractHTMLStrip, encoding = "UTF-8")
	}
	
	ws
}


#' Get Feed Meta Data from Yahoo News
#' @author Mario Annau
#' @param query words to be searched in Yahoo News, multiple words must be separated by '+'
#' @param params, additional query parameters, see \url{http://developer.yahoo.com/rss/}
#' @param ... additional parameters to \code{\link{WebSource}}
#' @return WebXMLSource
#' @export
#' @examples
#' \dontrun{
#' corpus <- Corpus(YahooNewsSource("Microsoft"))
#' }
#' @seealso \code{\link{WebSource}}
#' @importFrom XML xmlInternalTreeParse
#' @importFrom XML xpathSApply
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
YahooNewsSource <- function(query, params = 
				list(	p= query, 
						n = 20,
						ei = "UTF-8"), ...){
	feed <- "http://news.search.yahoo.com/rss"
	
	fq <- feedquery(feed, params)
	parser <- function(cr){
		tree <- xmlInternalTreeParse(cr, asText = TRUE)
		xpathSApply(tree, path = "//item")
	}
	
	ws <- WebSource(feedurls = fq, class = "WebXMLSource", parser = parser, ...)
	ws$DefaultReader = readYahoo
	ws$PostFUN = function(x){
		x <- getLinkContent(x)
		#tm_map(x, extract, extractor = ArticleExtractor)
	}
	ws
}



#' Get Feed Meta Data from NYTimes Article Search
#' @author Mario Annau
#' @param query character specifying query to be used to search NYTimes articles
#' @param n number of results defaults to 100
#' @param count number of results per page, defaults to 10
#' @param appid Developer App id to be used, obtained from \url{http://developer.nytimes.com/}
#' @param params additional query parameters, specified as list, see \url{http://developer.nytimes.com/docs/read/article_search_api}
#' @param ... additional parameters to \code{\link{WebSource}}
#' @seealso \code{\link{WebSource}}, \code{\link{readNYTimes}} 
#' @export
#' @examples
#' \dontrun{
#' #nytimes_appid needs to be specified
#' corpus <- Corpus(NYTimesSource("Microsoft", appid = nytimes_appid))
#' }
#' @export
#' @importFrom RJSONIO fromJSON
#' @aliases readNYTimes
NYTimesSource <- function(query, n = 100, count = 10, appid, params = 
		list(	format="json",
				query = query,
				offset=seq(0, n-count, by = count),
				"api-key" = appid),...){
	#importDefaults("getMeta.nytimes.articlesearch")
	feed <- "http://api.nytimes.com/svc/search/v1/article"
	fq <- feedquery(feed, params)
	
	parser <- function(cr){
		json <- fromJSON(cr)
		json$results
	}
	
	linkreader <- function(tree) tree[["url"]]
	
	ws <- WebSource(feedurls = fq, class = "WebJSONSource", parser = parser, ...)
	ws$DefaultReader <- readNYTimes
	ws$PostFUN = function(x){
		x <- getLinkContent(x)
		#tm_map(x, extract, extractor = ArticleExtractor)
	}
	ws
}



#' Get News from Yahoo Inplay
#' @author Mario Annau
#' @param ... additional parameters to \code{\link{WebSource}}
#' @return WebHTMLSource
#' @export
#' @examples
#' \dontrun{
#' corpus <- Corpus(YahooInplaySource())
#' }
#' @importFrom XML htmlTreeParse
#' @importFrom XML xpathSApply
#' @aliases readYahooInplay
YahooInplaySource <- function(...){
	url <- "http://finance.yahoo.com/marketupdate/inplay"
	parser <- function(cr){
		tree <- htmlTreeParse(cr, useInternalNodes = T)
		xp_expr = "//div[@class= 'yfitmbdy']/p"
		paragraphs = xpathSApply(tree, xp_expr)
	}
	
	ws <- WebSource(feedurls = url, class = "WebHTMLSource", parser = parser, ...)
	ws$DefaultReader = readYahooInplay
	ws
}


#' @S3method getElem WebXMLSource
#' @S3method getElem WebHTMLSource
#' @importFrom tm getElem eoi
#' @noRd
getElem.WebXMLSource <- 
getElem.WebHTMLSource <-
function(x) {
	virtual.file <- character(0)
	con <- textConnection("virtual.file", "w", local = TRUE)
	XML::saveXML(x$Content[[x$Position]], con)
	close(con)
	list(content = virtual.file, linkcontent = x$LinkContent[[x$Position]], uri = x$URI[[x$Position]])
}

#' @S3method getElem WebJSONSource
#' @noRd
getElem.WebJSONSource <- 
function(x) {
	list(content = x$Content[[x$Position]], linkcontent = x$LinkContent[[x$Position]], uri = x$URI[[x$Position]])
}

#' @S3method eoi WebXMLSource
#' @S3method eoi WebHTMLSource
#' @S3method eoi WebJSONSource
#' @noRd
eoi.WebXMLSource <- 
eoi.WebHTMLSource <- 
eoi.WebJSONSource <- 
function(x) length(x$Content) <= x$Position



