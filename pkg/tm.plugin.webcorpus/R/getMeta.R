#' Get Feed Meta Data from Yahoo! Finance
#' @author Mario Annau
#' @param query ticker symbols of companies to be searched for, see \url{http://finance.yahoo.com/lookup}
#' @param n number of results (curr. max is 20)
#' @param verbose print status output to console, defaults to TRUE
#' @param ... additional query parameters, see \url{http://developer.yahoo.com/finance/company.html}
#' @return list
#' @seealso \code{\link{getFeed}} \code{\link{getCorpus}}
#' @examples
#' \dontrun{
#' metalist <- getMeta.yahooFinance("MSFT", n = 20)
#' corpus <- getCorpus("MSFT", src = "yahooFinance", n = 20)
#' }
#' @export
`getMeta.yahooFinance` <- function(query, n = 20, verbose = TRUE, ...){
	#if multiple ticker symbols have been entered
	
	feed <- "http://finance.yahoo.com/rss/headline"
	params <- list(	s= query, n = n, ...)
	
	
	
	fieldnames <- 	list(
			Heading = "//item/title",
			Origin = "//item/link",
			Description = "//item/description",
			guid = "//item/guid",
			DateTimeStamp = "//item/pubDate"
	)
	meta <- getFeed(feed, params = params, fieldnames = fieldnames, htmlConvertFields = c(), type = "RSS", verbose = verbose)
	return(meta)
}

#' Get Feed Meta Data from Google Finance
#' @author Mario Annau
#' @param query ticker symbols of companies to be searched for, see \url{http://finance.yahoo.com/lookup}.
#' Please note that Google ticker symbols need to be prefixed with the exchange name, e.g. NASDAQ:MSFT
#' @param n number of results (curr. max is ?), defaults to 20
#' @param verbose print status output to console, defaults to TRUE
#' @param ... additional query parameters
#' @return list
#' @seealso \code{\link{getFeed}} \code{\link{getCorpus}}
#' @examples
#' \dontrun{
#' metalist <- getMeta.googleFinance("NASDAQ:MSFT", n = 20)
#' corpus <- getCorpus("NASDAQ:MSFT", src = "googleFinance", n = 20)
#' }
#' @export
`getMeta.googleFinance` <- function(query, n = 20, verbose = TRUE, ...){
	feed <- "http://www.google.com/finance/company_news"
	params = list(hl= 'en', q=query, ie='utf-8', start = 0, num = n, output='rss', ...)
	
	fieldnames <- 	list(
			Heading = "//item/title",
			Origin = "//item/link",
			Description = "//item/description",
			guid = "//item/guid",
			DateTimeStamp = "//item/pubDate"
	)
	
	meta <- getFeed(feed, params = params, fieldnames = fieldnames, type = "RSS", verbose = verbose)
	return(meta)
}

#' Get Feed Meta Data from Google BlogSearch
#' @author Mario Annau
#' @param query character vector of keywords to be searched for
#' @param n number of results (curr. max is ?), defaults to 100
#' @param verbose print status output to console, defaults to TRUE
#' @param ... additional query parameters
#' @return list
#' @seealso \code{\link{getFeed}} \code{\link{getCorpus}}
#' @examples
#' \dontrun{
#' query <- c("MSFT","Microsoft")
#' metalist <- getMeta.googleBlogSearch(query, n = 20)
#' corpus <- getCorpus(query, src = "googleBlogSearch", n = 20)
#' }
#' @export
`getMeta.googleBlogSearch` <- function(query, n = 100, verbose = TRUE, ...){
	fieldnames <- 	list(
			Heading = "//item/title",
			Origin = "//item/link",
			Description = "//item/description",
			Publisher = "//item/dc:publisher",
			Author = "//item/dc:creator",
			DateTimeStamp = "//item/dc:date"
	)
	feed <- "http://blogsearch.google.com/blogsearch_feeds"
	params = list(hl= 'en', q = query, ie='utf-8', num = n, output='rss', ...)
	
	meta <- getFeed(feed, params = params, fieldnames = fieldnames, type = "RSS", verbose = verbose)
	return(meta)
}


#' Get Feed Meta Data from MS Bing Live Search API
#' @author Mario Annau
#' @param query character vector of keywords to be searched for
#' @param n number of results (curr. max is ?), defaults to 100
#' @param count number of results per page, defaults to 10
#' @param appid Developer App id to be used obtained from \url{http://www.bing.com/developers}
#' @param sources Specifies source type like "news", "web" or "video", see \url{http://msdn.microsoft.com/en-us/library/bb266167.aspx}
#' @param market Market for which search should be done, defaults to "en-US" (typically offers most results)
#' @param verbose print status output to console, defaults to TRUE
#' @param ... additional query parameters, see \url{http://msdn.microsoft.com/en-us/library/bb266180.aspx}
#' @return list
#' @seealso \code{\link{getFeed}} \code{\link{getCorpus}}
#' @examples
#' \dontrun{
#' query <- c("MSFT","Microsoft")
#' metalist <- getMeta.bing(query, n = 20)
#' corpus <- getCorpus(query, src = "bing", n = 20, appid = #Your App-ID#)
#' }
#' @export
`getMeta.bing` <- function(query, n = 100, count = 10, appid,  sources = "news", market = "en-US", verbose = TRUE, ...){
	importDefaults("getMeta.bing")
	feed <- "http://api.search.live.net/xml.aspx"
	count = count
	maxitems = n
	offset = seq(0, maxitems-count, by = count)
	
	if(missing(appid)){
		stop("Please specify bing appid retrieved from http://bing.com/developers.")
	}
	
	countfield <- paste(sources, ".count", sep = "")
	offsetfield <- paste(sources, ".offset", sep = "")
	
	params <- list(	Appid = appid,
			query = query,
			sources=sources,
			offsetfield = offset,
			...
	)
	
	params[[countfield]] <- count
	params[[offsetfield]] <- offset
	
	if(market != ""){
		params[["market"]] <- market
	}
	
	namespaces = c("news" = "http://schemas.microsoft.com/LiveSearch/2008/04/XML/news")
	
	
	fieldnames <- list(
			Heading = "//news:NewsResult/news:Title",
			Origin = "//news:NewsResult/news:Url",
			Author = "//news:NewsResult/news:Source",
			Description = "//news:NewsResult/news:Snippet",
			DateTimeStamp = "//news:NewsResult/news:Date",
			BreakingNews = "//news:NewsResult/news:BreakingNews"
	)
	
	timeFormat = "%Y-%m-%dT%H:%M:%SZ"						
	meta <- getFeed(feed = feed, params = params, fieldnames = fieldnames, htmlConvertFields = c(), type = "XML", namespaces = namespaces,timeFormat=timeFormat, verbose = verbose)
	return(meta)
}

#' Get Feed Meta Data from Yahoo BOSS
#' @author Mario Annau
#' @param query character vector of keywords to be searched for
#' @param type Specifies source type, defaults to "news", see \url{http://developer.yahoo.com/search/boss/boss_guide/index.html} for other
#' @param n number of results (curr. max is ?), defaults to 100, (max is ~1000)
#' @param count number of results per page, defaults to 50
#' @param appid Developer App id to be used obtained from \url{http://developer.yahoo.com/search/boss/}
#' @param lang Language of search results, deraults to "en"
#' @param verbose print status output to console, defaults to TRUE
#' @param age, maximum age of search results, defaults to "30d" (maximum is 30 days)
#' @param ... additional query parameters, see \url{http://developer.yahoo.com/search/boss/boss_guide/index.html}
#' @return list
#' @seealso \code{\link{getFeed}} \code{\link{getCorpus}}
#' @examples
#' \dontrun{
#' query <- c("MSFT","Microsoft")
#' metalist <- getMeta.yahooBOSS(query, n = 1000)
#' corpus <- getCorpus(query, src = "yahooBOSS", n = 20, appid = #Your App-ID#)
#' }
#' @export
`getMeta.yahooBOSS` <- function(query, type = "news", n = 100, count = min(50, n), appid, lang = "en", verbose = TRUE, age = "30d", ...){
	importDefaults("getMeta.yahooBOSS")
	start <- seq(0, n-count, by = count)
	params <- list(	appid= appid,
			version= "2.0",
			count = count,
			start = start,
			format="xml",
			abstract="long",
			lang = lang, 
			age = age,
			...)
	
	
	feed <- paste("http://boss.yahooapis.com/ysearch/", type, "/v1/", query, sep = "")
	
	fieldnames <- 	list(
			Heading = "//a:result/a:title",
			Origin = "//a:result/a:url",
			Description = "//a:result/a:abstract",
			DateTimeStamp = "//a:result/a:date",
			Language = "//a:result/a:language",
			Author = "//a:result/a:source"
	)
	
	meta <- getFeed(feed, params = params, fieldnames = fieldnames, namespaces = c("a" = "http://www.inktomi.com/"), timeFormat = "%Y/%m/%d", htmlConvertFields = c(), type = "XML", verbose = verbose)
	return(meta)
}

#' Get Feed Meta Data and Content from Reuters Spotlight
#' @author Mario Annau
#' @param query character specifying news channel to be used, defaults to "newsOne", see \url{http://spotlight.reuters.com/page/2007/07/10/feeds} for all available channels
#' @param edition specifies edition to be used, like "us", "uk", "ru", see \url{http://spotlight.reuters.com/page/2007/07/10/feeds} for all available editions
#' @param content Content type to be used, like "channelarticles", "channelnews", "channelphotos" or "channelvideos"
#' @param n number of results defaults to 100, (max is 100)
#' @param type \code{character} specifying content type to be used, can either be "json", "atom" or "rss", defaults to "json". Please note that no content is integrated in RSS feed.
#' @param appid Developer App id to be used, obtained from \url{http://spotlight.reuters.com/}
#' @param verbose print status output to console, defaults to TRUE 
#' @param ... additional query parameters, see \url{http://spotlight.reuters.com/page/2007/07/10/feeds}
#' @return list
#' @seealso \code{\link{getFeed}} \code{\link{getCorpus}}
#' @examples
#' \dontrun{
#' #Retrieve Gold Market Report from Reuters Spotlight
#' query <- "goldMktRpt"
#' metalist <- getMeta.spotlight(query, n = 100)
#' corpus <- getCorpus(query, src = "spotlight", n = 100, appid = #Your App-ID#)
#' }
#' @importFrom XML xmlGetAttr
#' @export
`getMeta.spotlight` <- function(query = "newsOne", edition = "us", content = "channelarticles", n = 100, type = "json", appid, verbose = TRUE, ...){
	
	importDefaults("getMeta.spotlight")
	params <- list(	apikey= appid, count = n, version= "2.0", ...)
	timeFormat = "%Y-%m-%dT%H:%M:%SZ"
	
	feed <- paste("http://spotlight.reuters.com/api/feed",edition, content, query, type, sep = "/")
	
	metalst <- switch(type, 
			json = {
				
				parent <- "items"
				fieldnames = list(	Heading = "title",
						Origin  = "link",
						guid	= "guid",
						category= "category",
						DateTimeStamp = "published",
						Description = "description",
						keywords = "keywords",
						tickers = "tickers",
						bodytype = c("body", "type"),
						Language = c("body", "lang"),
						Content = c("body", "text"))
				meta <- getFeed(feed, params, fieldnames, type = "JSON", parent = "items", timeFormat = timeFormat, htmlConvertFields = c(), verbose = verbose)
				meta[["Content"]] <- extractContent(meta[["Content"]], extractor = extractHTMLStrip, verbose = verbose)
				meta
				
			},
			atom = {
				parent <- "//a:entry"
				fieldnames <- 	list(
						Heading = "//a:title",
						Origin = list("//a:link[@href]","xmlGetAttr", "href"),
						copyright = "//a:rights",
						id = "//a:id",
						DateTimeStamp = "//a:updated",
						category = list("//a:category[@term]","xmlGetAttr", "term"),
						ticker = "//rt:ticker/rt:symbol",
						Description = "//a:summary",
						Content = "//a:content"
				)
				namespaces = c("a" = "http://www.w3.org/2005/Atom", "rt" = "http://www.reuters.com/feeds", "rdf" = "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
				meta <- getFeed(feed, params = params, parent = parent, fieldnames = fieldnames, type = "ATOM", timeFormat = timeFormat, namespaces = namespaces, htmlConvertFields = c(), verbose = verbose)
				meta[["Content"]] <- extractContent(meta[["Content"]], extractor = extractHTMLStrip, verbose = verbose)
				meta
			},
			rss = {
				fieldnames <- 	list(
						Heading = "//item/title",
						Origin = "//item/link",
						Description = "//item/description",
						guid = "//item/guid",
						DateTimeStamp = "//item/pubDate",
						category = "//item/category"
				)
				meta <- getFeed(feed, params = params, fieldnames = fieldnames, type = "RSS", timeFormat = timeFormat, htmlConvertFields = c(), verbose = verbose)
				meta
			}
	)
	if(is.null(metalst)){
		stop("Specified Parameter 'type' was not recognised.")
	}
	return(metalst)
}

#' Get Feed Meta Data from NYTimes Article Search
#' @author Mario Annau
#' @param query character specifying query to be used to search NYTimes articles
#' @param n number of results defaults to 100
#' @param count number of results per page, defaults to 10
#' @param appid Developer App id to be used, obtained from \url{http://developer.nytimes.com/}
#' @param verbose print status output to console, defaults to TRUE
#' @param ... additional query parameters, see \url{http://developer.nytimes.com/docs/read/article_search_api}
#' @return list
#' @seealso \code{\link{getFeed}} \code{\link{getCorpus}}
#' @examples
#' \dontrun{
#' query <- c("MSFT","Microsoft")
#' metalist <- getMeta.nytimes.articlesearch(query, n = 100)
#' corpus <- getCorpus(query, src = "nytimes.articlesearch", n = 100, appid = #Your App-ID#)
#' }
#' @export
`getMeta.nytimes.articlesearch` <- function(query, n = 100, count = 10, appid, verbose = TRUE,...){
	importDefaults("getMeta.nytimes.articlesearch")
	feed <- "http://api.nytimes.com/svc/search/v1/article"
	offset <- seq(0, n-count, by = count)
	params <- list(format="json",
			query = query,
			offset=offset,
			"api-key" = appid, 
			...
	)
	parent <- "results"
	fieldnames = list(	Heading = "title",
			Origin  = "url",
			DateTimeStamp = "date",
			Description = "body")
	
	fq <- feedquery(feed, params)
	
	meta <- getFeed(feed, params = params, parent = parent, fieldnames = fieldnames, type = "JSON", textPreprocess = FALSE, timeFormat = "%Y%m%d")
	return(meta)
}

#' Get Feed Meta Data from Twitter
#' @author Mario Annau
#' @param query character specifying query to be used to search tweets
#' @param count number of results per page, defaults to 100
#' @param n number of results, defaults to 100
#' @param verbose print status output to console, defaults to TRUE
#' @param ... additional query parameters, see \url{http://search.twitter.com/api/}
#' @return list
#' @seealso \code{\link{getFeed}} \code{\link{getCorpus}}
#' @examples
#' \dontrun{
#' query <- c("MSFT","Microsoft")
#' metalist <- getMeta.twitter(query, n = 100)
#' corpus <- getCorpus(query, src = "twitter", n = 100)
#' }
#' @export
`getMeta.twitter` <- function(query, count = 100, n = 100, verbose = TRUE, ...){
	importDefaults("getMeta.twitter")
	
	page <- seq(1,ceiling(n/count), by = 1)
	params <- list( q = query,
			rpp = count,
			page=page,
			...
	)
	
	feed <- "http://search.twitter.com/search.atom"

	#parent <- "//a:entry"
	
	fieldnames = list(	ID = "///a:entry/a:id",
		Author = "///a:entry/a:author/a:name",
		DateTimeStamp = "///a:entry/a:published",
		Source = "///a:entry/twitter:source",
		Language = "///a:entry/twitter:lang",
		Content = "///a:entry/a:content"
	)
	
	namespaces = c(	"google" = "http://base.google.com/ns/1.0", 
			"openSearch" = "http://a9.com/-/spec/opensearch/1.1/",  
			"georss"="http://www.georss.org/georss", 
			"a" = "http://www.w3.org/2005/Atom", 
			"twitter"="http://api.twitter.com/")


	meta <- getFeed(feed, params = params,fieldnames = fieldnames, type = "ATOM", timeFormat = "%Y-%m-%dT%H:%M:%S", namespaces = namespaces, htmlConvertFields = c("Content"), verbose = verbose)
	#meta <- getFeed(feed, params = params,fieldnames = fieldnames, type = "ATOM", timeFormat = "%Y-%m-%dT%H:%M:%S", namespaces = namespaces, htmlConvertFields = c(), verbose = verbose)
	
	#meta$Content <- lapply(meta$Content, function(x) xmlValue(getNodeSet(htmlParse(x, asText = TRUE), "//p")[[1]]))
	
# JSON Variant with unfixed encoding problems 
#	
#	feed <- "http://search.twitter.com/search.json"
#	parent <- "results"
#	fq <- feedquery(feed, params)
#	fieldnames = list(	ID = "id",
#			Author = "from_user",
#			AuthorID = "from_user_id_str",
#			DateTimeStamp = "created_at",
#			Source = "source",
#			Language = "iso_language_code",
#			Content = "text")
#	
#	meta <- getFeed(feed, params = params, parent = parent, fieldnames = fieldnames, type = "JSON", textPreprocess = FALSE, timeFormat = "%a, %d %b %Y %H:%M:%S", htmlConvertFields = c())
	
	return(meta)
}

