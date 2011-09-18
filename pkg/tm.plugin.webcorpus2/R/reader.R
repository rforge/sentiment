# TODO: Add comment
# 
# Author: mario
###############################################################################

#' Read content from WebXMLSource
#' @export
#' @importFrom XML xmlInternalTreeParse
#' @importFrom XML free
readWebXML <- FunctionGenerator(function(spec, doc, extractFUN = NULL,...) {
			spec <- spec
			doc <- doc
			extractFUN <- extractFUN
			function(elem, language, id) {
				#options(suppressXMLNamespaceWarning = TRUE)
				tree <- XML::xmlInternalTreeParse(elem$content, asText = TRUE)
				
				Content(doc) <- if ("Content" %in% names(spec))
							tm:::.xml_content(tree, spec[["Content"]])
						else if(!is.null(elem$linkcontent)){
							if(!is.null(extractFUN))
								extractFUN(elem$linkcontent)
							else
								elem$linkcontent
						}
						else
							xmlTreeParse(elem$content, asText = TRUE)
				for (n in setdiff(names(spec), "Content"))
					meta(doc, n) <- tm:::.xml_content(tree, spec[[n]])
				free(tree)
				#options(suppressXMLNamespaceWarning = FALSE)
				#attr(doc, "Language") <- language
				doc
			}
		})

#' Read content from WebHTMLSource
#' @export
#' @importFrom XML htmlTreeParse
#' @importFrom XML free
readWebHTML <- FunctionGenerator(function(spec, doc, extractFUN = NULL,...) {
			spec <- spec
			doc <- doc
			extractFUN <- extractFUN
			function(elem, language, id) {
				#options(suppressXMLNamespaceWarning = TRUE)
				tree <- XML::htmlTreeParse(elem$content, asText = TRUE, useInternalNodes = TRUE)
				
				Content(doc) <- if ("Content" %in% names(spec))
							tm:::.xml_content(tree, spec[["Content"]])
						else if(!is.null(elem$linkcontent)){
							if(!is.null(extractFUN))
								extractFUN(elem$linkcontent)
							else
								elem$linkcontent
						}
						else
							xmlTreeParse(elem$content, asText = TRUE)
				for (n in setdiff(names(spec), "Content"))
					meta(doc, n) <- tm:::.xml_content(tree, spec[[n]])
				free(tree)
				#options(suppressXMLNamespaceWarning = FALSE)
				#attr(doc, "Language") <- language
				doc
			}
		})

#' Read content from WebJSONSource
#' @export
readWebJSON <- FunctionGenerator(function(spec, doc, extractFUN = NULL,...) {
			spec <- spec
			doc <- doc
			extractFUN <- extractFUN
			function(elem, language, id) {
				#options(suppressXMLNamespaceWarning = TRUE)
				tree <- elem$content
				
				Content(doc) <- if ("Content" %in% names(spec))
							json_content(tree, spec[["Content"]])
						else if(!is.null(elem$linkcontent)){
							if(!is.null(extractFUN))
								extractFUN(elem$linkcontent)
							else
								elem$linkcontent
						}
						else
							character(0)
				for (n in setdiff(names(spec), "Content"))
					meta(doc, n) <- json_content(tree, spec[[n]])
				#free(tree)
				#options(suppressXMLNamespaceWarning = FALSE)
				#attr(doc, "Language") <- language
				doc
			}
		})

#' Read content from JSONSource
#' @export
json_content <- 
function (doc, spec) 
{
	type <- spec[[1]]
	fun <- switch(type, field = identity, node = identity)
	if (identical(type, "unevaluated")) 
		spec[[2]]
	else if (identical(type, "function") && is.function(spec[[2]])) 
		spec[[2]](doc)
	else as.character(sapply(doc[[spec[[2]]]], 
						fun))
}

#' Read content from NYTimesSource
#' @export
readNYTimes <- readWebJSON(spec = list(Author = list("field", "byline"),
				Description = list("field", "body"),
				DateTimeStamp = list("function", function(node)
							strptime(node[["date"]],
									format = "%Y%m%d",
									tz = "GMT")),
				Heading = list("field", "title"),
				Origin = list("field", "url")),
				Language = list("unevaluated", "en"),
#				ID = list("node",  "//id")),
		extractFUN = extractContentDOM,
		doc = PlainTextDocument())


#' Read content from TwitterSource
#' @export
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
readTwitter <- readWebXML(spec = list(Author = list("node", "//author/name"),
				Content = list("node", "//content"),
				DateTimeStamp = list("function", function(node)
							strptime(sapply(getNodeSet(node, "//published"), xmlValue),
									format = "%Y-%m-%dT%H:%M:%S",
									tz = "GMT")),
				Source = list("node", "//source"),
				Language = list("node", "//lang"),
				ID = list("node",  "//id")),
		extractFUN = extractHTMLStrip,
		doc = PlainTextDocument())


#' Read content from GoogleFinanceSource
#' @export
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
readGoogleFinance <- readWebXML(spec = list(Heading = list("node", "//title"),
				DateTimeStamp = list("function", function(node){
							val <- sapply(getNodeSet(node, "//pubDate"), xmlValue)
							val <- substr(val, regexpr("\\s", val)+1, nchar(val))
							strptime(val,format = "%d %b %Y %H:%M:%S",tz = "GMT")
						}),
				Origin = list("node", "//link"),
				Description = list("node", "//item/description"),
				ID = list("node",  "//guid")),
		extractFUN = extractContentDOM,
		doc = PlainTextDocument())

#' Read content from YahooFinanceSource
#' @export
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
readYahooFinance <- readWebXML(spec = list(Heading = list("node", "//title"),
				DateTimeStamp = list("function", function(node){
							val <- sapply(getNodeSet(node, "//pubDate"), xmlValue)
							val <- substr(val, regexpr("\\s", val)+1, nchar(val))
							strptime(val,format = "%d %b %Y %H:%M:%S",tz = "GMT")
						}),
				Origin = list("node", "//link"),
				Description = list("node", "//item/description"),
				ID = list("node",  "//guid")),
		extractFUN = extractContentDOM,
		doc = PlainTextDocument())


#' Read content from GoogleBlogSearchSource
#' @export
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
readGoogleBlogSearch <- readWebXML(spec=list(Heading = list("node", "//title"),
		DateTimeStamp = list("function", function(node){
					val <- sapply(getNodeSet(node, "//dc:date"), xmlValue)
					val <- substr(val, regexpr("\\s", val)+1, nchar(val))
					strptime(val,format = "%d %b %Y %H:%M:%S",tz = "GMT")
				}),
		Origin = list("node", "//link"),
		Description = list("node", "//item/description"),
		Publisher = list("node","//dc:publisher"),
		Author = list("node","//dc:creator")),
extractFUN = extractContentDOM,
doc = PlainTextDocument())


#' Read content from GoogleFinanceSource
#' @export
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
readYahooInplay <- readWebHTML(spec = list(Heading = list("node", "//b[1]"),
				Content = list("node", "//p"),
				DateTimeStamp = list("function", function(node){
							val <- unlist(getNodeSet(node, "//b[1]", fun = xmlValue))
							substr(val, 1, regexpr("\\s", val)-1)
						}),
				Ticker  = list("node", "//p/b/a")),
		extractFUN = NULL,
		doc = PlainTextDocument())

#' Read content from BingSource
#' @export
#' @importFrom XML getNodeSet
#' @importFrom XML xmlValue
readBing <- readWebXML(spec = list(Heading = list("node", "//news:Title"),
				Origin = list("node", "//news:Url"),
				DateTimeStamp = list("function", function(node)
							strptime(sapply(getNodeSet(node, "//news:Date"), xmlValue),
									format = "%Y-%m-%dT%H:%M:%SZ",
									tz = "GMT")),
				Author = list("node", "//news:Source"),
				Description = list("node", "//news:Snippet"),
				BreakingNews = list("node", "//news:BreakingNews")),
		extractFUN = extractContentDOM,
		doc = PlainTextDocument())