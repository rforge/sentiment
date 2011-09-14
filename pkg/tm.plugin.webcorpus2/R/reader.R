# TODO: Add comment
# 
# Author: mario
###############################################################################

#' Read content from WebXMLSource
#' @export
#' @importFrom XML xmlTreeParse
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
				DateTimeStamp = list("function", function(node)
							strptime(sapply(getNodeSet(node, "//pubDate"), xmlValue),
									format = "%a, %d %b %Y %H:%M:%S",
									tz = "GMT")),
				Origin = list("node", "//link"),
				Description = list("node", "//item/description"),
				ID = list("node",  "//guid")),
		extractFUN = NULL,
		doc = PlainTextDocument())

