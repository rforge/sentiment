#' Extract Content from HTML Documents
#' @author Mario Annau
#' @param contentsrc list containing source text content to be extracted
#' @param extractor Extractor function to be used, defaults to \code{\link{extractContentDOM}}
#' @param verbose Print messages from extraction process, defaults to TRUE
#' @param ... additional parameters forwarded to extraction function
#' @seealso \code{\link{extractContentDOM}} \code{\link{extractHTMLStrip}} \code{\link{getCorpus}}
#' @references 	\url{http://www.elias.cn/En/ExtMainText}, 
#' 				\url{http://ai-depot.com/articles/the-easy-way-to-extract-useful-text-from-arbitrary-html/}
#' 				\cite{Gupta et al., DOM-based Content Extraction of HTML Documents},\url{http://www2003.org/cdrom/papers/refereed/p583/p583-gupta.html}
#' @export
extractContent <- 
function(contentsrc, extractor = "extractContentDOM", verbose = TRUE, ...){
	content <- list()
	if(!is.null(extractor)){
		if(verbose)
			cat("Extract Content...\n")
		content <- list()
		for(i in 1:length(contentsrc)){
			src <- contentsrc[[i]]
			content[[i]] <- tryCatch(do.call(extractor, list(src, ...)), 
					error = function(e){
						cat("An Error occured at Content Extraction, index ", i, "\n")
						print(e)
						return("")
					})
			gc()
			if(verbose){
				progress <- floor(i/length(contentsrc)*100)
				cat(progress, "%\r")
			}
		}
		if(verbose)
			cat("Done !\n")
	}else{
		content <- contentsrc
	}
	return(content)
}

#' Simply strip HTML Tags from Document
#' @author Mario Annau
#' @param url character, url or filename
#' @param asText specifies if url parameter is a \code{character}, defaults to TRUE
#' @param ... Additional parameters for \code{\link{htmlTreeParse}} 
#' @seealso \code{\link{xmlNode}}
#' @references 	\url{http://www.elias.cn/En/ExtMainText}, 
#' 				\url{http://ai-depot.com/articles/the-easy-way-to-extract-useful-text-from-arbitrary-html/}
#' 				\cite{Gupta et al., DOM-based Content Extraction of HTML Documents},\url{http://www2003.org/cdrom/papers/refereed/p583/p583-gupta.html}
#' @importFrom XML htmlTreeParse
#' @importFrom XML toString.XMLNode
#' @importFrom XML xmlChildren
#' @importFrom XML xmlValue
#' @export
extractHTMLStrip <-
function(url, asText = TRUE, ...){
	
	if(url == ""){
		return("")
	}
	
	parseerror <- capture.output(tree <- htmlTreeParse(url, asText = asText, useInternalNodes = TRUE, ...))
	
	childlen <- sapply(xmlChildren(tree), function(x) nchar(toString.XMLNode(x)))
	childidx <- which(childlen == max(childlen))
	html <- xmlChildren(tree)[[childidx]]
	
	return(xmlValue(html))
}

#' Extract Main HTML Content from DOM
#' Function extracts main HTML Content using its Document Object Model.
#' Idea comes basically from the fact, that main content of an HTML Document
#' is in a subnode of the HTML DOM Tree with a high text to tag - ratio.
#' @author Mario Annau
#' @param url character, url or filename
#' @param threshold threshold for extraction, defaults to 0.5
#' @param asText boolean, specifies if url should be interpreted as character
#' @param ... Additional Parameters to \code{\link{htmlTreeParse}}
#' @seealso \code{\link{xmlNode}}
#' @references 	\url{http://www.elias.cn/En/ExtMainText}, 
#' 				\url{http://ai-depot.com/articles/the-easy-way-to-extract-useful-text-from-arbitrary-html/}
#' 				\cite{Gupta et al., DOM-based Content Extraction of HTML Documents},\url{http://www2003.org/cdrom/papers/refereed/p583/p583-gupta.html}
#' @importFrom XML xmlChildren
#' @importFrom XML toString.XMLNode
#' @importFrom XML htmlTreeParse
#' @export
extractContentDOM <-
function(url, threshold = 0.5, asText = TRUE, ...){
	
	if(url == ""){
		return("")
	}
	
	parseerror <- capture.output(tree <- htmlTreeParse(url, asText = asText, useInternalNodes = TRUE, ...))
	childlen <- sapply(xmlChildren(tree), function(x) nchar(toString.XMLNode(x)))
	childidx <- which(childlen == max(childlen))
	html <- xmlChildren(tree)[[childidx]]
	tags <- c("script" , "noscript", "style")
	htmlclean <- removeTags(html, tags)
	
	htmlannotated <- assignValues(htmlclean, FUN = calcDensity, threshold)
	content <- getMainText(htmlannotated, threshold)
	return(content)
}

#' Calculate density of html text to overall length of html tree text
#' @author Mario Annau
#' @param xn object of class xmlNode
#' @param annotate Specifies if \code{xn} should be annotated, defaults to TRUE
#' @seealso \code{\link{extractContentDOM}}, \code{\link{xmlNode}}
#' @importFrom XML toString.XMLNode
#' @importFrom XML xmlValue
#' @importFrom XML addAttributes
calcDensity <-
function(xn, annotate = TRUE){
	textlen <- nchar( xmlValue(xn))
	treelen <- nchar(toString.XMLNode(xn))
	dens <- textlen / treelen
	if(annotate & inherits(xn, "XMLInternalElementNode")){
		addAttributes(xn, "dens" = dens, "textlen" = textlen, "treelen" = treelen)
	}
	return(c(dens, textlen, treelen))
}

#' Assign Values as Attributes to xmlNode
#' @author Mario Annau
#' @param t object of class xmlNode
#' @param FUN Function to be executed
#' @param threshold maximum threshold needed to step down the tree, defaults to 0.5
#' @param attribname ???
# TODO rewrite funtion to be more general and remove attribname???
#' @param recursive should tree be recursively annotated?, defaults to TRUE
#' @param mintextlen minimum textlength needed to step down the tree
#' @param ... additional arguments for FUN
#' @seealso \code{\link{extractContentDOM}}, \code{\link{xmlNode}}
#' @importFrom XML xmlApply
assignValues <-
function(t, FUN, threshold = 0.5, attribname = "attrib", recursive = TRUE, mintextlen = 10, ...){
	dens <- xmlApply(t, FUN)
	dens <- do.call("rbind", dens)
	#dens <- as.data.frame(dens)
	
	if(!recursive){
		return(t)
	}
	lapply(t[(dens[,2] > mintextlen) & (dens[,1] < threshold)], assignValues, FUN, ...)
	return(t)
	
}
#' Get Main Text from Annotated HTML Tree
#' Main Text is obtained from Tree -Subnode where threshold > threshold and 
#' textlength is at maximum
#' @author Mario Annau
#' @param xml object of class xmlNode
#' @param threshold minimum threshold needed to be considered
#' @seealso \code{\link{extractContentDOM}}, \code{\link{xmlNode}}
#' @importFrom XML xpathSApply
#' @importFrom XML xmlValue
getMainText <-
function(xml, threshold = 0.5){
	textlen <- as.numeric( xpathSApply(xml, path = "//attribute::textlen"))
	dens <- as.numeric( xpathSApply(xml, path = "//attribute::dens"))
	
	textlen[dens < threshold] <- 0
	idxmaintext <- which(textlen == max(textlen))
	if(max(textlen) == 0){
		return("")
	}
	
	content <-  xpathSApply(xml, path = paste("//*[@textlen][@dens]",sep = ""))[[idxmaintext]]
	
	cleancontent <-  xmlValue(content)
	cleancontent <- trimWhiteSpaces(cleancontent)
	
	return(cleancontent)
}

#' Remove specified tags from (XML) Document Tree.
#' Tags and all of its inner content will be removed.
#' @author Mario Annau
#' @param xmldoc xmlDoc object of class xmlDoc 
#' @param tags character vector which specifies tags to remove
#' @seealso \code{\link{extractContentDOM}}
#' @export
#' @importFrom XML getNodeSet
#' @importFrom XML removeNodes

removeTags <-
function(xmldoc, tags){
	#remove scripts tags
	xquery <- paste("//", tags, sep = "", collapse = " | ")
	scripts <-  getNodeSet(xmldoc, path = xquery)
	ret <- removeNodes(scripts , free = rep(FALSE, length(scripts)))
	removeTags <- xmldoc
}


