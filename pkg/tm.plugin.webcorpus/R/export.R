#not implemented yet
#xmltolist <- function(){
#
#}
#`readWebCorpus` <- function(path, contentpath = "extractContentDOM", index = "index.xml", relativepaths = TRUE, ...){
#	if(relativepaths){
#		contentpath <- paste(path, contentpath, sep = "/")
#		index = paste(path, index, sep = "/")
#	}
#	
#	xmlidx <- xmlTreeParse(file = index)
#	
#	
#}
#
#contentpath = "extractContentDOM"
#index = "index.xml"
#relativepaths = TRUE

#' Create new XML node in tree
#' @param node xml node
#' @param item item to be added
#' @param nodename name of node to be added
#' @importFrom XML newXMLNode
#' @importFrom XML newXMLTextNode
#' @export
`addXMLNode` <- 
function(node, item, nodename){
	
	subnode <- newXMLNode(nodename, parent = node)
	
	if(class(item)[1] == "list"){
		for(i in 1:length(item)){
			iname <- "Item"
			if(!is.null(names(item))){
				iname <- names(item)[i]
			}
			node <- addXMLNode(subnode, item[[i]], iname)
		}
	}else{
		if(length(item) == 1){
			newXMLTextNode(as.character(item), parent = subnode)
		}else if(length(item) > 1){
			for(i in 1:length(item)){
				subnode <- addXMLNode(subnode, item[i], "Item")
			}
		}else{
			warning("Item is empty")
		}
	}
	node
	
	
}

#' Convert contents of a named list to an XML index
#' @note This function is still in an experimental status
#' @param corpuslist list holding elements of a (corpus) list
#' @param idxfilename character specifying full path of filename of XML index to be created
#' @export
`listtoxml` <- 
function(corpuslist, idxfilename){
	top = newXMLNode("Root")
	top <- addXMLNode(top, corpuslist, "Items")
	top
}

#' Write list containing web corpus to disk
#' @note This function is still in an experimental status
#' @param corpuslist list containing corpus fields
#' @param html_source list containing html sources of the corpus
#' @param sourcefield field specifying sourcefield in corpuslist which will not be used in xml index
#' @param path to be used for corpus list writing
#' @param extractdir character, specifying directory which holds extracted text elements, defaults to "extract"
#' @param sourcedir  character, specifying directory which holds extracted text elements, defaults to "source"
#' @param idxnum index numbers to be used for elements, defaults to \code{1:length(corpuslist[[1]])}
#' @importFrom XML saveXML
#' @export
`writeWebCorpus` <- 
function(corpuslist, html_source, sourcefield = NULL, path, extractdir = "extract", sourcedir = "source", idxnum = 1:length(corpuslist[[1]])){
	idxfilename <- paste(path, "index.xml", sep = "/")		
	extractdirFull <- paste(path, extractdir, sep = "/")
	sourcedirFull <- paste(path, sourcedir, sep = "/")
	
	dir.create(path, recursive = TRUE, showWarnings = FALSE)
	dir.create(extractdirFull, recursive = TRUE, showWarnings = FALSE)
	dir.create(sourcedirFull, recursive = TRUE, showWarnings = FALSE)

	#write content
	content <- corpuslist[["Content"]]
	fncontent <- paste(extractdirFull, "/", idxnum, ".txt", sep = "")
	dummy <- lapply(1:length(fncontent), function(x) writeLines(content[[x]], fncontent[x]))
	

	fnsource <- paste(sourcedirFull, "/", idxnum, ".html", sep = "")
	dummy <- lapply(1:length(fnsource), function(x) writeLines(html_source[x], fnsource[x]))

	
	corpuslist[["Content"]] <- NULL
	corpuslist[[sourcefield]] <- NULL
	
	xmllist <- listtoxml(corpuslist)
	saveXML(xmllist, idxfilename)
	
}
