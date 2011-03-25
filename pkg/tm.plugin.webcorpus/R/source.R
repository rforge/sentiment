#'getSources related functions
#' Get all available corpus data sources
#' @author Mario Annau
#' @export getSources 
getSources <- function(){
c(tm::getSources(), "ListSource")
}



#'ListSource
#' Convert named list to a corpus
#' @author Mario Annau
#' @param x named list to be converted
#' @param encoding Encoding to be used for source reading
#' @aliases ListSource readList
#' @export ListSource 
ListSource <- function(x, encoding = "UTF-8") {
	s <- tm:::.Source(readPlain, encoding, length(x[[1]]), FALSE, names(x[[1]]), 0, TRUE)
	s$Content <- x
	class(s) <-  c("ListSource", "Source")
	s
}

#' @S3method getElem ListSource
#' @importFrom tm getElem
#' @nord
#' ...
getElem.ListSource <- function(x) list(content = x$Content[x$Position, ], uri = match.call()$x)

#' @S3method pGetElem ListSource
#' @importFrom tm pGetElem
#' @nord
#' ...
pGetElem.ListSource <- function(x)
	lapply(seq_len(x$Length), function(y) list(content = lapply(x$Content, function(z) z[y]), uri = match.call()$x))

#' @S3method eoi ListSource
#' @importFrom tm eoi
#' @nord
#' ...
eoi.ListSource <- function(x) length(x[[1]]) <= x$Position

#'@export readList
#' @nord
#' ...
readList <- FunctionGenerator(function(...) {
			#mapping <- mapping
			function(elem, language, id) {
				doc <- PlainTextDocument(id = id, language = language)
				for (n in names(elem$content))
					content_meta(doc, n) <- elem$content[[n]]
				doc
			}
		})