# TODO: Add comment
# 
# Author: mario
###############################################################################


#' Enclose Text Content in HTML tags
#' @param x object of PlainTextDocument class
#' @export
#' @aliases encloseHTML.PlainTextDocument encloseHTML.character
encloseHTML <- function(x) UseMethod("encloseHTML", x)

#' @S3method encloseHTML PlainTextDocument
#' @export 
#' @noRd
# FIXME: Could be done easier?? 
encloseHTML.PlainTextDocument <- function(x){
	Content(x) <- sprintf("<html>%s</html>", x)
	x
} 

#' Remove non-ASCII characters from Text
#' @param x object of PlainTextDocument class
#' @param fields specifies fields to be converted, defaults to fields = c("Content", "Heading", "Description")
#' @param from specifies encoding from which conversion should be done, defaults to "UTF-8"
#' @param to speciefies target encoding, defaults to "ASCII//TRANSLIT"
#' @export
#' @aliases removeNonASCII.PlainTextDocument
removeNonASCII <- function(x, fields = c("Content", "Heading", "Description"), from = "UTF-8", to = "ASCII//TRANSLIT")
	UseMethod("removeNonASCII", x)

#' @S3method removeNonASCII PlainTextDocument
#' @export 
#' @noRd
removeNonASCII.PlainTextDocument <- function(x, fields = c("Content", "Heading", "Description"), from = "UTF-8", to = "ASCII//TRANSLIT"){
	if("Content" %in% fields){
		Content(x) <- iconv(x, from, to)
	}
	for(fn in setdiff(fields, "Content")){
		meta(x, fn) <- iconv(meta(x, fn), from, to)
	}
	x
} 