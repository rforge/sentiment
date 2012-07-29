# TODO: Add comment
# 
# Author: mario
###############################################################################

#' Extract Main Content from Text Documents
#' @param x PlainTextDocument
#' @param extractor default extraction function to be used, defaults to \code{\link{extractContentDOM}}
#' @param ... additional parameters to extractor function
#' @export
#' @aliases extract.PlainTextDocument
extract <- function(x, extractor, ...) UseMethod("extract", x)


#' Extract Main Content from Text Documents
#' @S3method extract PlainTextDocument
#' @param x PlainTextDocument
#' @param extractor default extraction function to be used, defaults to \code{\link{extractContentDOM}}
#' @param ... additional parameters to extractor function
#' @export
#' @noRd
extract.PlainTextDocument <- function(x, extractor = extractContentDOM, ...){
	Content(x) <- tryCatch(extractor(x, ...),
							error = function(e){
								warning(e)
								Content(x)
							})
	x
} 
