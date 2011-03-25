#' Remove punctuations except specified one. 
#' Helper function for preprocessing of POS tagged items
#' @author Mario Annau
#' @param td TextDocument
#' @param except punctuation patterns which should not be removed
#' @seealso \code{\link{removePunctuation}} \code{\link{tm_map}}
#' @note Perl syntax is used.
#' @export
removePunctuationExcept <-
function(td, except = "/"){
	patt <- paste("[^A-Za-z\\s", except, "]", sep = "")
	Content(td) <- gsub(patt, "",Content(td), perl = TRUE)	
	removePunctuationExcept <- td
}

