
#' @S3method renew.token GoogleReaderSource
#' @noRd
renew.token.GoogleReaderSource <- function(x, gmail, gpass, verbose = getOption("verbose")) {
	newtoken <- auth.google.reader(email <- gmail, password = gpass)
	
	
	
	
}
