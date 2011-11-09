#' 
#' \tabular{ll}{
#' Package: \tab tm.plugin.webmining\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 201009-13\cr
#' License: \tab GPL-3\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' tm.plugin.webmining facilitates textual data retrieval from various 
#' web feed formats like XML and JSON. Also direct retrieval from HTML 
#' is supported. As most (news) feeds only incorporate small fractions
#' of the original text tm.plugin.webmining goes a step further and even
#' retrieves and extracts the text of the original text source.
#' Genereally, retrieval procedure can be described as a two--step process:
#' \itemize{
#' \item{Meta Retrieval}{In a first step, all relevant meta feeds are retrieved.
#' From these feeds all relevant meta data items are extracted.
#' }
#' \item{Content Retrieval}{In a second step all relevant source content is retrieved.
#' Using the \code{boilerpipeR} package even the main content of \code{HTML} pages can
#' be extracted.
#' }}
#' 
#' @name tm.plugin.webmining-package
#' @aliases tm.plugin.webmining webmining
#' @docType package
#' @title Retrieve structured, textual data from various web sources
#' @author Mario Annau \email{mario.annau@@gmail}
#' @keywords package
#' @seealso \code{\link{WebSource}} \code{\link{readWeb}}
#' @examples
#' \dontrun{
#' test1corp <- Corpus(GoogleFinanceSource("NASDAQ:MSFT"))
#' test2corp <- Corpus(TwitterSource("Microsoft"))
#' test3corp <- Corpus(NYTimesSource("Microsoft", appid = nytimes_appid))
#' test4corp <- Corpus(YahooInplaySource())
#' test5corp <- Corpus(YahooFinanceSource("MSFT"))
#' test6corp <- Corpus(GoogleBlogSearchSource("Microsoft"))
#' test7corp <- Corpus(BingSource("Microsoft", appid = bing_appid))
#' test8corp <- Corpus(YahooNewsSource("Microsoft"))
#' }
NULL

#' This is data to be included in my package
#'
#' @name baracktwitter
#' @docType data
#' @author Mario Annau
#' @references \url{search.twitter.com}
#' @keywords data
#' @examples
#' #Data set has been generated as follows:
#' \dontrun{
#' baracktwitter <- Corpus(TwitterSource("#BarackObama"))
#' baracktwitter <- tm_map(baracktwitter, function(x) iconv(x,from="UTF-8", to = "ASCII"))
#' baracktwitter <- tm_map(baracktwitter, function(x) {meta(x, "Author") <- iconv(meta(x, "Author"),from="UTF-8", to = "ASCII");x})
#' baracktwitter <- tm_map(baracktwitter, function(x) {meta(x, "AuthorURI") <- iconv(meta(x, "AuthorURI"),from="UTF-8", to = "ASCII");x})
#' baracktwitter <- tm_map(baracktwitter, function(x) {meta(x, "Source") <- iconv(meta(x, "Source"),from="UTF-8", to = "ASCII");x})
#' }
NULL

