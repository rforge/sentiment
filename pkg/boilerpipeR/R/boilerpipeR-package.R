#' 
#' \tabular{ll}{
#' Package: \tab boilerpipeR\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0\cr
#' Date: \tab 2012-12-13\cr
#' License: \tab Apache License (== 2.0)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' \pkg{boilerpipeR} interfaces the boilerpipe Java library, created by Christian
#' Kohlschutter \url{http://code.google.com/p/boilerpipe/}. It implements robust heuristics
#' to extract the main content of HTML files, removing unessecary
#' elements like ads, banners and headers/footers.
#' 
#' @name boilerpipeR-package
#' @aliases boilerpipe
#' @docType package
#' @title Extract the main content from HTML files
#' @author Mario Annau \email{mario.annau@@gmail}
#' @keywords package
#' @seealso \code{\link{Extractor}} \code{\link{DefaultExtractor}} \code{\link{ArticleExtractor}}
#' @examples
#' \dontrun{
#' data(content)
#' extract <- DefaultExtractor(content)
#' cat(extract)
#' }
NULL

#' This is data to be included in my package
#'
#' @name content
#' @docType data
#' @author Mario Annau
#' @references \url{http://quantivity.wordpress.com}
#' @keywords data
#' @examples
#' #Data set has been generated as follows:
#' \dontrun{
#' library(RCurl)
#' url <- "http://quantivity.wordpress.com/2012/11/09/multi-asset-market-regimes/"
#' content <- getURL(url)
#' }
NULL

