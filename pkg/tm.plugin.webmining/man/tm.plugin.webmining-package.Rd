\docType{package}
\name{tm.plugin.webmining-package}
\alias{tm.plugin.webmining}
\alias{tm.plugin.webmining-package}
\alias{webmining}
\title{Retrieve structured, textual data from various web sources}
\description{
  \tabular{ll}{ Package: \tab tm.plugin.webmining\cr Type:
  \tab Package\cr Version: \tab 0.1\cr Date: \tab
  201009-13\cr License: \tab GPL-3\cr LazyLoad: \tab yes\cr
  }
}
\details{
  tm.plugin.webmining facilitates textual data retrieval
  from various web feed formats like XML and JSON. Also
  direct retrieval from HTML is supported. As most (news)
  feeds only incorporate small fractions of the original
  text tm.plugin.webmining goes a step further and even
  retrieves and extracts the text of the original text
  source. Genereally, retrieval procedure can be described
  as a two--step process: \itemize{ \item{Meta
  Retrieval}{In a first step, all relevant meta feeds are
  retrieved. From these feeds all relevant meta data items
  are extracted. } \item{Content Retrieval}{In a second
  step all relevant source content is retrieved. Using the
  \code{boilerpipeR} package even the main content of
  \code{HTML} pages can be extracted. }}
}
\examples{
\dontrun{
test1corp <- Corpus(GoogleFinanceSource("NASDAQ:MSFT"))
test2corp <- Corpus(TwitterSource("Microsoft"))
test3corp <- Corpus(NYTimesSource("Microsoft", appid = nytimes_appid))
test4corp <- Corpus(YahooInplaySource())
test5corp <- Corpus(YahooFinanceSource("MSFT"))
test6corp <- Corpus(GoogleBlogSearchSource("Microsoft"))
test7corp <- Corpus(BingSource("Microsoft", appid = bing_appid))
test8corp <- Corpus(YahooNewsSource("Microsoft"))
}
}
\author{
  Mario Annau \email{mario.annau@gmail}
}
\seealso{
  \code{\link{WebSource}} \code{\link{readWeb}}
}
\keyword{package}

