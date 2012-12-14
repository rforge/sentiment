\docType{package}
\name{tm.plugin.webmining-package}
\alias{tm.plugin.webmining}
\alias{tm.plugin.webmining-package}
\alias{webmining}
\title{Retrieve structured, textual data from various web sources}
\description{
  \tabular{ll}{ Package: \tab tm.plugin.webmining\cr Type:
  \tab Package\cr Version: \tab 1.0\cr Date: \tab
  2012-12-13\cr License: \tab GPL-3\cr LazyLoad: \tab
  yes\cr }
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
googleblogsearch <- WebCorpus(GoogleBlogSearchSource("Microsoft"))
googlefinance <- WebCorpus(GoogleFinanceSource("NASDAQ:MSFT"))
googlenews <- WebCorpus(GoogleNewsSource("Microsoft"))
nytimes <- WebCorpus(NYTimesSource("Microsoft", appid = nytimes_appid))
reutersnews <- WebCorpus(ReutersNewsSource("businessNews"))
twitter <- WebCorpus(TwitterSource("Microsoft"))
yahoofinance <- WebCorpus(YahooFinanceSource("MSFT"))
yahooinplay <- WebCorpus(YahooInplaySource())
yahoonews <- WebCorpus(YahooNewsSource("Microsoft"))

token <- auth.google.reader()
feed <- "http://feeds.feedburner.com/RBloggers"
test <- WebCorpus(GoogleReaderSource(feed, auth.token = token, params = list(n = 100)))
}
}
\author{
  Mario Annau \email{mario.annau@gmail}
}
\seealso{
  \code{\link{WebCorpus}} \code{\link{readWeb}}
}
\keyword{package}

