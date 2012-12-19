\name{YahooInplaySource}
\alias{readYahooInplay}
\alias{YahooInplaySource}
\title{Get News from Yahoo Inplay.
Yahoo Inplay lists a range of company news provided by Briefing.com. Since Yahoo Inplay
does not provide a structured XML news feed, content is parsed directly from the HTML page.
Therefore, no further Source parameters can be specified. The number of feed items per
request can vary substantially.}
\usage{
  YahooInplaySource(...)
}
\arguments{
  \item{...}{additional parameters to
  \code{\link{WebSource}}}
}
\value{
  WebHTMLSource
}
\description{
  Get News from Yahoo Inplay. Yahoo Inplay lists a range of
  company news provided by Briefing.com. Since Yahoo Inplay
  does not provide a structured XML news feed, content is
  parsed directly from the HTML page. Therefore, no further
  Source parameters can be specified. The number of feed
  items per request can vary substantially.
}
\examples{
\dontrun{
corpus <- Corpus(YahooInplaySource())
}
}
\author{
  Mario Annau
}

