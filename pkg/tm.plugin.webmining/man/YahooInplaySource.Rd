\name{YahooInplaySource}
\alias{readYahooInplay}
\alias{YahooInplaySource}
\title{Get News from Yahoo Inplay}
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
  Get News from Yahoo Inplay
}
\examples{
\dontrun{
corpus <- Corpus(YahooInplaySource())
}
}
\author{
  Mario Annau
}

