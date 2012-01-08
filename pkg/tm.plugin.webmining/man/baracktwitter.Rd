\docType{data}
\name{baracktwitter}
\alias{baracktwitter}
\title{This is data to be included in my package}
\description{
  This is data to be included in my package
}
\examples{
#Data set has been generated as follows:
\dontrun{
baracktwitter <- Corpus(TwitterSource("#BarackObama"))
baracktwitter <- tm_map(baracktwitter, function(x) iconv(x,from="UTF-8", to = "ASCII"))
baracktwitter <- tm_map(baracktwitter, function(x) {meta(x, "Author") <- iconv(meta(x, "Author"),from="UTF-8", to = "ASCII");x})
baracktwitter <- tm_map(baracktwitter, function(x) {meta(x, "AuthorURI") <- iconv(meta(x, "AuthorURI"),from="UTF-8", to = "ASCII");x})
baracktwitter <- tm_map(baracktwitter, function(x) {meta(x, "Source") <- iconv(meta(x, "Source"),from="UTF-8", to = "ASCII");x})
}
}
\author{
  Mario Annau
}
\references{
  \url{search.twitter.com}
}
\keyword{data}
\keyword{datasets}

