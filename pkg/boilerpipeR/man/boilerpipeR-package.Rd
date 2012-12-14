\docType{package}
\name{boilerpipeR-package}
\alias{boilerpipe}
\alias{boilerpipeR-package}
\title{Extract the main content from HTML files}
\description{
  \tabular{ll}{ Package: \tab boilerpipeR\cr Type: \tab
  Package\cr Version: \tab 1.0\cr Date: \tab 2012-12-13\cr
  License: \tab Apache License (== 2.0)\cr LazyLoad: \tab
  yes\cr }
}
\details{
  \pkg{boilerpipeR} interfaces the boilerpipe Java library,
  created by Christian Kohlschutter
  \url{http://code.google.com/p/boilerpipe/}. It implements
  robust heuristics to extract the main content from HTML
  files, removing unessecary elements like ads, banners and
  headers/footers.
}
\examples{
\dontrun{
data(content)
extract <- DefaultExtractor(content)
cat(extract)
}
}
\author{
  Mario Annau \email{mario.annau@gmail}
}
\seealso{
  \code{\link{Extractor}} \code{\link{DefaultExtractor}}
  \code{\link{ArticleExtractor}}
}
\keyword{package}

